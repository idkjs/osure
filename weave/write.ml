(* Weave file writer. *)

open Core

module Write_sink : (Parse.Sink with type t = Stream.writer) = struct
  type t = Stream.writer
  let insert x _ = x
  let delete x _ = x
  let ending x _ = x
  let plain wr text keep =
    if keep then wr#write_lines [text];
    wr
end

module Write_pusher = Parse.Pusher (Write_sink)

class delta_writer (wr : Stream.writer) =
  object
    method insert delta = wr#write_lines [sprintf "\x01I %d" delta]
    method delete delta = wr#write_lines [sprintf "\x01D %d" delta]
    method ending delta = wr#write_lines [sprintf "\x01E %d" delta]
    method plain text = wr#write_lines [text]
  end

module Delta_sink : (Parse.Sink with type t = delta_writer) = struct
  type t = delta_writer
  let insert wr delta = wr#insert delta; wr
  let delete wr delta = wr#delete delta; wr
  let ending wr delta = wr#ending delta; wr
  let plain wr text _keep = wr#plain text; wr
end

module Delta_pusher = Parse.Pusher (Delta_sink)

let latest_delta (deltas : Header_t.version list) =
  match List.last deltas with
    | Some { number; _ } -> number
    | None -> 0

(* Get the header, structurally. *)
let read_header ns =
  let header = Naming.with_main_reader ns ~f:Parse.read_header in
  Header_j.header_of_string header

let write_header wr header =
  let htext = Header_j.string_of_header header in
  wr#write_lines [sprintf "\x01t%s" htext]

(* Return the current time, formatted as an iso8601 basic time. *)
let now () =
  Time.to_string_iso8601_basic (Time.now ()) ~zone:(force Time.Zone.local)

(* We expect the tags to always contain a "name" value.  Extract the
 * 'name' value from the tags, and return the rest of the tags,
 * already converted to json.  If there is no "name" field, just use a
 * timestamp. *)
let get_name tags =
  let rec loop pre tags = match tags with
    | [] ->
        let name = now () in
        (name, Tags.to_json (List.rev pre))
    | (("name", name)::xs) ->
        (name, Tags.to_json (List.rev_append pre xs))
    | (x::xs) ->
        loop (x :: pre) xs in
  loop [] tags

(** Build a new version from a non-existent weave file. *)
(* The entire contents is written as a single insert block, for delta
 * 1, which we return. *)
let with_first_delta ns ~tags ~f =
  let name, tags = get_name tags in
  let deltas : Header_t.version list = [
    { name; number = 1; tags; time = now () } ] in
  let header : Header_t.header = { version = 1; deltas = deltas } in
  let (tname, result) = Naming.with_new_temp ns ~compressed:true ~f:(fun wr ->
    write_header wr header;
    let dwr = new delta_writer wr in
    dwr#insert 1;
    let result = f wr in
    dwr#ending 1;
    result) in
  let mname = ns#main_file in
  printf "Rename %S to %S\n" tname mname;
  Unix.rename ~src:tname ~dst:mname;
  (result, 1)

let diff_re = Re2.of_string {|^(\d+)(,(\d+))?([acd])|}

module Delta_state = struct
  type t = {
    is_adding : bool;
    is_done : bool;
    new_delta : int;
    wr : delta_writer;
    dpush : Delta_pusher.state;
  }

  let make wr dpush new_delta = { is_adding = false; is_done = false; new_delta; wr; dpush }

  (* TODO: Don't pass 'wr' into these. *)
  let diffy state =
    if state.is_adding then begin
      state.wr#ending state.new_delta;
      { state with is_adding = false }
    end else state

  (* Process a diff change line. *)
  let change state low high ch =
    let state = begin match ch with
      | "c" | "d" ->
          let (n, dpush, _) = Delta_pusher.push_to ~stop:low state.dpush state.wr in
          assert (n = low || n = 0);
          let state = { state with dpush; is_done = n = 0 } in
          state.wr#delete state.new_delta;
          let (n, dpush, _) = Delta_pusher.push_to ~stop:(high+1) state.dpush state.wr in
          assert (n = high + 1 || n = 0);
          let state = { state with dpush; is_done = n = 0 } in
          state.wr#ending state.new_delta;
          state
      | _ ->
          let (n, dpush, _) = Delta_pusher.push_to ~stop:(high+1) state.dpush state.wr in
          assert (n = high + 1 || n = 0);
          { state with dpush; is_done = n = 0 }
    end in
    match ch with
      | "c" | "a" ->
          state.wr#insert state.new_delta;
          { state with is_adding = true }
      | _ -> state

  let finish state =
    if state.is_adding then
      state.wr#ending state.new_delta;
    if not state.is_done then begin
      let (n, _, _) = Delta_pusher.push_to ~stop:0 state.dpush state.wr in
      assert (n = 0)
    end
end
module DS = Delta_state

(* Add a new version to the delta. *)
let with_new_delta ns ~tags ~f =
  let name, tags = get_name tags in

  (* Call the user-code to write the contents to a temporary file. *)
  let (tname, result) = Naming.with_new_temp ns ~compressed:false ~f:(fun wr ->
    f wr) in
  (* printf "Tmp: %S\n" tname; *)

  let header = read_header ns in
  let last = latest_delta (header.deltas) in

  (* Extract the last delta to another temp file. *)
  let (oldtname, _) =
    Naming.with_main_reader ns ~f:(fun rd ->
      Naming.with_new_temp ns ~compressed:false ~f:(fun wr ->
        Write_pusher.run rd ~delta:last ~ustate:wr)) in
  (* printf "Old: %S\n" oldtname; *)

  let (toutname, _) = Naming.with_new_temp ns ~compressed:true ~f:(fun wr ->
    write_header wr { header with deltas =
      header.deltas @ [{ name; number = (last + 1); time = now (); tags }] };
    let delwr = new delta_writer wr in
    Naming.with_main_reader ns ~f:(fun main_rd ->
      let dpush = Delta_pusher.make main_rd ~delta:last in
      let state = Shell.run_fold "diff" [oldtname; tname] ~expect:[0; 1]
        ~init:(DS.make delwr dpush (last + 1)) ~f:(fun state line ->
          begin match Re2.find_submatches diff_re line with
            | Error _ ->
                (* printf "diff: %S\n" line; *)
                if String.length line >= 2 && Char.(=) line.[0] '>' then
                  delwr#plain (String.subo line ~pos:2);
                (state, `Continue)
            | Ok pats ->
                let state = DS.diffy state in
                let low = Int.of_string (Option.value_exn pats.(1)) in
                let high = Option.value_map pats.(3) ~default:low ~f:(Int.of_string) in
                let cmd = Option.value_exn pats.(4) in
                (* printf "%d,%d %S\n" low high cmd; *)
                (DS.change state low high cmd, `Continue)
          end) in
      DS.finish state))
  in
  (* printf "toutname: %S\n" toutname; *)
  let mname = ns#main_file in
  let backname = ns#backup_file in
  Unix.rename ~src:mname ~dst:backname;
  Unix.rename ~src:toutname ~dst:mname;
  Unix.remove tname;
  Unix.remove oldtname;

  (result, last + 1)
