(* Generalized progress meter. *)

open Core

type meter = {
  (** The last message printed. *)
  mutable message : string;

  (** Generate a new message from current data. *)
  mutable update : unit -> string;

  (** When we expect to update the message. *)
  mutable next_update : Time.t;
}
type t = meter

let next_interval time = Time.add time (Time.Span.of_ms 250.0)

let clear meter =
  let count = 1 + String.count meter.message ~f:(fun ch -> Char.(ch = '\n')) in
  for _ = 1 to count do
    printf "\x1b[1A\x1b[2K"
  done;
  printf "%!"

let show_meter meter =
  let message = meter.update () in
  clear meter;
  meter.message <- message;
  printf "%s\n%!" message

(** Update the meter, possibly showing the message, if it is time. *)
let update meter ~f =
  meter.update <- f;
  let curtime = Time.now () in
  if Time.(curtime >= meter.next_update) then begin
    show_meter meter;
    meter.next_update <- next_interval curtime
  end

let with_meter ~f =
  let meter = { message = "";
    update = (fun () -> "");
    next_update = next_interval (Time.now ()) } in
  let result = f meter in
  show_meter meter;
  result

let units = ["B  "; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]

let humanize_size size =
  let size = Int64.to_float size in
  let precision value =
    if Float.(value < 10.0) then 3
    else if Float.(value < 100.0) then 2
    else 1 in
  let rec loop size = function
    | (_::uus) when Float.(size > 1024.0) -> loop (size /. 1024.0) uus
    | (uu::_) -> sprintf "%.*f%s" (precision size) size uu
    | _ -> failwith "Absurd size overflow"
  in loop size units

module DirState = struct
  type t = {
    dirs : int64;
    files : int64;
    total_bytes : int64;
  }
  let make () = { dirs = 0L; files = 0L; total_bytes = 0L }
  let show meter { dirs; files; total_bytes } =
    update meter ~f:(fun () ->
      sprintf "scan: %Ld dirs %Ld files, %s bytes" dirs files
        (humanize_size total_bytes))
end

let att_size atts = match Map.find atts "size" with
  | None -> 0L
  | Some text -> Int64.of_string text

let scan_meter meter seq =
  let state0 = DirState.make () in
  let last_state = ref state0 in
  Sequence.folding_map seq ~init:state0 ~f:(fun state node ->
    let state = match node with
      | Node.Enter _ ->
          { state with dirs = Int64.(state.dirs + 1L) }
      | Node.File (_, atts) ->
          { state with files = Int64.(state.files + 1L);
            total_bytes = Int64.(state.total_bytes + att_size atts) }
      | Node.Sep -> state
      | Node.Leave ->
          DirState.show meter state;
          state
    in
    last_state := state;
    (state, node))
