(* Weave file parser. *)

open Core

type line =
  | Plain of string
  | Header of string
  | Insert of int
  | Delete of int
  | End of int
  | Done
(* [@@deriving show] *)

(* Extract the field information.  The lines are always of the form
 * "\x01C nnn" where C is a command, and nnn is a decimal integer.
 *)
let field line =
  let line = String.subo line ~pos:3 in
  Int.of_string line

(* Given a reader, decode it into info about it. *)
let rec next_line rd =
  match rd#read_line with
    | None -> Done
    | Some line ->
        if String.length line > 3 && Char.(=) line.[0] '\x01' then begin
          match line.[1] with
            | 'E' -> End (field line)
            | 'I' -> Insert (field line)
            | 'D' -> Delete (field line)
            | 't' -> Header (String.subo line ~pos:2)
            | _ -> next_line rd
        end else if String.length line > 1 && Char.(=) line.[0] '\x01' then
          next_line rd
        else
          Plain line

type state_mode = Keep | Skip | Next
(* [@@deriving show, sexp] *)

type state = {
  keeps : (int, state_mode, Int.comparator_witness) Map.t;
  keep : bool }

(*
let show_state st =
  let buf = Buffer.create 64 in
  bprintf buf "[%b" st.keep;
  Map.iteri st.keeps ~f:(fun ~key ~data ->
    bprintf buf "; %d->%s" key (show_state_mode data));
  bprintf buf "]";
  Buffer.contents buf
*)

let init () = { keeps = Map.empty (module Int); keep = false }

(*
type state = {
  keep : (int, state_mode, Int.comparator_witness) Map.t;
  line : int
}
(* [@@deriving sexp] *)

let init () = { keep = Map.empty (module Int); line = 0 }
*)

(* Compute the value of keep from a given map.  Iterated as a lazy
 * sequence, because the iterator stops at the first element *)
let get_keep keeps =
  let rec loop seq =
    match Sequence.next seq with
      | None -> false
      | Some ((_, Keep), _) -> true
      | Some ((_, Skip), _) -> false
      | Some (_, seq) -> loop seq in
  loop (Map.to_sequence ~order:`Decreasing_key keeps)

(* Given an existing state, update that state based on a particular
 * line.  Passed in is the old state, the desired delta (needed to
 * keep the state meaningful), and it returns the new state. *)
let update ostate desired line =
  let next nkeeps = { keeps = nkeeps; keep = get_keep nkeeps } in
  match line with
    | Insert lev ->
        next @@ Map.add_exn ostate.keeps ~key:lev ~data:(if desired >= lev then Keep else Skip)
    | Delete lev ->
        next @@ Map.add_exn ostate.keeps ~key:lev ~data:(if desired >= lev then Skip else Next)
    | End lev ->
        next @@ Map.remove ostate.keeps lev
    | _ -> ostate

(* These operations are available to the push parser (Pusher) that can
 * be invoked for each record encountered. *)
module type Sink = sig
  type t
  (** Type of value passed through *)
  val insert : t -> int -> t
  (** Hit an insert record. *)
  val delete : t -> int -> t
  (** Hit a delete record. *)
  val ending : t -> int -> t
  (** Hit an end record. *)
  val plain : t -> string -> bool -> t
  (** Hit a plain text record.  The bool indicates if this is included
   * in the desired delta. *)
end

(* The Empty_Sink module discards everything. *)
module Empty_Sink : Sink = struct
  type t = unit
  let insert () _ = ()
  let delete () _ = ()
  let ending () _ = ()
  let plain () _ _ = ()
end

module Pusher (S : Sink) = struct
  let pusher rd ~delta ~s0 =
    let rec loop st ust =
      let line = next_line rd in
      let st' = update st delta line in
      match line with
        | Done -> ust
        | Insert delta ->
            let ust' = S.insert ust delta in
            loop st' ust'
        | Delete delta ->
            let ust' = S.delete ust delta in
            loop st' ust'
        | End delta ->
            let ust' = S.ending ust delta in
            loop st' ust'
        | Plain text ->
            let ust' = S.plain ust text st'.keep in
            loop st' ust'
        | Header _ ->
            (* TODO, should there be a call for this? *)
            loop st' ust in
    loop (init ()) s0
end

(* Run a push parser over the input. *)
(*
let pusher rd ~delta =
  let rec loop st =
    let line = next_line rd in
    let st2 = update st delta line in
    printf "line: %s  state: %s\n" (show_line line) (show_state st2);
    match line with
      | Done -> ()
      | _ -> loop st2
  in loop (init ())
*)

let sample () =
  let sn = Naming.simple_naming ~path:"/" ~base:"2sure" ~ext:"dat" ~compress:true in
  let rd = Naming.main_reader sn in
  let rec loop () =
    let thing = next_line rd in
    (* printf "%s\n" (show_line thing); *)
    match thing with
      | Done -> ()
      | _ -> loop ()
  in
  loop ()

module Numeric_Sink : (Sink with type t = int list) = struct
  type t = int list
  (* TODO, make this work so the empty sink can just be included. *)
  let insert x _ = x
  let delete x _ = x
  let ending x _ = x
  let plain nums text keep =
    if keep then begin
      let num = Int.of_string text in
      num :: nums
    end else nums
end

module Numeric_Pusher = Pusher (Numeric_Sink)

(* Test entrance. *)
let test_check path delta expected =
  (* let delta = 1 in *)
  let base = Filename.chop_suffix (Filename.basename path) ".dat" in
  let path = Filename.dirname path in
  let sn = Naming.simple_naming ~path ~base ~ext:"dat" ~compress:false in
  let rd = Naming.main_reader sn in

  (*
  let _ = expected in
  pusher rd ~delta
  *)

  let nums = Array.of_list (List.rev (Numeric_Pusher.pusher rd ~delta ~s0:[])) in
  (* printf "exp: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t expected); *)
  (* printf "got: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t nums); *)
  assert (Array.equal Int.equal nums expected)
  (*
  pusher rd ~delta
  printf "path: %S\n" path;
  let rec loop i =
    let thing = next_line rd in
    match thing with
      | Done -> i
      | line ->
          printf "line: %s\n" (show_line line);
          loop (i + 1) in
  let lines = loop 0 in
  printf "  lines: %d\n" lines
  *)
