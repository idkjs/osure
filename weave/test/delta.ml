(* Test deltas using a library. *)

open Core
module RS = Random.State

let nums_size = 1_000
let num_versions = 100

module type STORE = sig
  (** The type representing whatever data needs to be kept to track
   * this store. *)
  type t

  (** Construct a new store, in the given directory. *)
  val make : string -> t

  (** Write the first version, often different. *)
  val add_initial : t -> f:(Weave.Stream.writer -> unit) -> int

  (** Add a new delta to the store.  The function should write lines
   * of data to the given stream writer.  The whole function should
   * return an int to represent the stored delta. *)
  val add_delta : t -> f:(Weave.Stream.writer -> unit) -> int

  (** Read a delta previously written.  It is a push parser, so user
   * state is pushed through.
   * TODO: It'd be nice to make this much more general, instead of it
   * just collecting a string list, allow it to collect anything, but
   * the type embedded into signatures to be easy to make dynamic, so
   * we just make this specific.  It will probably better to do all of
   * this differently.
   * Instead, for now, this just reads all of the lines in. *)
  val read_delta : t -> delta:int -> string list
end

module Tester (Store : STORE) = struct
  (** A tester. *)
  type t = {
    tdir : string;
    store : Store.t;
    mutable nums : int array list;
    mutable deltas : int list;
    rng : RS.t
  }

  let make () =
    let tdir = Filename.temp_dir ~in_dir:"/var/tmp" "weave" "" in
    let store = Store.make tdir in
    let nums = [Array.init nums_size ~f:(fun x -> x + 1)] in
    let rng = RS.make [|1;2;3|] in
    { tdir; store; nums; rng; deltas = [] }

  let with_tester ~f =
    let s = make () in
    let ex = try Ok (f s) with
      | ex -> Error ex in
    if Option.is_none (Sys.getenv "WEAVE_KEEP") then
      FileUtil.rm ~recurse:true [s.tdir];
    match ex with
      | Ok result -> result
      | Error ex -> raise ex

  (** A simple pseudorandom shuffle of the numbers. *)
  let shuffle s =
    let top = Array.copy (List.hd_exn s.nums) in
    let len = Array.length top in
    let a = RS.int s.rng len in
    let b = RS.int s.rng len in
    let a, b = if a > b then b, a else a, b in
    let rec loop a b =
      if a < b then begin
        Array.swap top a b;
        loop (a+1) (b-1)
      end
    in
    loop a b;
    s.nums <- top :: s.nums

  let validate_one s delta expected =
    let got = Store.read_delta s ~delta in
    let got = List.map got ~f:Int.of_string in
    let got = Array.of_list (List.rev got) in
    if not (Array.equal Int.equal got expected) then begin
      printf "exp: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t expected);
      printf "got: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t got)
    end;
    assert (Array.equal Int.equal got expected)

  let validate s =
    List.iter2_exn (List.rev s.deltas) (List.rev s.nums) ~f:(validate_one s.store)

  let write_data s wr =
    let nums = List.hd_exn s.nums in
    let nums = Array.to_list nums in
    let nums = List.map nums ~f:Int.to_string in
    wr#write_lines nums

  let run () =
    with_tester ~f:(fun s ->
      printf "tdir: %S\n" s.tdir;
      let delta = Store.add_initial s.store ~f:(write_data s) in
      s.deltas <- delta :: s.deltas;

      for _ = 2 to num_versions do
        shuffle s;
        let delta = Store.add_delta s.store ~f:(write_data s) in
        s.deltas <- delta :: s.deltas
      done;

      validate s)

end
