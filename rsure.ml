open Core
open Rsure

let nothing () =
  Sequence.iter (Walk.walk ".") ~f:(fun e ->
    print_endline @@ Node.show e)

let bench_stream () = Weave.Stream.bulk_io ()

(* Read a large file to get an idea of timing. *)
let bench_gzip_in () =
  let fd = Weave.Stream.gzip_in "/home/2sure.dat.gz" in
  let rec loop count =
    match fd#read_line with
      | Some _ -> loop (count + 1)
      | None -> count in
  let total = loop 0 in
  fd#close;
  printf "%d lines\n" total

let naming _ =
  Weave.Naming.trial ()

let sample () =
  Weave.Parse.sample ()

module Load_sink : (Weave.Parse.Sink with type t = string list) = struct
  type t = string list
  let insert xs _ = xs
  let delete xs _ = xs
  let ending xs _ = xs
  let plain xs text keep = if keep then text :: xs else xs
end

module Count_sink : (Weave.Parse.Sink with type t = int) = struct
  type t = int
  let insert xs _ = xs
  let delete xs _ = xs
  let ending xs _ = xs
  let plain xs _ keep = if keep then xs + 1 else xs
end

module Loader = Weave.Parse.Pusher (Load_sink)
module Counter = Weave.Parse.Pusher (Count_sink)

(* TODO: Use atdgen to build real structures out of the json data.
 * For now, just use this awful parser. *)
(*
let assoc_get json key =
  let rec lookup = function
    | (k,v)::xs ->
        if String.(=) k key then v
        else lookup xs
    | [] -> failwith "Key not present" in
  match json with
    | `Assoc xs -> lookup xs
    | _ -> failwith "Unexpected json"

module Version = struct
  type t = {
    name : string;
    number : int;
    (*tags : string list;
    time : string*)
  }

  let from_json json =
    let name = match assoc_get json "name" with
      | `String text -> text
      | _ -> failwith "Unexpected json" in
    let number = match assoc_get json "number" with
      | `Int num -> num
      | _ -> failwith "Unexpected json" in
    { name; number }
end
*)

(* Try reading in my largest sure file for some benchmarking ideas. *)
let benchy () =
  let sn = Weave.Naming.simple_naming ~path:"/home" ~base:"2sure" ~ext:"dat" ~compress:true in
  let header = Weave.Naming.with_main_reader sn ~f:Weave.Parse.read_header in
  let header = Weave.Header_j.header_of_string header in
  (* printf "%S\n" (Weave.Header_j.string_of_header header) *)
  let deltas = List.take header.deltas 4 in
  List.iter deltas ~f:(fun ver ->
    printf "Version %d %S\n" ver.number ver.name;
    Out_channel.flush stdout;
    (* *)
    let lines = Weave.Naming.with_main_reader sn ~f:(fun rd ->
      Loader.run rd ~delta:ver.number ~ustate:[]) in
    printf "   There are %d lines\n" (List.length lines);
    (* *)
    (*
    let lines = Weave.Naming.with_main_reader sn ~f:(fun rd ->
      Counter.run rd ~delta:ver.number ~ustate:0) in
    printf "   There are %d lines\n" lines;
    Out_channel.flush stdout
    *)
  )

(*
let _ =
  Weave.Write.sample ()
*)
