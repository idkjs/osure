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
