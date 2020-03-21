open Core
open Rsure

let nothing () =
  Sequence.iter (Walk.walk ".") ~f:(fun e ->
    print_endline @@ Node.show e)

let _ = Weave.Stream.bulk_io ()
