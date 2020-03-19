open Core
open Rsure

let _ =
  Sequence.iter (Walk.walk ".") ~f:(fun e ->
    print_endline @@ Node.show e)
