(* Run the various tests. *)

open Core

let () =
  let present = Sccs.sccs_check () in
  printf "Sccs present: %b\n" present;
  if present then Sccs.run_test ()
