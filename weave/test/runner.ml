(* Run the various tests. *)

open Core

let () =
  if true then begin
    let present = Sccs.sccs_check () in
    printf "Sccs present: %b\n" present;
    if present then Sccs.run_test ()
  end

let () =
  if true then begin
    printf "Delta test\n";
    Parse.run_test ()
  end
