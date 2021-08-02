/* Run the various tests. */

open Core;

let () =
  if (true) {
    let present = Sccs.sccs_check();
    printf("Sccs present: %b\n", present);
    if (present) {
      Sccs.run_test();
    };
  };

let () =
  if (true) {
    printf("Delta test\n");
    Parse.run_test();
  };
