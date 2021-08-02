/* Weave writing */

let with_first_delta:
  (Naming.t, ~tags: Tags.t, ~f: Stream.writer => 'a) => ('a, int);
let with_new_delta:
  (Naming.t, ~tags: Tags.t, ~f: Stream.writer => 'a) => ('a, int);

/* val sample : unit -> unit */
