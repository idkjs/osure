/** Weave parsing. */;

/** These operations are available to the push parser (Pusher) that can
 * be invoked for each record encountered. */

module type Sink = {
  /** Type of value passed through */

  type t;

  /** Type of value passed through */
  /** Hit an insert record. */

  let insert: (t, int) => t;

  /** Hit an insert record. */
  /** Hit a delete record. */

  let delete: (t, int) => t;

  /** Hit a delete record. */
  /** Hit an end record. */

  let ending: (t, int) => t;

  /** Hit an end record. */
  /** Hit a plain text record.  The bool indicates if this is included
   * in the desired delta. */

  let plain: (t, string, bool) => t;
};

module Pusher:
  (S: Sink) =>
   {
    /** Simple case, run the entire process. */

    let run: (Stream.reader, ~delta: int, ~ustate: S.t) => S.t;

    /** An internal state to allow the parser to be run in pieces. */

    type state;

    /** Construct the initial parser state, reading from a given reader,
   * intending to extract a particular delta. */

    let make: (Stream.reader, ~delta: int) => state;

    /** Run the parser (pushing) through.  The stop value indicates when
   * the parser should stop.  A value of 0 indicates it should run
   * through to the end.  A non-zero value indicates it should stop
   * when reaching the given line of input (before the line is
   * pushed).  The return result is the line stopped on, the new state
   * of the parser, and the updated value of the user state. */

    let push_to: (~stop: int=?, state, S.t) => (int, state, S.t);
  };

module Puller: {
  type state;

  let make: (Stream.reader, ~delta: int) => state;

  /** Run a single step of a pull parser, returning the next line of
   * input, or None when there is no more input. */

  let pull_plain: ref(state) => option(string);
};

let test_check: (string, int, array(int)) => unit;
let sample: unit => unit;

let read_header: Stream.reader => string;
