/* Parse testing. */

module Line_pusher: {
  let run:
    (Weave.Stream.reader, ~delta: int, ~ustate: list(string)) => list(string);
};

let run_test: unit => unit;
