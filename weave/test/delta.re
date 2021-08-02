/* Test deltas using a library. */

open Core;
module RS = Random.State;

let nums_size = 1_000;
let num_versions = 100;

module type STORE = {
  /** The type representing whatever data needs to be kept to track
   * this store. */

  type t;

  /** Construct a new store, in the given directory. */

  let make: string => t;

  /** Write the first version, often different. */

  let add_initial: (t, ~f: Weave.Stream.writer => unit) => int;

  /** Add a new delta to the store.  The function should write lines
   * of data to the given stream writer.  The whole function should
   * return an int to represent the stored delta. */

  let add_delta: (t, ~f: Weave.Stream.writer => unit) => int;

  /** Read a delta previously written.  It is a push parser, so user
   * state is pushed through.
   * TODO: It'd be nice to make this much more general, instead of it
   * just collecting a string list, allow it to collect anything, but
   * the type embedded into signatures to be easy to make dynamic, so
   * we just make this specific.  It will probably better to do all of
   * this differently.
   * Instead, for now, this just reads all of the lines in. */

  let read_delta: (t, ~delta: int) => list(string);
};

module Tester = (Store: STORE) => {
  /** A tester. */

  type t = {
    tdir: string,
    store: Store.t,
    mutable nums: list(array(int)),
    mutable deltas: list(int),
    rng: RS.t,
  };

  let make = () => {
    let tdir = Filename.temp_dir(~in_dir="/var/tmp", "weave", "");
    let store = Store.make(tdir);
    let nums = [Array.init(nums_size, ~f=x => x + 1)];
    let rng = RS.make([|1, 2, 3|]);
    {tdir, store, nums, rng, deltas: []};
  };

  let with_tester = (~f) => {
    let s = make();
    let ex =
      try(Ok(f(s))) {
      | ex => Error(ex)
      };
    if (Option.is_none(Sys.getenv("WEAVE_KEEP"))) {
      FileUtil.rm(~recurse=true, [s.tdir]);
    };
    switch (ex) {
    | Ok(result) => result
    | Error(ex) => raise(ex)
    };
  };

  /** A simple pseudorandom shuffle of the numbers. */

  let shuffle = s => {
    let top = Array.copy(List.hd_exn(s.nums));
    let len = Array.length(top);
    let a = RS.int(s.rng, len);
    let b = RS.int(s.rng, len);
    let (a, b) =
      if (a > b) {
        (b, a);
      } else {
        (a, b);
      };
    let rec loop = (a, b) =>
      if (a < b) {
        Array.swap(top, a, b);
        loop(a + 1, b - 1);
      };

    loop(a, b);
    s.nums = [top, ...s.nums];
  };

  let validate_one = (s, delta, expected) => {
    let got = Store.read_delta(s, ~delta);
    let got = List.map(got, ~f=Int.of_string);
    let got = Array.of_list(List.rev(got));
    if (!Array.equal(Int.equal, got, expected)) {
      printf(
        "exp: %s\n",
        Sexp.to_string @@ Array.sexp_of_t(Int.sexp_of_t, expected),
      );
      printf(
        "got: %s\n",
        Sexp.to_string @@ Array.sexp_of_t(Int.sexp_of_t, got),
      );
    };
    assert(Array.equal(Int.equal, got, expected));
  };

  let validate = s =>
    List.iter2_exn(
      List.rev(s.deltas),
      List.rev(s.nums),
      ~f=validate_one(s.store),
    );

  let write_data = (s, wr) => {
    let nums = List.hd_exn(s.nums);
    let nums = Array.to_list(nums);
    let nums = List.map(nums, ~f=Int.to_string);
    wr#write_lines(nums);
  };

  let run = () =>
    with_tester(~f=s => {
      printf("tdir: %S\n", s.tdir);
      let delta = Store.add_initial(s.store, ~f=write_data(s));
      s.deltas = [delta, ...s.deltas];

      for (_ in 2 to num_versions) {
        shuffle(s);
        let delta = Store.add_delta(s.store, ~f=write_data(s));
        s.deltas = [delta, ...s.deltas];
      };

      validate(s);
    });
};
