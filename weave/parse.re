/* Weave file parser. */

open Core;

type line =
  | Plain(string)
  | Header(string)
  | Insert(int)
  | Delete(int)
  | End(int)
  | Done;
/* [@@deriving show] */

/* Extract the field information.  The lines are always of the form
 * "\x01C nnn" where C is a command, and nnn is a decimal integer.
 */
let field = line => {
  let line = String.subo(line, ~pos=3);
  Int.of_string(line);
};

/* Given a reader, decode it into info about it. */
let rec next_line = rd =>
  switch (rd#read_line) {
  | None => Done
  | Some(line) =>
    if (String.length(line) > 3 && Char.(line.[0] == '\001')) {
      switch (line.[1]) {
      | 'E' => End(field(line))
      | 'I' => Insert(field(line))
      | 'D' => Delete(field(line))
      | 't' => Header(String.subo(line, ~pos=2))
      | _ => next_line(rd)
      };
    } else if (String.length(line) > 1 && Char.(line.[0] == '\001')) {
      next_line(rd);
    } else {
      Plain(line);
    }
  };

type state_mode =
  | Keep
  | Skip
  | Next;
/* [@@deriving show, sexp] */

module State = {
  type t = {
    rd: Stream.reader,
    pending: option(string),
    delta: int,
    keeps: Map.t(int, state_mode, Int.comparator_witness),
    keep: bool,
    lineno: int,
  };
};

/*
 let show_state st =
   let buf = Buffer.create 64 in
   bprintf buf "[(%d)%b" st.lineno st.keep;
   Map.iteri st.keeps ~f:(fun ~key ~data ->
     bprintf buf "; %d->%s" key (show_state_mode data));
   bprintf buf "]";
   Buffer.contents buf
 */

let init = (rd, delta): State.t => {
  rd,
  delta,
  pending: None,
  keeps: Map.empty((module Int)),
  keep: false,
  lineno: 0,
};

/* Compute the value of keep from a given map.  Iterated as a lazy
 * sequence, because the iterator stops at the first element */
let get_keep = keeps => {
  let rec loop = seq =>
    switch (Sequence.next(seq)) {
    | None => false
    | Some(((_, Keep), _)) => true
    | Some(((_, Skip), _)) => false
    | Some((_, seq)) => loop(seq)
    };
  loop(Map.to_sequence(~order=`Decreasing_key, keeps));
};

/* Given an existing state, update that state based on a particular
 * line.  Passed in is the old state, the desired delta (needed to
 * keep the state meaningful), and it returns the new state. */
let update = (ostate: State.t, line) => {
  let next = nkeeps => {...ostate, keeps: nkeeps, keep: get_keep(nkeeps)};
  switch (line) {
  | Insert(lev) =>
    next @@
    Map.add_exn(
      ostate.keeps,
      ~key=lev,
      ~data=
        if (ostate.delta >= lev) {
          Keep;
        } else {
          Skip;
        },
    )
  | Delete(lev) =>
    next @@
    Map.add_exn(
      ostate.keeps,
      ~key=lev,
      ~data=
        if (ostate.delta >= lev) {
          Skip;
        } else {
          Next;
        },
    )
  | End(lev) => next @@ Map.remove(ostate.keeps, lev)
  | _ => ostate
  };
};

/* These operations are available to the push parser (Pusher) that can
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

/* The Empty_Sink module discards everything. */
module Empty_Sink: Sink = {
  type t = unit;
  let insert = ((), _) => ();
  let delete = ((), _) => ();
  let ending = ((), _) => ();
  let plain = ((), _, _) => ();
};

module Pusher = (S: Sink) => {
  type state = State.t;

  let make = (rd, ~delta) => init(rd, delta);

  let push_to = (~stop=0, st: State.t, ustate) => {
    let (st, ustate) =
      switch (st.pending) {
      | None => (st, ustate)
      | Some(text) => (
          {...st, pending: None},
          S.plain(ustate, text, st.keep),
        )
      };
    let _ = stop;
    let rec loop = (st: State.t, ust) => {
      let line = next_line(st.rd);
      let st' = update(st, line);
      switch (line) {
      | Done => (0, st', ust)
      | Insert(delta) =>
        let ust' = S.insert(ust, delta);
        loop(st', ust');
      | Delete(delta) =>
        let ust' = S.delete(ust, delta);
        loop(st', ust');
      | End(delta) =>
        let ust' = S.ending(ust, delta);
        loop(st', ust');
      | Plain(text) =>
        let st' =
          if (st'.keep) {
            {...st', lineno: st'.lineno + 1};
          } else {
            st';
          };
        if (st'.keep && st'.lineno == stop) {
          let st' = {...st', pending: Some(text)};
          (stop, st', ust);
        } else {
          let ust' = S.plain(ust, text, st'.keep);
          loop(st', ust');
        };
      | Header(_) =>
        /* TODO, should there be a call for this? */
        loop(st', ust)
      };
    };
    loop(st, ustate);
  };

  let run = (rd, ~delta, ~ustate) => {
    let st0 = make(rd, ~delta);
    let (_, _, ustate') = push_to(st0, ustate);
    ustate';
  };
};

module Puller = {
  type state = State.t;

  let make = (rd, ~delta) => init(rd, delta);

  /* A pull parser that only pulls the plain text lines of a given
   * revision. */
  let pull_plain = stref => {
    let rec loop = (st: State.t) => {
      let line = next_line(st.rd);
      let st = update(st, line);
      switch (line) {
      | Done => None
      | Plain(text) =>
        if (st.keep) {
          let st = {...st, lineno: succ(st.lineno)};
          stref := st;
          Some(text);
        } else {
          loop(st);
        }
      | _ => loop(st)
      };
    };

    loop(stref^);
  };
};

let sample = () => {
  let sn =
    Naming.simple_naming(
      ~path="/",
      ~base="2sure",
      ~ext="dat",
      ~compress=true,
    );
  let rd = Naming.main_reader(sn);
  let rec loop = () => {
    let thing = next_line(rd);
    /* printf "%s\n" (show_line thing); */
    switch (thing) {
    | Done => ()
    | _ => loop()
    };
  };

  loop();
};

module Numeric_Sink: Sink with type t = list(int) = {
  type t = list(int);
  /* TODO, make this work so the empty sink can just be included. */
  let insert = (x, _) => x;
  let delete = (x, _) => x;
  let ending = (x, _) => x;
  let plain = (nums, text, keep) =>
    if (keep) {
      let num = Int.of_string(text);
      [num, ...nums];
    } else {
      nums;
    };
};

module Numeric_Pusher = Pusher(Numeric_Sink);

/* A special case, that reads just the header from the file.  It
 * expects the header to be the first thing read from the file. */
let read_header = rd =>
  switch (next_line(rd)) {
  | Header(text) => text
  | _ => failwith("Invalid header line read")
  };

/* Test entrance. */
let test_check = (path, delta, expected) => {
  /* let delta = 1 in */
  let base = Filename.chop_suffix(Filename.basename(path), ".dat");
  let path = Filename.dirname(path);
  let sn = Naming.simple_naming(~path, ~base, ~ext="dat", ~compress=false);
  Naming.with_main_reader(
    sn,
    ~f=rd => {
      let nums =
        Array.of_list(List.rev(Numeric_Pusher.run(rd, ~delta, ~ustate=[])));
      /* printf "exp: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t expected); */
      /* printf "got: %s\n" (Sexp.to_string @@ Array.sexp_of_t Int.sexp_of_t nums); */
      assert(Array.equal(Int.equal, nums, expected));
    },
  );

  /* Run, stopping at 50, and make sure it still gets everything. */
  Naming.with_main_reader(
    sn,
    ~f=rd => {
      let st = Numeric_Pusher.make(rd, ~delta);
      let (stop, st, ust) = Numeric_Pusher.push_to(~stop=50, st, []);
      assert(stop == 50);
      assert(List.length(ust) == 49); /* before the stop */
      let (stop, _, ust) = Numeric_Pusher.push_to(st, ust);
      assert(stop == 0);
      let nums = List.to_array(ust);
      Array.rev_inplace(nums);
      assert(Array.equal(Int.equal, nums, expected));
    },
  );
};

/*
 pusher rd ~delta
 printf "path: %S\n" path;
 let rec loop i =
   let thing = next_line rd in
   match thing with
     | Done -> i
     | line ->
         printf "line: %s\n" (show_line line);
         loop (i + 1) in
 let lines = loop 0 in
 printf "  lines: %d\n" lines
 */
