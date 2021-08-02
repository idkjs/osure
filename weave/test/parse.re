/* Parse testing. */

open Core;

module Line_sink: Weave.Parse.Sink with type t = list(string) = {
  type t = list(string);
  let insert = (x, _) => x;
  let delete = (x, _) => x;
  let ending = (x, _) => x;
  let plain = (lines, text, keep) =>
    if (keep) {
      [text, ...lines];
    } else {
      lines;
    };
};

module Line_pusher = Weave.Parse.Pusher(Line_sink);

/* Store that manages a weave */

module Delta_store = {
  type t = {
    ns: Weave.Naming.t,
    mutable index: int,
  };

  let make_tags = t => {
    let name = sprintf("delta #%d", t.index);
    t.index = t.index + 1;
    [("key", "value"), ("name", name)];
  };

  let make = path => {
    let ns =
      Weave.Naming.simple_naming(
        ~path,
        ~base="ptest",
        ~ext="dat",
        ~compress=true,
      );
    {ns, index: 1};
  };

  let add_initial = (t, ~f) => {
    let ((), delta) =
      Weave.Write.with_first_delta(t.ns, ~tags=make_tags(t), ~f);
    delta;
  };

  let add_delta = (t, ~f) => {
    let ((), delta) =
      Weave.Write.with_new_delta(t.ns, ~tags=make_tags(t), ~f);
    delta;
  };

  let read_delta = (t, ~delta) =>
    Weave.Naming.with_main_reader(t.ns, ~f=rd =>
      Line_pusher.run(rd, ~delta, ~ustate=[])
    );
};

module Delta_tester = Delta.Tester(Delta_store);

/* Another variant that is like the above, but uses the plaintext pull
 * parser. */
module Pull_store: Delta.STORE = {
  include Delta_store;

  let read_delta = (t, ~delta) =>
    Weave.Naming.with_main_reader(
      t.Delta_store.ns,
      ~f=rd => {
        let st = ref(Weave.Parse.Puller.make(rd, ~delta));
        let rec loop = lines =>
          switch (Weave.Parse.Puller.pull_plain(st)) {
          | None => lines
          | Some(line) => loop([line, ...lines])
          };
        loop([]);
      },
    );
};

module Pull_tester = Delta.Tester(Pull_store);

let run_test = () => {
  Delta_tester.run();
  Pull_tester.run();
};
