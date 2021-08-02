/* Weave file writer. */

open Core;

module Write_sink: Parse.Sink with type t = Stream.writer = {
  type t = Stream.writer;
  let insert = (x, _) => x;
  let delete = (x, _) => x;
  let ending = (x, _) => x;
  let plain = (wr, text, keep) => {
    if (keep) {
      wr#write_lines([text]);
    };
    wr;
  };
};

module Write_pusher = Parse.Pusher(Write_sink);

class delta_writer (wr: Stream.writer) = {
  as _;
  pub insert = delta => wr#write_lines([sprintf("\001I %d", delta)]);
  pub delete = delta => wr#write_lines([sprintf("\001D %d", delta)]);
  pub ending = delta => wr#write_lines([sprintf("\001E %d", delta)]);
  pub plain = text => wr#write_lines([text]);
};

module Delta_sink: Parse.Sink with type t = delta_writer = {
  type t = delta_writer;
  let insert = (wr, delta) => {
    wr#insert(delta);
    wr;
  };
  let delete = (wr, delta) => {
    wr#delete(delta);
    wr;
  };
  let ending = (wr, delta) => {
    wr#ending(delta);
    wr;
  };
  let plain = (wr, text, _keep) => {
    wr#plain(text);
    wr;
  };
};

module Delta_pusher = Parse.Pusher(Delta_sink);

let latest_delta = (deltas: list(Header_t.version)) =>
  switch (List.last(deltas)) {
  | Some({number, _}) => number
  | None => 0
  };

/* Get the header, structurally. */
let read_header = ns => {
  let header = Naming.with_main_reader(ns, ~f=Parse.read_header);
  Header_j.header_of_string(header);
};

let write_header = (wr, header) => {
  let htext = Header_j.string_of_header(header);
  wr#write_lines([sprintf("\001t%s", htext)]);
};

/* Return the current time, formatted as an iso8601 basic time. */
let now = () =>
  Time.to_string_iso8601_basic(Time.now(), ~zone=force(Time.Zone.local));

/* We expect the tags to always contain a "name" value.  Extract the
 * 'name' value from the tags, and return the rest of the tags,
 * already converted to json.  If there is no "name" field, just use a
 * timestamp. */
let get_name = tags => {
  let rec loop = (pre, tags) =>
    switch (tags) {
    | [] =>
      let name = now();
      (name, Tags.to_json(List.rev(pre)));
    | [("name", name), ...xs] => (
        name,
        Tags.to_json(List.rev_append(pre, xs)),
      )
    | [x, ...xs] => loop([x, ...pre], xs)
    };
  loop([], tags);
};

/* The entire contents is written as a single insert block, for delta
 * 1, which we return. */
/** Build a new version from a non-existent weave file. */

let with_first_delta = (ns, ~tags, ~f) => {
  let (name, tags) = get_name(tags);
  let deltas: list(Header_t.version) = (
    [{name, number: 1, tags, time: now()}]: list(Header_t.version)
  );
  let header: Header_t.header = ({version: 1, deltas}: Header_t.header);
  let (tname, result) =
    Naming.with_new_temp(
      ns,
      ~compressed=true,
      ~f=
        wr => {
          write_header(wr, header);
          let dwr = (new delta_writer)(wr);
          dwr#insert(1);
          let result = f(wr);
          dwr#ending(1);
          result;
        },
      (),
    );
  let mname = ns#main_file;
  printf("Rename %S to %S\n", tname, mname);
  Unix.rename(~src=tname, ~dst=mname);
  (result, 1);
};

let diff_re = Re.compile(Re.Pcre.re({|^(\d+)(,(\d+))?([acd])|}));

module Delta_state = {
  type t = {
    is_adding: bool,
    is_done: bool,
    new_delta: int,
    wr: delta_writer,
    dpush: Delta_pusher.state,
  };

  let make = (wr, dpush, new_delta) => {
    is_adding: false,
    is_done: false,
    new_delta,
    wr,
    dpush,
  };

  /* TODO: Don't pass 'wr' into these. */
  let diffy = state =>
    if (state.is_adding) {
      (state.wr)#ending(state.new_delta);
      {...state, is_adding: false};
    } else {
      state;
    };

  /* Process a diff change line. */
  let change = (state, low, high, ch) => {
    let state =
      switch (ch) {
      | "c"
      | "d" =>
        let (n, dpush, _) =
          Delta_pusher.push_to(~stop=low, state.dpush, state.wr);
        assert(n == low || n == 0);
        let state = {...state, dpush, is_done: n == 0};
        (state.wr)#delete(state.new_delta);
        let (n, dpush, _) =
          Delta_pusher.push_to(~stop=high + 1, state.dpush, state.wr);
        assert(n == high + 1 || n == 0);
        let state = {...state, dpush, is_done: n == 0};
        (state.wr)#ending(state.new_delta);
        state;
      | _ =>
        let (n, dpush, _) =
          Delta_pusher.push_to(~stop=high + 1, state.dpush, state.wr);
        assert(n == high + 1 || n == 0);
        {...state, dpush, is_done: n == 0};
      };
    switch (ch) {
    | "c"
    | "a" =>
      (state.wr)#insert(state.new_delta);
      {...state, is_adding: true};
    | _ => state
    };
  };

  let finish = state => {
    if (state.is_adding) {
      (state.wr)#ending(state.new_delta);
    };
    if (!state.is_done) {
      let (n, _, _) = Delta_pusher.push_to(~stop=0, state.dpush, state.wr);
      assert(n == 0);
    };
  };
};
module DS = Delta_state;

/* Add a new version to the delta. */
let with_new_delta = (ns, ~tags, ~f) => {
  let (name, tags) = get_name(tags);

  /* Call the user-code to write the contents to a temporary file. */
  let (tname, result) =
    Naming.with_new_temp(ns, ~compressed=false, ~f=wr => f(wr), ());
  /* printf "Tmp: %S\n" tname; */

  let header = read_header(ns);
  let last = latest_delta(header.deltas);

  /* Extract the last delta to another temp file. */
  let (oldtname, _) =
    Naming.with_main_reader(ns, ~f=rd =>
      Naming.with_new_temp(
        ns,
        ~compressed=false,
        ~f=wr => Write_pusher.run(rd, ~delta=last, ~ustate=wr),
        (),
      )
    );
  /* printf "Old: %S\n" oldtname; */

  let (toutname, _) =
    Naming.with_new_temp(
      ns,
      ~compressed=true,
      ~f=
        wr => {
          write_header(
            wr,
            {
              ...header,
              deltas:
                header.deltas @ [{name, number: last + 1, time: now(), tags}],
            },
          );
          let delwr = (new delta_writer)(wr);
          Naming.with_main_reader(
            ns,
            ~f=main_rd => {
              let dpush = Delta_pusher.make(main_rd, ~delta=last);
              let state =
                Shell.run_fold(
                  "diff",
                  [oldtname, tname],
                  ~expect=[0, 1],
                  ~init=DS.make(delwr, dpush, last + 1),
                  ~f=(state, line) =>
                  switch (Re.exec_opt(diff_re, line)) {
                  | None =>
                    /* printf "diff: %S\n" line; */
                    if (String.length(line) >= 2 && Char.(==)(line.[0], '>')) {
                      delwr#plain(String.subo(line, ~pos=2));
                    };
                    (state, `Continue);
                  | Some(pats) =>
                    let state = DS.diffy(state);
                    let low = Int.of_string(Re.Group.get(pats, 1));
                    let high =
                      if (Re.Group.test(pats, 3)) {
                        Int.of_string(Re.Group.get(pats, 3));
                      } else {
                        low;
                      };
                    let cmd = Re.Group.get(pats, 4);
                    /* printf "%d,%d %S\n" low high cmd; */
                    (DS.change(state, low, high, cmd), `Continue);
                  }
                );
              DS.finish(state);
            },
          );
        },
      (),
    );

  /* printf "toutname: %S\n" toutname; */
  let mname = ns#main_file;
  let backname = ns#backup_file;
  Unix.rename(~src=mname, ~dst=backname);
  Unix.rename(~src=toutname, ~dst=mname);
  Unix.remove(tname);
  Unix.remove(oldtname);

  (result, last + 1);
};
