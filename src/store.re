/* Store */

open Core;

type t = {ns: Weave.Naming.t};

let show = st => sprintf("{ns=%S}", (st.ns)#main_file);

let read_header = st => {
  let header =
    Weave.Naming.with_main_reader(st.ns, ~f=rd =>
      Weave.Parse.read_header(rd)
    );
  Weave.Header_j.header_of_string(header);
};

let listing = st => {
  let header = read_header(st);
  printf("vers | Time captured                    | name\n");
  printf("-----+----------------------------------+---------------------\n");
  List.iter(header.deltas, ~f=ver =>
    printf("% 4d | %-32s | %s\n", ver.number, ver.time, ver.name)
  );
};

let parse = file => {
  let (dir, file) = Filename.split(file);
  printf("dir=%S file=%S\n", dir, file);
  if (!String.(suffix(file, 7) == ".dat.gz")) {
    failwith("Currently, filename must end in \".dat.gz\"");
  };
  let base = String.drop_suffix(file, 7);
  let ns =
    Weave.Naming.simple_naming(~path=dir, ~base, ~ext="dat", ~compress=true);
  {ns: ns};
};

type revision = [ | `Latest | `Previous | `Num(int)];

let get_rev = (st, rev) => {
  let header = read_header(st);
  let deltas = List.rev(header.deltas);
  switch (rev) {
  | `Latest => List.nth(deltas, 0)
  | `Previous => List.nth(deltas, 1)
  | `Num(n) => List.find(~f=x => x.number == n, deltas)
  };
};

let must = (state, expect) =>
  switch (Weave.Parse.Puller.pull_plain(state)) {
  | None => failwith("Unexpected end of input")
  | Some(text) when String.(text == expect) => ()
  | Some(text) => failwith(sprintf("Unexpected line: %S", text))
  };

let rd_must = (rd, expect) =>
  switch (rd#read_line) {
  | None => failwith("Unexpected end of input")
  | Some(text) when String.(text == expect) => ()
  | Some(text) => failwith(sprintf("Unexpected line: %S", text))
  };

let with_rev = (st, rev, ~f) => {
  let rev = get_rev(st, rev);
  let rev = Option.value_exn(rev);
  let delta = rev.number;
  Weave.Naming.with_main_reader(
    st.ns,
    ~f=rd => {
      let state = ref(Weave.Parse.Puller.make(rd, ~delta));
      must(state, "asure-2.0");
      must(state, "-----");
      f(() =>
        Option.map(Weave.Parse.Puller.pull_plain(state), ~f=Node.parse)
      );
    },
  );
};

let with_temp = (st, ~f) =>
  Weave.Naming.with_new_temp(
    st.ns,
    ~compressed=false,
    ~f=
      wr => {
        wr#write_lines(["asure-2.0", "-----"]);
        f(node => wr#write_lines([Node.show(node)]));
      },
    (),
  );

let with_temp_in = (fname, ~gzip, ~f) =>
  Weave.Stream.with_in(
    fname,
    ~gzip,
    ~f=rd => {
      rd_must(rd, "asure-2.0");
      rd_must(rd, "-----");
      f(() => Option.map(rd#read_line, ~f=Node.parse));
    },
  );

let with_temp_db = (st, ~f) =>
  Weave.Naming.with_new_temp(
    st.ns,
    ~compressed=false,
    ~f=
      wr => {
        wr#close;
        let name = wr#name;
        let db = Sqlite3.db_open(~mutex=`FULL, name);
        let result =
          try(Ok(f(db))) {
          | ex => Error(ex)
          };
        let _ = Sqlite3.db_close(db); /* TODO warn if can't close */
        switch (result) {
        | Ok(result) => result
        | Error(ex) => raise(ex)
        };
      },
    (),
  );

let with_first_delta = (~tags=[], st, ~f) =>
  Weave.Write.with_first_delta(
    st.ns,
    ~tags,
    ~f=wr => {
      wr#write_lines(["asure-2.0", "-----"]);
      f(node => wr#write_lines([Node.show(node)]));
    },
  );

let with_added_delta = (~tags=[], st, ~f) =>
  Weave.Write.with_new_delta(
    st.ns,
    ~tags,
    ~f=wr => {
      wr#write_lines(["asure-2.0", "-----"]);
      f(node => wr#write_lines([Node.show(node)]));
    },
  );
