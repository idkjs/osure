open Core;
open Osure;

module SureFile = {
  type t = {
    dir: string,
    file: string,
    store: Store.t,
  };
  let show = ({dir, file, store}) =>
    sprintf("{dir=%S;file=%S;store=%s}", dir, file, Store.show(store));

  let t_param = {
    open Command.Let_syntax;
    let%map_open dir =
      flag(
        "--dir",
        ~aliases=["-d"],
        optional(string),
        ~doc="Directory to scan, defaults to \".\"",
      )
    and file =
      flag(
        "--file",
        ~aliases=["-f"],
        optional(string),
        ~doc="Filename for surefile, defaults to 2sure.dat.gz",
      );
    let dir = Option.value(dir, ~default=".");
    let file = Option.value(file, ~default="2sure.dat.gz");
    let store = Store.parse(file);
    {dir, file, store};
  };
};

let list_act = (sfile: SureFile.t) => Store.listing(sfile.store);

let scan_or_update = (op, sfile: SureFile.t, tags) => {
  let (dbname, _) =
    Store.with_temp_db(
      sfile.store,
      ~f=db => {
        Db.make_hash_schema(db);

        /* Scan the filesystem (without hashes). */
        let (scan_temp, _) =
          Store.with_temp(sfile.store, ~f=wnode =>{
            Progress.with_meter(~f=meter =>{
              let elts = Walk.walk(sfile.dir);
              let elts = Progress.scan_meter(meter, elts);
              Sequence.iter(elts, ~f=elt => wnode(elt));
            })
          });

        /* Update the hash from the latest revision, if there is one. */
        let (hashed_file, rdfile) =
          switch (op) {
          | `Update =>
            let (hashed_file, _) =
              Store.with_temp(sfile.store, ~f=whashed =>{
                Store.with_rev(sfile.store, `Latest, ~f=older =>{
                  Store.with_temp_in(scan_temp, ~gzip=false, ~f=latest =>
                    Scan.cp_hashes(~older, ~latest, whashed)
                  )
                })
              });
            (Some(hashed_file), hashed_file);
          | `Scan => (None, scan_temp)
          };

        Progress.with_meter(~f=meter =>{
          let hstate =
            Store.with_temp_in(rdfile, ~gzip=false, ~f=rnode =>
              Scan.hash_count(~meter, rnode)
            );
          Store.with_temp_in(rdfile, ~gzip=false, ~f=rnode =>
            Scan.hash_update(~hstate, sfile.dir, db, rnode)
          );
        });

        /* Retrieve the hashes */
        let wither =
          switch (op) {
          | `Update => Store.with_added_delta
          | `Scan => Store.with_first_delta
          };
        let (_, delta) =
          wither(~tags, sfile.store, ~f=wnode =>
            Db.with_hashes(db, ~f=elts =>
              Store.with_temp_in(rdfile, ~gzip=false, ~f=rnode =>
                Scan.merge_hashes(elts, rnode, wnode)
              )
            )
          );

        printf("New delta: %d\n%!", delta);

        Option.iter(hashed_file, ~f=Unix.unlink);
        Unix.unlink(scan_temp);
      },
    );
  Unix.unlink(dbname);
};

let scan_act = (sfile: SureFile.t, tags) =>
  scan_or_update(`Scan, sfile, tags);

let update_act = (sfile: SureFile.t, tags) =>
  scan_or_update(`Update, sfile, tags);

let with_tdir = (~f) => {
  let tdir = Filename.temp_dir("osure", "");
  let ex =
    try(Ok(f(tdir))) {
    | ex => Error(ex)
    };
  if (Option.is_none(Sys.getenv("OSURE_KEEP"))) {
    FileUtil.rm(~recurse=true, [tdir]);
  };
  switch (ex) {
  | Ok(result) => result
  | Error(ex) => raise(ex)
  };
};

let check_act = (sfile: SureFile.t) =>
  with_tdir(~f=tdir =>{
    let tstore = Store.parse(tdir ^/ "2sure.dat.gz");
    scan_or_update(`Scan, {...sfile, store: tstore}, []);
    Store.with_rev(sfile.store, `Latest, ~f=prior =>
      Store.with_rev(tstore, `Latest, ~f=current =>
        Compare.compare(prior, current)
      )
    );
  });

let signoff_act = (sfile: SureFile.t) =>
  Store.with_rev(sfile.store, `Previous, ~f=prior =>
    Store.with_rev(sfile.store, `Latest, ~f=current =>
      Compare.compare(prior, current)
    )
  );

let general = (act, summary) =>
  Command.basic(
    ~summary,
    {
      open Command.Let_syntax;
      let%map_open sfile = SureFile.t_param;
      () => act(sfile);
    },
  );

let tagged = (act, summary) =>
  Command.basic(
    ~summary,
    {
      open Command.Let_syntax;
      let%map_open sfile = SureFile.t_param
      and tags =
        flag(
          "--tag",
          ~aliases=["-t"],
          listed(string),
          ~doc="Add tag as key=value",
        );
      () => {
        let tags = List.map(tags, ~f=Weave.Tags.from_equal);
        act(sfile, tags);
      };
    },
  );

let () =
  /* let open Command.Let_syntax in */
  Command.group(
    ~summary="Rsure",
    [
      ("scan", tagged(scan_act, "Scan a directory for the first time")),
      ("update", tagged(update_act, "Update the scan using the dat file")),
      ("list", general(list_act, "List revisions in a given sure store")),
      (
        "check",
        general(check_act, "Compare the directory with the dat file"),
      ),
      (
        "signoff",
        general(signoff_act, "Compare the last two version in dat file"),
      ),
    ],
  )
  |> Command.run;

let nothing = () =>
  Sequence.iter(Walk.walk("."), ~f=e => print_endline @@ Node.show(e));

let bench_stream = () => Weave.Stream.bulk_io();

/* Read a large file to get an idea of timing. */
let bench_gzip_in = () => {
  let fd = Weave.Stream.gzip_in("/home/2sure.dat.gz");
  let rec loop = count =>
    switch (fd#read_line) {
    | Some(_) => loop(count + 1)
    | None => count
    };
  let total = loop(0);
  fd#close;
  printf("%d lines\n", total);
};

let naming = _ => Weave.Naming.trial();

let sample = () => Weave.Parse.sample();

module Load_sink: Weave.Parse.Sink with type t = list(string) = {
  type t = list(string);
  let insert = (xs, _) => xs;
  let delete = (xs, _) => xs;
  let ending = (xs, _) => xs;
  let plain = (xs, text, keep) =>
    if (keep) {
      [text, ...xs];
    } else {
      xs;
    };
};

module Count_sink: Weave.Parse.Sink with type t = int = {
  type t = int;
  let insert = (xs, _) => xs;
  let delete = (xs, _) => xs;
  let ending = (xs, _) => xs;
  let plain = (xs, _, keep) =>
    if (keep) {
      xs + 1;
    } else {
      xs;
    };
};

module Loader = Weave.Parse.Pusher(Load_sink);
module Counter = Weave.Parse.Pusher(Count_sink);

/* Try reading in my largest sure file for some benchmarking ideas. */
let benchy = () => {
  let sn =
    Weave.Naming.simple_naming(
      ~path="/home",
      ~base="2sure",
      ~ext="dat",
      ~compress=true,
    );
  let header = Weave.Naming.with_main_reader(sn, ~f=Weave.Parse.read_header);
  let header = Weave.Header_j.header_of_string(header);
  /* printf "%S\n" (Weave.Header_j.string_of_header header) */
  let deltas = List.take(header.deltas, 4);
  List.iter(

      deltas,
      ~f=ver => {
        printf("Version %d %S\n", ver.number, ver.name);
        Out_channel.flush(stdout);
        /* */
        let lines =
          Weave.Naming.with_main_reader(sn, ~f=rd =>
            Loader.run(rd, ~delta=ver.number, ~ustate=[])
          );
        printf("   There are %d lines\n", List.length(lines));
      },
    );
    /* */
    /*
     let lines = Weave.Naming.with_main_reader sn ~f:(fun rd ->
       Counter.run rd ~delta:ver.number ~ustate:0) in
     printf "   There are %d lines\n" lines;
     Out_channel.flush stdout
     */
};

/*
 let _ =
   Weave.Write.sample ()
 */
