/* Walk a directory into a sequence. */

open Core;

module Se = Sequence;

let scan_dir = path => {
  let dirh = Unix.opendir(path);
  let rec loop = (dirs, files) =>
    switch (Unix.readdir_opt(dirh)) {
    | None => (dirs, files)
    | Some(".")
    | Some("..") => loop(dirs, files)
    | Some(name) =>
      let sbuf = Unix.lstat @@ path ^/ name;
      switch (sbuf.st_kind) {
      | Unix.S_DIR => loop([(name, sbuf), ...dirs], files)
      | _ => loop(dirs, [(name, sbuf), ...files])
      };
    };
  let (dirs, files) = loop([], []);
  Unix.closedir(dirh);
  let comp = ((a, _), (b, _)) => String.compare(a, b);
  let dirs = List.sort(~compare=comp, dirs);
  let files = List.sort(~compare=comp, files);
  (dirs, files);
};

let walk = path => {
  let root_stat = Unix.lstat(path);
  open Se.Generator;
  let rec walk = (path, name, stat) => {
    /* printf "Walk: %s\n" path; */
    let (dirs, files) =
      if (root_stat.st_dev == stat.Unix.st_dev) {
        scan_dir(path);
      } else {
        ([], []);
      };

    yield @@
    [@implicit_arity] Node.Enter(name, Node.stat_to_atts(path ^/ name, stat))
    >>= (
      () =>
        subdirs(path, dirs)
        >>= (
          () =>
            yield(Node.Sep)
            >>= (() => subfiles(path, files) >>= (() => yield @@ Node.Leave))
        )
    );
  }
  and subdirs = path =>
    fun
    | [] => return()
    | [(name, stat), ...xs] =>
      walk(path ^/ name, name, stat) >>= (() => subdirs(path, xs))
  and subfiles = path =>
    fun
    | [] => return()
    | [(name, stat), ...xs] =>
      yield @@
      [@implicit_arity]
      Node.File(name, Node.stat_to_atts(path ^/ name, stat))
      >>= (() => subfiles(path, xs));

  run @@ walk(path, "__root__", root_stat);
};
