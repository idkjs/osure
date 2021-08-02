/* Comparators. */

open Core;

/* The comparison should never get to the end of the node data. */
let next = gen =>
  switch (gen()) {
  | None => failwith("Erroneous end of file")
  | Some(x) => x
  };

/* Ensure that we are looking at a directory.  If so, return the name
 * and atts from it. */
let must_dir =
  fun
  | [@implicit_arity] Node.Enter(name, atts) => (name, atts)
  | _ => failwith("Unexpected node type");

/* Report on a missing attribute.  We store the attributes we've seen
 * so we only will warn/report one time.  TODO: This should be logged
 * instead of just printed. */
let missings = ref(Set.empty((module String)));
let missing = (att, side) => {
  let name = sprintf("%s/%s", att, side);
  if (!Set.mem(missings^, name)) {
    printf("Attribute [%s] missing from %s\n", att, side);
    missings := Set.add(missings^, name);
  };
};

/* Compare attributes from two entities. */
let attcmp = (aatts, batts, name) => {
  let rec walk = diffs =>
    fun
    | ([], []) => diffs
    | ([(ak, _), ...aa], []) => {
        missing(ak, "prior");
        walk(diffs, (aa, []));
      }
    | ([], [(bk, _), ...bb]) => {
        missing(bk, "current");
        walk(diffs, ([], bb));
      }
    | ([(ak, _), ...aa], [(bk, _), ..._] as bb) when String.(ak < bk) => {
        missing(ak, "prior");
        walk(diffs, (aa, bb));
      }
    | ([(ak, _), ..._] as aa, [(bk, _), ...bb]) when String.(ak > bk) => {
        missing(bk, "current");
        walk(diffs, (aa, bb));
      }
    | ([("ino", _), ...aa], [_, ...bb]) => walk(diffs, (aa, bb))
    | ([("ctime", _), ...aa], [_, ...bb]) => walk(diffs, (aa, bb))
    | ([(ak, av), ...aa], [(_, bv), ...bb]) =>
      if (String.(av != bv)) {
        walk([ak, ...diffs], (aa, bb));
      } else {
        walk(diffs, (aa, bb));
      };

  let aatts = Map.to_alist(aatts);
  let batts = Map.to_alist(batts);
  let diffs = walk([], (aatts, batts));
  if (!List.is_empty(diffs)) {
    let buf = Buffer.create(20);
    List.iter(
      List.rev(diffs),
      ~f=name => {
        if (Buffer.length(buf) > 0) {
          bprintf(buf, ",");
        };
        bprintf(buf, "%s", name);
      },
    );
    printf("  [%-20s] %S\n", Buffer.contents(buf), name);
  };
};

let print_alone = (change, kind, name) =>
  printf("%c %-20s   %S\n", change, kind, name);

let compare = (agen, bgen) => {
  let rec subdir = dir => children(dir, (next(agen), next(bgen)))
  and children = dir =>
    fun
    | (Node.Sep, Node.Sep) => files(dir, (next(agen), next(bgen)))
    | ([@implicit_arity] Enter(aname, _), Sep) => {
        print_alone('-', "dir", dir ^/ aname);
        consume(agen);
        children(dir, (next(agen), Sep));
      }
    | (Sep, [@implicit_arity] Enter(bname, _)) => {
        print_alone('+', "dir", dir ^/ bname);
        consume(bgen);
        children(dir, (Sep, next(bgen)));
      }
    | (
        [@implicit_arity] Enter(aname, _),
        [@implicit_arity] Enter(bname, _) as bb,
      )
        when String.(aname < bname) => {
        print_alone('-', "dir", dir ^/ aname);
        consume(agen);
        children(dir, (next(agen), bb));
      }
    | (
        [@implicit_arity] Enter(aname, _) as aa,
        [@implicit_arity] Enter(bname, _),
      )
        when String.(aname > bname) => {
        print_alone('+', "dir", dir ^/ bname);
        consume(bgen);
        children(dir, (aa, next(bgen)));
      }
    | (
        [@implicit_arity] Enter(aname, aatts),
        [@implicit_arity] Enter(_, batts),
      ) => {
        attcmp(aatts, batts, dir ^/ aname);
        subdir(dir ^/ aname);
        children(dir, (next(agen), next(bgen)));
      }
    | _ => failwith("Invalid directory structure")
  and files = dir =>
    fun
    | (Leave, Leave) => ()
    | ([@implicit_arity] File(aname, _), Leave) => {
        print_alone('-', "file", dir ^/ aname);
        files(dir, (next(agen), Leave));
      }
    | (Leave, [@implicit_arity] File(bname, _)) => {
        print_alone('+', "file", dir ^/ bname);
        files(dir, (Leave, next(bgen)));
      }
    | (
        [@implicit_arity] File(aname, _),
        [@implicit_arity] File(bname, _) as bb,
      )
        when String.(aname < bname) => {
        print_alone('-', "file", dir ^/ aname);
        files(dir, (next(agen), bb));
      }
    | (
        [@implicit_arity] File(aname, _) as aa,
        [@implicit_arity] File(bname, _),
      )
        when String.(aname > bname) => {
        print_alone('+', "file", dir ^/ bname);
        files(dir, (aa, next(bgen)));
      }
    | (
        [@implicit_arity] File(aname, aatts),
        [@implicit_arity] File(_, batts),
      ) => {
        attcmp(aatts, batts, dir ^/ aname);
        files(dir, (next(agen), next(bgen)));
      }
    | _ => failwith("Invalid file structure")
  and consume = gen => {
    let node = next(gen);
    /* printf "consume: %s\n" (Node.show node); */
    switch (node) {
    | Node.Leave => ()
    | Sep
    | File(_) => consume(gen)
    | Enter(_) =>
      consume(gen);
      consume(gen);
    };
  };

  /* Both must start with an Enter node, for this to be meaningful. */
  let a = next(agen);
  let b = next(bgen);

  let (aname, aatts) = must_dir(a);
  let (bname, batts) = must_dir(b);
  if (String.(aname != bname)) {
    failwith("Root directories have differing names");
  };
  attcmp(aatts, batts, ".");
  subdir(".");
};
