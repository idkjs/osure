/* Scanning */

open Core;

module Internal: {
  /** Augment the node coming from a sure tree to track the full path. */

  let track_path:
    (string, Sequence.t(Node.t)) => Sequence.t((Node.t, string));

  /** Determine if the given node should have a hash, but is missing
   * one.  The node must be a "file" not, with a kind of "file", and not
   * already have a sha1 attribute. */

  let needs_hash: Node.t => bool;
} = {
  /** Reverse and combine the components into a path. */

  let revcombine =
    fun
    | [] => "."
    | elts => List.reduce_exn(List.rev(elts), ~f=(^/));

  let track_path = (root, seq) =>
    Sequence.folding_map(
      seq, ~init=([root], root), ~f=((elts, path), node) =>
      switch (node) {
      | [@implicit_arity] Node.Enter("__root__", _)
          when List.length(elts) == 1 =>
        /* The root directory is a special case, we use the passed
         * in name instead of the "__root__" that will be in the
         * node.  There isn't anything preventing the name
         * "__root__" from occurring elsewhere, but it saves us from
         * having to compute the element list length every time. */
        ((elts, path), (node, path))
      | [@implicit_arity] Node.Enter(name, _) =>
        let elts = [name, ...elts];
        let path = revcombine(elts);
        ((elts, path), (node, path));
      | Leave =>
        let elts =
          switch (elts) {
          | [] => failwith("Too many Leave nodes")
          | [_, ...elts] => elts
          };
        let path = revcombine(elts);
        ((elts, path), (node, path));
      | Sep => ((elts, path), (node, path))
      | [@implicit_arity] File(name, _) => (
          (elts, path),
          (node, path ^/ name),
        )
      }
    );

  let needs_hash =
    fun
    | Node.Enter(_)
    | Leave
    | Sep => false
    | [@implicit_arity] File(_, atts) =>
      switch (Map.find(atts, "kind")) {
      | Some("file") => !Map.mem(atts, "sha1")
      | _ => false
      };
};

open Internal;

let to_seq = f =>
  Sequence.unfold(~init=f, ~f=f => Option.map(f(), ~f=node => (node, f)))
  |> Sequence.memoize;

module HashState = {
  type t = {
    mutable files: int64,
    mutable tfiles: int64,
    mutable octets: int64,
    mutable toctets: int64,
    meter: Progress.t,
  };
  let show = t => sprintf("%Ld files, %Ld bytes", t.tfiles, t.toctets);
};

let node_size =
  fun
  | [@implicit_arity] Node.File(_, atts) =>
    switch (Map.find(atts, "size")) {
    | None => 0L
    | Some(text) => Int64.of_string(text)
    }
  | _ => 0L;

let hash_count = (~meter, prior) => {
  let state = {
    HashState.files: 0L,
    tfiles: 0L,
    octets: 0L,
    toctets: 0L,
    meter,
  };
  Sequence.iter(to_seq(prior), ~f=node =>
    if (needs_hash(node)) {
      state.tfiles = Int64.succ(state.tfiles);
      state.toctets = Int64.(+)(state.toctets, node_size(node));
    }
  );
  state;
};

let update_meter = (hstate: HashState.t, node) => {
  hstate.files = Int64.succ(hstate.files);
  hstate.octets = Int64.(+)(hstate.octets, node_size(node));
  Progress.update(hstate.meter, ~f=() =>
    sprintf(
      "  %6Ld/%6Ld (%5.1f%%) files, %s/%s (%5.1f%%) bytes",
      hstate.files,
      hstate.tfiles,
      Float.(
        Int64.to_float(hstate.files) * 100.0 / Int64.to_float(hstate.tfiles)
      ),
      Progress.humanize_size(hstate.octets),
      Progress.humanize_size(hstate.toctets),
      Float.(
        Int64.to_float(hstate.octets)
        * 100.0
        / Int64.to_float(hstate.toctets)
      ),
    )
  );
};

/** A Hashing module performs updates of the hash database. */
module type Hasher = {
  type t;
  let make: (~hstate: HashState.t, Sqlite3.db) => t;
  let hash_file: (t, int, Node.t, string) => unit;
  let finalize: t => unit;
};

/** Hashing module that computes hashes and adds them to the database,
 * all within a single thread. */
module Direct_hash: Hasher = {
  type t = {
    hstate: HashState.t,
    db: Sqlite3.db,
    stmt: Sqlite3.stmt,
  };

  let make = (~hstate, db) => {
    let stmt = Sqlite3.prepare(db, "INSERT INTO hashes VALUES (?, ?)");
    {hstate, db, stmt};
  };

  let hash_file = (t, index, node, path) => {
    let hash =
      try(Some(Sha1.hash_file(path))) {
      | _ex =>
        eprintf("Warning: error hashing %S\n", path);
        None;
      };

    update_meter(t.hstate, node);

    Option.iter(
      hash,
      ~f=hash => {
        Sqlite3.bind_int(t.stmt, 1, index) |> Sqlite3.Rc.check;
        Sqlite3.bind_blob(t.stmt, 2, hash) |> Sqlite3.Rc.check;
        switch (Sqlite3.step(t.stmt)) {
        | DONE => ()
        | err => failwith(Sqlite3.Rc.to_string(err))
        };
        Sqlite3.reset(t.stmt) |> Sqlite3.Rc.check;
      },
    );
  };

  let finalize = t => Sqlite3.finalize(t.stmt) |> Sqlite3.Rc.check;
};

module Threaded_hash: Hasher = {
  /* Number of cpus.  This should actually get from the system. */
  let ncpu = 4;

  let nworkers = ncpu;

  /* We create n+1 threads, where the n threads run hash_worker, and
   * the remaining thread runs hash_collector. */

  type t = {
    /* The work queue.  The main thread will write to this, and the
     * workers will pull from it.  When the main thread is done, it
     * will push a None for each worker to indicate they should
     * finish. */
    work: Channel.t(option((int, Node.t, string))),
    /* The workers push their state to the finish queue as they hash.
     * Each worker will also push a None to indicate they are finished
     * with all of their work. */
    finish: Channel.t(option((int, Node.t, string))),
    /* The threads. */
    collector: Thread.t,
  };

  let hash_collector = (finish, ~hstate, db) => {
    let stmt = Sqlite3.prepare(db, "INSERT INTO hashes VALUES (?, ?)");
    let rec loop = workers =>
      if (workers == 0) {
        ();
      } else {
        switch (Channel.pop(finish)) {
        | None => loop(workers - 1)
        | Some((index, node, hash)) =>
          update_meter(hstate, node);
          Sqlite3.bind_int(stmt, 1, index) |> Sqlite3.Rc.check;
          Sqlite3.bind_blob(stmt, 2, hash) |> Sqlite3.Rc.check;
          switch (Sqlite3.step(stmt)) {
          | DONE => ()
          | err => failwith(Sqlite3.Rc.to_string(err))
          };
          Sqlite3.reset(stmt) |> Sqlite3.Rc.check;
          loop(workers);
        };
      };
    loop(nworkers);
    Sqlite3.finalize(stmt) |> Sqlite3.Rc.check;
  };

  let rec hash_worker = t =>
    switch (Channel.pop(t.work)) {
    | None => Channel.push(t.finish, None)
    | Some((index, node, path)) =>
      let hash =
        try(Some(Sha1.hash_file(path))) {
        | _ex =>
          eprintf("Warning: error hashing %S\n%!", path);
          None;
        };

      Option.iter(hash, ~f=hash =>
        Channel.push(t.finish, Some((index, node, hash)))
      );
      hash_worker(t);
    };

  let make = (~hstate, db) => {
    let finish = Channel.create(~bound=2 * ncpu);
    let collector =
      Thread.create(
        hash_collector(finish, ~hstate),
        db,
        ~on_uncaught_exn=`Kill_whole_process,
      );

    let t = {work: Channel.create(~bound=2 * ncpu), finish, collector};
    for (_ in 1 to nworkers) {
      let _ =
        Thread.create(hash_worker, t, ~on_uncaught_exn=`Kill_whole_process);
      ();
    };
    t;
  };

  let hash_file = (t, index, node, path) =>
    Channel.push(t.work, Some((index, node, path)));

  let finalize = t => {
    /* Tell all of the workers we are done. */
    for (_ in 1 to nworkers) {
      Channel.push(t.work, None);
    };

    /* Wait for the collector to finish. */
    Thread.join(t.collector);
  };
};

module Hash_update = (H: Hasher) => {
  let hash_update = (~hstate, path, db, prior) => {
    printf("hash update\n");
    let nodes = to_seq(prior);
    let nodes = track_path(path, nodes);
    Db.with_xact(
      db,
      ~f=db => {
        let hasher = H.make(~hstate, db);
        Sequence.iteri(nodes, ~f=(index, (node, path)) =>
          if (needs_hash(node)) {
            /*printf "Node: %s\n" (Node.show node);*/
            /*printf "  %S\n" path;*/
            H.hash_file(
              hasher,
              index,
              node,
              path,
            );
          }
        );
        H.finalize(hasher);
      },
    );
  };
};

/* include Hash_update (Direct_hash) */
include Hash_update(Threaded_hash);

/** Update the hash within a node. */

let node_newhash = (node, newhash) => {
  let newhash = Sha1.hexlify(newhash);
  switch (node) {
  | [@implicit_arity] Node.File(name, atts) =>
    [@implicit_arity]
    Node.File(name, Map.set(atts, ~key="sha1", ~data=newhash))
  | _ => failwith("Node sequence error")
  };
};

let merge_hashes = (elts, rnode, wnode) => {
  let input = to_seq(rnode);
  let input = Sequence.mapi(input, ~f=(index, node) => (index, node));
  let rec loop =
    fun
    | (None, None) => ()
    | (Some((_, anext)), None) => loop((Sequence.next(anext), None))
    | (None, Some(((_, bnode), bnext))) => {
        wnode(bnode);
        loop((None, Sequence.next(bnext)));
      }
    | (
        Some(((aindex, aelt), anext)) as aa,
        Some(((bindex, belt), bnext)) as bb,
      ) =>
      if (aindex < bindex) {
        loop((Sequence.next(anext), bb));
      } else if (aindex > bindex) {
        wnode(belt);
        loop((aa, Sequence.next(bnext)));
      } else {
        /* Update */
        let belt = node_newhash(belt, aelt);
        wnode(belt);
        loop((Sequence.next(anext), Sequence.next(bnext)));
      };

  loop((Sequence.next(elts), Sequence.next(input)));
};

let must_dir =
  fun
  | [@implicit_arity] Node.Enter(name, _) => name
  | _ => failwith("Unexpected node type");

let is_file = atts =>
  switch (Map.find(atts, "kind")) {
  | Some("file") => true
  | _ => false
  };

/** Determine if these sets of attributes are similar enough to copy
 * the 'sha1' from the older set to the newer. */

let migrate_hash = (older, newer) =>
  /* printf "older: %s\n" (Node.show (Node.File ("fake", older))); */
  /* printf "newer: %s\n" (Node.show (Node.File ("fake", newer))); */
  /* Don't do anything if a hash exists already */
  if (Map.mem(newer, "sha1")) {
    newer;
  } else if
    /* Don't copy unless both are files. */
    (!(is_file(older) && is_file(newer))) {
    newer;
  } else {
    let attsame = key =>
      switch (Map.find(older, key), Map.find(newer, key)) {
      | (Some(v1), Some(v2)) when String.(v1 == v2) => true
      | _ => false
      };

    switch (Map.find(older, "sha1")) {
    | None => newer
    | Some(sha1) =>
      if (attsame("ino") && attsame("ctime")) {
        Map.set(newer, ~key="sha1", ~data=sha1);
      } else {
        newer;
      }
    };
  };

let cp_hashes = (~older, ~latest, out) => {
  let next = gen =>
    switch (gen()) {
    | Some(node) => node
    | None => failwith("Unexpected end of tree")
    };

  let rec children = (anode, bnode) =>
    switch (anode, bnode) {
    | (Node.Sep, Node.Sep) =>
      out(Node.Sep);
      files(next(older), next(latest));
    | (Enter(_), Sep) =>
      aconsume();
      children(next(older), bnode);
    | (Sep, Enter(_)) =>
      out(bnode);
      bconsume();
      children(anode, next(latest));
    | ([@implicit_arity] Enter(aname, _), [@implicit_arity] Enter(bname, _))
        when String.(aname < bname) =>
      aconsume();
      children(next(older), bnode);
    | ([@implicit_arity] Enter(aname, _), [@implicit_arity] Enter(bname, _))
        when String.(aname > bname) =>
      out(bnode);
      bconsume();
      children(anode, next(latest));
    | (Enter(_), Enter(_)) =>
      out(bnode);
      children(next(older), next(latest));
      children(next(older), next(latest));
    | _ => failwith("Invalid node in tree")
    }
  and files = (anode, bnode) =>
    switch (anode, bnode) {
    | (Node.Leave, Node.Leave) => out(bnode)
    | (File(_), Leave) => files(next(older), bnode)
    | (Leave, File(_)) =>
      out(bnode);
      files(anode, next(latest));
    | ([@implicit_arity] File(aname, _), [@implicit_arity] File(bname, _))
        when String.(aname < bname) =>
      files(next(older), bnode)
    | ([@implicit_arity] File(aname, _), [@implicit_arity] File(bname, _))
        when String.(aname > bname) =>
      out(bnode);
      files(anode, next(latest));
    | (
        [@implicit_arity] File(_, aatts),
        [@implicit_arity] File(bname, batts),
      ) =>
      let batts = migrate_hash(aatts, batts);
      out([@implicit_arity] File(bname, batts));
      files(next(older), next(latest));
    | _ => failwith("Invalid node in file part of tree")
    }
  and aconsume = () => {
    let node = next(older);
    switch (node) {
    | Node.Leave => ()
    | Sep
    | File(_) => aconsume()
    | Enter(_) =>
      aconsume();
      aconsume();
    };
  }
  and bconsume = () => {
    let node = next(latest);
    out(node);
    switch (node) {
    | Node.Leave => ()
    | Sep
    | File(_) => bconsume()
    | Enter(_) =>
      bconsume();
      bconsume();
    };
  };

  let a = next(older);
  let b = next(latest);

  out(b);

  let aname = must_dir(a);
  let bname = must_dir(b);

  if (String.(aname != bname)) {
    failwith("Root directories have differing names");
  };
  children(next(older), next(latest));
};
