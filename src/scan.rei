/* Scanning. */

open Core;

module HashState: {
  type t;
  let show: t => string;
};

/** Turn a fetching function into a sequence.  Sequences don't require
 * that the result be evaluated in order, so we have to put a little
 * work into making sure this works (using memoize). */

let to_seq: (unit => option('a)) => Sequence.t('a);

/** Scan the tree, hashing any names that don't have hashes, and adding
 * them to the hashes. */

let hash_update:
  (~hstate: HashState.t, string, Sqlite3.db, unit => option(Node.t)) => unit;

let hash_count: (~meter: Progress.t, unit => option(Node.t)) => HashState.t;

/** Merge the hashes from the database (elts) with the nodes from
 * rnode, writing the updated nodes to wnode. */

let merge_hashes:
  (Sequence.t((int, string)), unit => option(Node.t), Node.t => unit) =>
  unit;

/** cp_hashes older latest -> dest.  Using the tree generated by
 * 'older', makes a copy of 'latest', updating the hashes in files
 * that have not had changes to their contents. */

let cp_hashes:
  (
    ~older: unit => option(Node.t),
    ~latest: unit => option(Node.t),
    Node.t => unit
  ) =>
  unit;
