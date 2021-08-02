/** A generalized progress meter. */;

open Core;

/** The type of a progress meter. */

type t;

/** Perform some operation with a progress meter present. */

let with_meter: (~f: t => 'a) => 'a;

/** Insert a meter to show 'scan' progress. */

let scan_meter: (t, Sequence.t(Node.t)) => Sequence.t(Node.t);

/** Update what would be shown on the meter. */

let update: (t, ~f: unit => string) => unit;

/** Convert a file size to a readable summary form. */

let humanize_size: int64 => string;
