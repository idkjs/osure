/** Nodes */;

open Core;

/* Attributes are most simply represented as a alist. */
type atts = Map.M(String).t(string);

type t =
  | Enter(string, atts)
  | File(string, atts)
  | Sep
  | Leave;

let equal: (t, t) => bool;

let show: t => string;

let parse: string => t;

let stat_to_atts: (string, Unix.stats) => atts;
