(** Nodes *)

open Core

(* Attributes are most simply represented as a alist. *)
type atts = (string, string, String.comparator_witness) Map.t

type t =
  | Enter of string * atts
  | File of string * atts
  | Sep
  | Leave

val equal : t -> t -> bool

val show : t -> string

val parse : string -> t

val stat_to_atts : string -> Unix.stats -> atts
