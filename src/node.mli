(** Nodes *)

open Core

(* Attributes are most simply represented as a alist. *)
type atts = (string, string, String.comparator_witness) Map.t

type t =
  | Enter of string * atts
  | File of string * atts
  | Sep
  | Leave

val show : t -> string

val stat_to_atts : string -> Unix.stats -> atts
