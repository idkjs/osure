(** Comparisons between trees. *)

(** Compare two trees, each made with a node generator. *)
val compare : (unit -> Node.t option) -> (unit -> Node.t option) -> unit
