(* Scanning. *)

open Core

module HashState : sig
  type t
  val show : t -> string
end

(** Scan the tree, hashing any names that don't have hashes, and adding
 * them to the hashes. *)
val hash_update : hstate:HashState.t -> string -> Sqlite3.db ->
  (unit -> Node.t option) -> unit

val hash_count : meter:Progress.t -> (unit -> Node.t option) -> HashState.t

val merge_hashes : (int * string) Sequence.t ->
  (unit -> Node.t option) -> (Node.t -> unit) -> unit
(** Merge the hashes from the database (elts) with the nodes from
 * rnode, writing the updated nodes to wnode. *)
