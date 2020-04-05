(* Scanning. *)

open Core

(** Scan the tree, hashing any names that don't have hashes, and adding
 * them to the hashes. *)
val hash_update : string -> Sqlite3.db -> (unit -> Node.t option) -> unit

val merge_hashes : (int * string) Sequence.t -> (unit -> Node.t option) ->
  (Node.t -> unit) -> unit
(** Merge the hashes from the database (elts) with the nodes from
 * rnode, writing the updated nodes to wnode. *)
