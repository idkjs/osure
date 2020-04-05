(* Database operations. *)

(** Add the schema for the hash database to the given database. *)
val make_hash_schema : Sqlite3.db -> unit

(** Evaluate f in the context of a transaction.  If f exits normally,
 * the transaction will be committed.  If it raises an exception, the
 * transaction will be rolled back. *)
val with_xact : Sqlite3.db -> f:(Sqlite3.db -> 'a) -> 'a

(** Run f in a context where its argument is valid as a sequence to
 * retrieve the hashes (sorted by id).  The sequence operations will
 * become invalid once this function returns. *)
val with_hashes : Sqlite3.db -> f:((int * string) Core.Sequence.t -> 'a) -> 'a
