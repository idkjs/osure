(** The store.
 * The store maps between a naming convention (directory, basename,
 * etc) weaves stored in that file, and provides the support necessary
 * to read and write sure data to them.
 *)

(** The store itself. *)
type t

(** Printer for the store *)
val show : t -> string

(** Given a filename, decode the name, creating a 't' if possible. *)
val parse : string -> t

(** Print a listing of the given store. *)
val listing : t -> unit

(** Specifier for revision numbers. *)
type revision =
  [ `Latest
  | `Previous
  | `Num of int ]

(** Retrieve a revision, calling 'f' with a pull parser that returns
 * the lines of the revision. *)
val with_rev : t -> revision -> f:((unit -> Node.t option) -> 'a) -> 'a

(** Start writing nodes to a new temp file.  The function is called
 * with a writer argument that writes these nodes to a temp file.
 * Returns the function return value, along with the name of the temp
 * file. *)
val with_temp : t -> f:((Node.t -> unit) -> 'a) -> (string * 'a)

(** Read in a temp file as a node reader. *)
val with_temp_in : string -> gzip:bool -> f:((unit -> Node.t option) -> 'a) -> 'a

(** Create a new temp file, and invoke 'f' with an Sqlite database
 * handle opened on that file. *)
val with_temp_db : t -> f:(Sqlite3.db -> 'a) -> (string * 'a)

(** Call 'f' with a node writer that will make the first delta is
 * done. *)
val with_first_delta : ?tags:Weave.Tags.t -> t -> f:((Node.t -> unit) -> 'a) -> ('a * int)

(** Call 'f' with a node writer that will make a new delta. *)
val with_added_delta : ?tags:Weave.Tags.t -> t -> f:((Node.t -> unit) -> 'a) -> ('a * int)
