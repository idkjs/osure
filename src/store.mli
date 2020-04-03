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
