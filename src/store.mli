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
