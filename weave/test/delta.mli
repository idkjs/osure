(** Delta testing *)

module type STORE = sig
  (** The type representing whatever data needs to be kept to track
   * this store. *)
  type t

  (** Construct a new store, in the given directory. *)
  val make : string -> t

  (** Write the first version, often different. *)
  val add_initial : t -> f:(Weave.Stream.writer -> unit) -> int

  (** Add a new delta to the store.  The function should write lines
   * of data to the given stream writer.  The whole function should
   * return an int to represent the stored delta. *)
  val add_delta : t -> f:(Weave.Stream.writer -> unit) -> int

  (** Read a delta previously written.  It is a push parser, so user
   * state is pushed through.
   * TODO: It'd be nice to make this much more general, instead of it
   * just collecting a string list, allow it to collect anything, but
   * the type embedded into signatures to be easy to make dynamic, so
   * we just make this specific.  It will probably better to do all of
   * this differently.
   * Instead, for now, this just reads all of the lines in. *)
  val read_delta : t -> delta:int -> string list
end

module Tester (Store : STORE) : sig
  val run : unit -> unit
end
