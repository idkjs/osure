(** Weave parsing. *)

(** These operations are available to the push parser (Pusher) that can
 * be invoked for each record encountered. *)
module type Sink = sig
  type t
  (** Type of value passed through *)
  val insert : t -> int -> t
  (** Hit an insert record. *)
  val delete : t -> int -> t
  (** Hit a delete record. *)
  val ending : t -> int -> t
  (** Hit an end record. *)
  val plain : t -> string -> bool -> t
  (** Hit a plain text record.  The bool indicates if this is included
   * in the desired delta. *)
end

module Pusher (S : Sink) : sig
  val run : Stream.reader -> delta:int -> ustate:S.t -> S.t
  (** Simple case, run the entire process. *)

  type state
  (** An internal state to allow the parser to be run in pieces. *)

  val make : Stream.reader -> delta:int -> state
  (** Construct the initial parser state, reading from a given reader,
   * intending to extract a particular delta. *)

  val push_to : ?stop:int -> state -> S.t -> (int * state * S.t)
  (** Run the parser (pushing) through.  The stop value indicates when
   * the parser should stop.  A value of 0 indicates it should run
   * through to the end.  A non-zero value indicates it should stop
   * when reaching the given line of input (before the line is
   * pushed).  The return result is the line stopped on, the new state
   * of the parser, and the updated value of the user state. *)
end

module Puller : sig
  type state

  val make : Stream.reader -> delta:int -> state

  val pull_plain : state ref -> string option
  (** Run a single step of a pull parser, returning the next line of
   * input, or None when there is no more input. *)
end

val test_check : string -> int -> int array -> unit
val sample : unit -> unit

val read_header : Stream.reader -> string
