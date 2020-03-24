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
  val pusher : Stream.reader -> delta:int -> s0:S.t -> S.t
end

val test_check : string -> int -> int array -> unit
val sample : unit -> unit
