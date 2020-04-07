(** A single pipe that is a message channel.
 *
 * These are simple channels, built upon the mutex/condition
 * primitives in ocaml (which are built on the Posix primitives).
 *)

(** A message channel capable of conveying messages of type 'a.  The
 * channel is a FIFO queue, with an upper bound determined at creation
 * time. *)
type 'a t

(** Create a new message channel with the given bound.  Attempts to
 * 'push' when the channel already contains that many messages will
 * block until there is room. *)
val create : bound:int -> _ t

(** Push a new message to the channel, blocking if the queue is
 * already at the bound. *)
val push : 'a t -> 'a -> unit

(** Pop a message off of the queue.  There is no mechanism to indicate
 * "closing" of a channel, which should be done with the contents of
 * the messages. *)
val pop :'a t -> 'a
