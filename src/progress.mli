(** A generalized progress meter. *)

open Core

(** The type of a progress meter. *)
type t

(** Perform some operation with a progress meter present. *)
val with_meter : f:(t -> 'a) -> 'a

(** Insert a meter to show 'scan' progress. *)
val scan_meter : t -> Node.t Sequence.t -> Node.t Sequence.t

(*

(** Set the function used to print the meter. *)
val set_meter : t -> f:(unit -> string) -> unit

(** Indicate to the meter that it might make sense to update. *)
val update : t -> unit

(** Print a message independent of the current meter. *)
val printf : ('a, Out_channel.t, unit) Stdlib.format -> 'a
*)
