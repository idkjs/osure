(* Parse testing. *)

module Line_pusher : sig
  val run : Weave.Stream.reader -> delta:int -> ustate:(string list) -> string list
end

val run_test : unit -> unit
