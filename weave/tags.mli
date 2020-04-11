(* Weave tags *)

type t = (string * string) list

val to_json : t -> Yojson.Safe.t
val from_json : Yojson.Safe.t -> t

(* Parse a "key=value" tag. *)
val from_equal : string -> (string * string)
