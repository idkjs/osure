(* Weave tags *)

type t = (string * string) list

val to_json : t -> Yojson.Safe.t
val from_json : Yojson.Safe.t -> t
