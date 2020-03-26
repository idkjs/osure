(* Streaming weaves. *)

type writer = <
  write_lines : string list -> unit;
  close : unit;
  name : string >

type reader = <
  read_line : string option;
  close : unit;
  name : string >

val create_out : string -> writer
val open_in : string -> reader

val gzip_out : string -> writer
val gzip_in : string -> reader

val with_in : gzip:bool -> f:(reader -> 'a) -> string -> 'a
val with_out : gzip:bool -> f:(writer -> 'a) -> string -> 'a

(* Benchmark *)
val bulk_io : unit -> unit
