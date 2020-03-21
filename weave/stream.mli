(* Streaming weaves. *)

type writer = <
  write_lines : string list -> unit;
  close : unit;
  to_name : string >

type reader = <
  read_line : string option;
  close : unit;
  to_name : string >

val create_out : string -> writer
val open_in : string -> reader

(* Benchmark *)
val bulk_io : unit -> unit
