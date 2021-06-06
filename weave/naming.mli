(* Naming conventions *)

type t = <
  new_temp : bool -> Stream.writer;
  main_file : string;
  backup_file : string;
  is_compressed : bool >

val simple_naming : path:string -> base:string -> ext:string -> compress:bool -> t

val main_reader : t -> Stream.reader
val with_main_reader : t -> f:(Stream.reader -> 'a) -> 'a
val with_new_temp : t -> ?compressed:bool -> f:(Stream.writer -> 'a) -> unit -> (string * 'a)

val trial : unit -> unit
