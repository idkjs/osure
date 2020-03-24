(* Naming conventions *)

type t = <
  new_temp : bool -> Stream.writer;
  main_file : string;
  backup_file : string;
  is_compressed : bool >

val simple_naming : path:string -> base:string -> ext:string -> compress:bool -> t

val main_reader : t -> Stream.reader

val trial : unit -> unit
