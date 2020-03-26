(** Weave files will follow a naming convention.  This determines the
 * names of various temp files and other aspects.  The SCCS
 * conventions are not followed, because they are not safe (this code
 * will never write to a file that already exists). *)

open Core

type t = <
  new_temp : bool -> Stream.writer;
  main_file : string;
  backup_file : string;
  is_compressed : bool >

(* Open a reader for the main file. *)
let main_reader sn =
  let opener = if sn#is_compressed then Stream.gzip_in else Stream.open_in in
  opener sn#main_file

(* Open a reader, closing at the end. *)
let with_main_reader sn = Stream.with_in ~gzip:sn#is_compressed sn#main_file

(* Open a new temp file, for writing, and invoke the function to write
 * the contents, and then close the file.  Returns the filename at the
 * end. *)
let with_new_temp sn ?(compressed=true) ~f =
  let wr = sn#new_temp compressed in
  let name = wr#name in
  let res = try Ok (f wr) with
    | ex -> Error ex in
  wr#close;
  match res with
  | Ok result -> (name, result)
  | Error ex -> raise ex

(* The simple naming convention has a basename, with the main file
 * having a specified extension, the backup file having a ".bak"
 * extension, and the temp files using a numbered extension starting
 * with ".0".  If the names are intended to be compressed, a ".gz"
 * suffix can also be added.
 *)

let simple_naming ~path ~base ~ext ~compress =
  let make_name ext compressed =
    path ^/ sprintf "%s.%s%s" base ext (if compressed then ".gz" else "") in

  (* Construct a new output file. *)
  let temp_file this_compress =
    let make = if this_compress then Stream.create_out else Stream.gzip_out in
    let rec loop n =
      let name = make_name (Int.to_string n) this_compress in
      let fd = try Some (make name) with
        | Sys_error _ -> None in
      match fd with
        | None -> loop (n + 1)
        | Some fd -> fd in
    loop 0 in

  object
    method new_temp this_compress = temp_file this_compress
    method main_file = make_name ext compress
    method backup_file = make_name "bak" compress
    method is_compressed = compress
  end

let trial () =
  let sn = simple_naming ~path:"." ~base:"haha" ~ext:"dat" ~compress:true in
  let f1 = sn#new_temp true in
  printf "name: %s\n" f1#name
