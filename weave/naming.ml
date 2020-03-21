(** Weave files will follow a naming convention.  This determines the
 * names of various temp files and other aspects.  The SCCS
 * conventions are not followed, because they are not safe (this code
 * will never write to a file that already exists). *)

open Core

(* The simple nameing convention has a basename, with the main file
 * having a specified extension, the backup file having a ".bak"
 * extension, and the temp files using a numbered extension starting
 * with ".0".  If the names are intended to be compressed, a ".gz"
 * suffix can also be added.
 *)

type simple_naming = {
  path : string;
  base : string;
  ext : string;
  compress : bool
}

let make_name sn ext compressed =
  sprintf "%s.%s%s" sn.base ext (if compressed then ".gz" else "")

(* Construct a new output file. *)
(* let temp_file sn compressed = () *)
