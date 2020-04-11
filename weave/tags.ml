(** Weave Tags
 *
 * Every version stored in the weave has a set of tags associated with
 * it.  These are name/value pairs, and can be user specified.  There
 * are also pre-defined tags, namely "dir" -> "dir scanned" that
 * records the directory that was scanned.
 *
 * The tags are represented as alists in ocaml code, and as a
 * Yojson.Safe.t `Assoc.  These utilties convert in both directions.
 *)

open Core

type t = (string * string) list

let to_json items =
  `Assoc (List.map items ~f:(fun (k, v) -> (k, `String v)))

let from_json items =
  let items = match items with
    | `Assoc items -> items
    | _ -> failwith "Invalid JSON for tags" in
  List.map items ~f:(function
    | (k, `String v) -> (k, v)
    | _ -> failwith "Invalid Json for tags")

let from_equal text = match String.split ~on:'=' text with
  | (key::datas) -> (key, String.concat ~sep:"=" datas)
  | _ -> failwith "Expecting tag as key=value"
