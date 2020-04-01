(* Nodes. *)

open Core

open Escape

type atts = (string, string, String.comparator_witness) Map.t

type t =
  | Enter of string * atts
  | File of string * atts
  | Sep
  | Leave

let build kind name atts =
  let buf = Buffer.create 32 in
  Buffer.add_char buf kind;
  Buffer.add_string buf (escape name);
  Buffer.add_string buf " [";
  Map.iteri atts ~f:(fun ~key ~data ->
    Buffer.add_string buf key;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (escape data);
    Buffer.add_char buf ' ');
  Buffer.add_char buf ']';
  Buffer.contents buf

let show = function
  | Enter (name, atts) -> build 'd' name atts
  | File (name, atts) -> build 'f' name atts
  | Leave -> "u"
  | Sep -> "-"

(* Attribute conversions to strings. *)
let of_int = Int.to_string
let of_int64 = Int64.to_string

(* Add time info. *)
let time_info atts (stat : Unix.stats) =
  (* Just use the integer part of time. *)
  let atts = Map.add_exn atts ~key:"mtime" ~data:(
    of_int64 (Float.to_int64 stat.st_mtime)) in
  let atts = Map.add_exn atts ~key:"ctime" ~data:(
    of_int64 (Float.to_int64 stat.st_ctime)) in
  atts

(* Add device info.  This is defined in a macro, not easily
 * accessible. It is at least unlikely to change on Linux, but might
 * on other platforms. *)
let add_dev atts (stat : Unix.stats) =
  let atts = Map.add_exn atts ~key:"devmaj" ~data:(
    of_int ((stat.st_rdev lsr 8) land 0xfff)) in
  let atts = Map.add_exn atts ~key:"devmin" ~data:(
    of_int (stat.st_rdev land 0xff)) in
  atts

(* Convert attributes, based on file type.  Uses the path to resolve
 * symlinks. *)
let stat_to_atts path (stat : Unix.stats) =
  let atts = Map.empty (module String) in
  let atts = Map.add_exn atts ~key:"uid" ~data:(of_int stat.st_uid) in
  let atts = Map.add_exn atts ~key:"gid" ~data:(of_int stat.st_gid) in
  let atts = Map.add_exn atts ~key:"perm" ~data:(of_int stat.st_perm) in
  let atts = match stat.st_kind with
    | Unix.S_DIR ->
        Map.add_exn atts ~key:"kind" ~data:"dir"
    | Unix.S_REG ->
        let atts = Map.add_exn atts ~key:"kind" ~data:"file" in
        let atts = Map.add_exn atts ~key:"ino" ~data:(of_int stat.st_ino) in
        let atts = Map.add_exn atts ~key:"size" ~data:(of_int64 stat.st_size) in
        time_info atts stat
    | Unix.S_LNK ->
        let atts = Map.add_exn atts ~key:"kind" ~data:"lnk" in
        let targ = try
          Unix.readlink path
        with Unix.Unix_error _ -> "???" in
        let atts = Map.add_exn atts ~key:"targ" ~data:targ in
        atts
    | Unix.S_FIFO -> Map.add_exn atts ~key:"kind" ~data:"fifo"
    | Unix.S_SOCK -> Map.add_exn atts ~key:"kind" ~data:"sock"
    | Unix.S_CHR ->
        let atts = Map.add_exn atts ~key:"kind" ~data:"chr" in
        add_dev atts stat
    | Unix.S_BLK ->
        let atts = Map.add_exn atts ~key:"kind" ~data:"blk" in
        add_dev atts stat
  in atts
