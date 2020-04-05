(* Nodes. *)

open Core

open Escape

type atts = string Map.M(String).t

let equal_atts ats = Map.equal (String.equal) ats

type t =
  | Enter of string * atts
  | File of string * atts
  | Sep
  | Leave
  [@@deriving eq]

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

let decoder line =
  let len = String.length line in

  (* Get a substring, up until the next space, skipping the space. *)
  let tospace pos =
    let rec loop p2 =
      if p2 >= len then failwith "Unexpected end of string";
      if Char.(line.[p2] = ' ') then
        (String.sub line ~pos ~len:(p2 - pos), p2 + 1)
      else
        loop (p2 + 1)
    in loop pos
  in

  (* Expect a specific character. *)
  let must pos ch =
    if pos >= len then failwith "Unexpected end of string";
    if Char.(line.[pos] <> ch) then failwith "Unexpected character";
    pos + 1 in

  (* Are we looking at a specific character? *)
  let isat pos ch =
    if pos >= len then failwith "Unexpected end of string";
    Char.(line.[pos] = ch) in

  let atend pos =
    if pos <> len then failwith "Unexpected text at end of string" in

  (* Start by fetching the name. *)
  let pos = 1 in
  let name, pos = tospace pos in
  let pos = must pos '[' in
  let rec loop atts pos =
    if isat pos ']' then begin
      atend (pos + 1);
      atts
    end else begin
      let key, pos = tospace pos in
      let data, pos = tospace pos in
      loop (Map.add_exn atts ~key ~data:(Escape.unescape data)) pos
    end in
  let atts = loop (Map.empty (module String)) pos in
  (Escape.unescape name, atts)

let parse text =
  let len = String.length text in
  if len = 0 then failwith "Invalid blank line";
  match len, text.[0] with
    | 1, '-' -> Sep
    | 1, 'u' -> Leave
    | _, 'f' -> let name, atts = decoder text in File (name, atts)
    | _, 'd' -> let name, atts = decoder text in Enter (name, atts)
    | _, _ ->
        eprintf "line: %S\n" text;
        failwith "Invalid input line"

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
        let targ = try Unix.readlink path
        with Unix.Unix_error _ ->
          printf "Warning: Unable to readlink %S\n" path;
          "???"
        in
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
