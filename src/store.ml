(* Store *)

open Core

type t = {
  ns : Weave.Naming.t;
}

let show st = sprintf "{ns=%S}" st.ns#main_file

let listing st =
  let header = Weave.Naming.with_main_reader st.ns ~f:(fun rd ->
    Weave.Parse.read_header rd) in
  let header = Weave.Header_j.header_of_string header in
  printf "vers | Time captured                  | name\n";
  printf "-----+--------------------------------+---------------------\n";
  List.iter header.deltas ~f:(fun ver ->
    printf "% 4d | %s | %s\n" ver.number ver.time ver.name)

let parse file =
  let dir, file = Filename.split file in
  printf "dir=%S file=%S\n" dir file;
  if not String.(suffix file 7 = ".dat.gz") then
    failwith "Currently, filename must end in \".dat.gz\"";
  let base = String.drop_suffix file 7 in
  let ns = Weave.Naming.simple_naming ~path:dir ~base ~ext:"dat" ~compress:true in
  { ns }
