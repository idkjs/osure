(* Store *)

open Core

type t = {
  ns : Weave.Naming.t;
}

let show st = sprintf "{ns=%S}" st.ns#main_file

let read_header st =
  let header = Weave.Naming.with_main_reader st.ns ~f:(fun rd ->
    Weave.Parse.read_header rd) in
  Weave.Header_j.header_of_string header

let listing st =
  let header = read_header st in
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

type revision =
  [ `Latest
  | `Previous
  | `Num of int ]

let get_rev st rev =
  let header = read_header st in
  let deltas = List.rev header.deltas in
  match rev with
    | `Latest -> List.nth deltas 0
    | `Previous -> List.nth deltas 1
    | `Num n -> List.find ~f:(fun x -> x.number = n) deltas

let must state expect =
  match Weave.Parse.Puller.pull_plain state with
    | None -> failwith "Unexpected end of input"
    | Some text when String.(text = expect) -> ()
    | Some text -> failwith (sprintf "Unexpected line: %S" text)

let rd_must rd expect =
  match rd#read_line with
    | None -> failwith "Unexpected end of input"
    | Some text when String.(text = expect) -> ()
    | Some text -> failwith (sprintf "Unexpected line: %S" text)

let with_rev st rev ~f =
  let rev = get_rev st rev in
  let rev = Option.value_exn rev in
  let delta = rev.number in
  Weave.Naming.with_main_reader st.ns ~f:(fun rd ->
    let state = ref (Weave.Parse.Puller.make rd ~delta) in
    must state "asure-2.0";
    must state "-----";
    f (fun () -> Option.map (Weave.Parse.Puller.pull_plain state) ~f:Node.parse))

let with_temp st ~f =
  Weave.Naming.with_new_temp st.ns ~compressed:false ~f:(fun wr ->
    wr#write_lines ["asure-2.0"; "-----"];
    f (fun node -> wr#write_lines [Node.show node]))

let with_temp_in fname ~gzip ~f =
  Weave.Stream.with_in fname ~gzip ~f:(fun rd ->
    rd_must rd "asure-2.0";
    rd_must rd "-----";
    f (fun () -> Option.map rd#read_line ~f:Node.parse))

let with_temp_db st ~f =
  Weave.Naming.with_new_temp st.ns ~compressed:false ~f:(fun wr ->
    wr#close;
    let name = wr#name in
    let db = Sqlite3.db_open ~mutex:`FULL name in
    let result = try Ok (f db) with
      | ex -> Error ex in
    let _ = Sqlite3.db_close db in (* TODO warn if can't close *)
    match result with
      | Ok result -> result
      | Error ex -> raise ex)

let with_first_delta ?(tags=[]) st ~f =
  Weave.Write.with_first_delta st.ns ~tags ~f:(fun wr ->
    wr#write_lines ["asure-2.0"; "-----"];
    f (fun node -> wr#write_lines [Node.show node]))

let with_added_delta ?(tags=[]) st ~f =
  Weave.Write.with_new_delta st.ns ~tags ~f:(fun wr ->
    wr#write_lines ["asure-2.0"; "-----"];
    f (fun node -> wr#write_lines [Node.show node]))
