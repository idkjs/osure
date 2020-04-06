open Core
open Osure

module SureFile = struct
  type t = {
    dir : string;
    file : string;
    store : Store.t;
  }
  let show { dir; file; store } =
    sprintf "{dir=%S;file=%S;store=%s}" dir file (Store.show store)

  let t_param =
    let open Command.Let_syntax in
    let%map_open dir = flag "--dir" ~aliases:["-d"] (optional string)
      ~doc:"Directory to scan, defaults to \".\""
    and file = flag "--file" ~aliases:["-f"] (optional string)
      ~doc:"Filename for surefile, defaults to 2sure.dat.gz" in
    let dir = Option.value dir ~default:"." in
    let file = Option.value file ~default:"2sure.dat.gz" in
    let store = Store.parse file in
    { dir; file; store }
end

let list_act (sfile : SureFile.t) =
  Store.listing sfile.store

let scan_or_update op (sfile : SureFile.t) =
  let dbname, _ = Store.with_temp_db sfile.store ~f:(fun db ->
    Db.make_hash_schema db;

    (* Scan the filesystem (without hashes). *)
    let scan_temp, _ = Store.with_temp sfile.store ~f:(fun wnode ->
      Progress.with_meter ~f:(fun meter ->
        let elts = Walk.walk sfile.dir in
        let elts = Progress.scan_meter meter elts in
        Sequence.iter elts ~f:(fun elt -> wnode elt))
    ) in

    (* Update the hash from the latest revision, if there is one. *)
    let hashed_file, rdfile = match op with
      | `Update ->
        let hashed_file, _ = Store.with_temp sfile.store ~f:(fun whashed ->
          Store.with_rev sfile.store `Latest ~f:(fun older ->
            Store.with_temp_in scan_temp ~gzip:false ~f:(fun latest ->
              Scan.cp_hashes ~older ~latest whashed
            )
          )
        ) in
        (Some hashed_file, hashed_file)
      | `Scan -> None, scan_temp in

    Progress.with_meter ~f:(fun meter ->
      let hstate = Store.with_temp_in rdfile ~gzip:false ~f:(fun rnode ->
        Scan.hash_count ~meter rnode) in
      Store.with_temp_in rdfile ~gzip:false ~f:(fun rnode ->
        Scan.hash_update ~hstate sfile.dir db rnode)
    );

    (* Retrieve the hashes *)
    let wither = match op with
      | `Update -> Store.with_added_delta
      | `Scan -> Store.with_first_delta in
    let _, delta = wither sfile.store ~f:(fun wnode ->
      Db.with_hashes db ~f:(fun elts ->
        Store.with_temp_in rdfile ~gzip:false ~f:(fun rnode ->
          Scan.merge_hashes elts rnode wnode)))
    in
    printf "New delta: %d\n%!" delta;

    Option.iter hashed_file ~f:Unix.unlink;
    Unix.unlink scan_temp
  ) in
  Unix.unlink dbname

let scan_act (sfile : SureFile.t) =
  scan_or_update `Scan sfile

let update_act (sfile : SureFile.t) =
  scan_or_update `Update sfile

let check_act (sfile : SureFile.t) =
  printf "check: %s\n" (SureFile.show sfile)

let signoff_act (sfile : SureFile.t) =
  printf "signoff: %s\n" (SureFile.show sfile);
  Store.with_rev sfile.store `Previous ~f:(fun prior ->
    Store.with_rev sfile.store `Latest ~f:(fun current ->
      Compare.compare prior current))

let general act summary =
  Command.basic ~summary
    begin
      let open Command.Let_syntax in
      let%map_open sfile = SureFile.t_param in
      fun () -> act sfile
    end

let () =
  (* let open Command.Let_syntax in *)
  Command.group
    ~summary:"Rsure"
    [("scan", general scan_act "Scan a directory for the first time");
      ("update", general update_act "Update the scan using the dat file");
      ("list", general list_act "List revisions in a given sure store");
      ("check", general check_act "Compare the directory with the dat file");
      ("signoff", general signoff_act "Compare the last two version in dat file") ]
  |> Command.run

let nothing () =
  Sequence.iter (Walk.walk ".") ~f:(fun e ->
    print_endline @@ Node.show e)

let bench_stream () = Weave.Stream.bulk_io ()

(* Read a large file to get an idea of timing. *)
let bench_gzip_in () =
  let fd = Weave.Stream.gzip_in "/home/2sure.dat.gz" in
  let rec loop count =
    match fd#read_line with
      | Some _ -> loop (count + 1)
      | None -> count in
  let total = loop 0 in
  fd#close;
  printf "%d lines\n" total

let naming _ =
  Weave.Naming.trial ()

let sample () =
  Weave.Parse.sample ()

module Load_sink : (Weave.Parse.Sink with type t = string list) = struct
  type t = string list
  let insert xs _ = xs
  let delete xs _ = xs
  let ending xs _ = xs
  let plain xs text keep = if keep then text :: xs else xs
end

module Count_sink : (Weave.Parse.Sink with type t = int) = struct
  type t = int
  let insert xs _ = xs
  let delete xs _ = xs
  let ending xs _ = xs
  let plain xs _ keep = if keep then xs + 1 else xs
end

module Loader = Weave.Parse.Pusher (Load_sink)
module Counter = Weave.Parse.Pusher (Count_sink)

(* Try reading in my largest sure file for some benchmarking ideas. *)
let benchy () =
  let sn = Weave.Naming.simple_naming ~path:"/home" ~base:"2sure" ~ext:"dat" ~compress:true in
  let header = Weave.Naming.with_main_reader sn ~f:Weave.Parse.read_header in
  let header = Weave.Header_j.header_of_string header in
  (* printf "%S\n" (Weave.Header_j.string_of_header header) *)
  let deltas = List.take header.deltas 4 in
  List.iter deltas ~f:(fun ver ->
    printf "Version %d %S\n" ver.number ver.name;
    Out_channel.flush stdout;
    (* *)
    let lines = Weave.Naming.with_main_reader sn ~f:(fun rd ->
      Loader.run rd ~delta:ver.number ~ustate:[]) in
    printf "   There are %d lines\n" (List.length lines);
    (* *)
    (*
    let lines = Weave.Naming.with_main_reader sn ~f:(fun rd ->
      Counter.run rd ~delta:ver.number ~ustate:0) in
    printf "   There are %d lines\n" lines;
    Out_channel.flush stdout
    *)
  )

(*
let _ =
  Weave.Write.sample ()
*)
