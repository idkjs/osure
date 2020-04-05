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

let scan_act (sfile : SureFile.t) =
  printf "scan: %s\n%!" (SureFile.show sfile);
  let dbname, _ = Store.with_temp_db sfile.store ~f:(fun db ->
    Db.make_hash_schema db;
    let tname, _ = Store.with_temp sfile.store ~f:(fun wnode ->
      Progress.with_meter ~f:(fun meter ->
        let elts = Walk.walk sfile.dir in
        let elts = Progress.scan_meter meter elts in
        Sequence.iter elts ~f:(fun elt -> wnode elt))) in
    (* printf "tempy name: %s\n" tname; *)
    printf "Updating hashes\n%!";
    Store.with_temp_in tname ~gzip:false ~f:(fun rnode ->
      Scan.hash_update sfile.dir db rnode);

    (* Retrieve the hashes. *)
    let _, delta = Store.with_first_delta sfile.store ~f:(fun wnode ->
      Db.with_hashes db ~f:(fun elts ->
        Store.with_temp_in tname ~gzip:false ~f:(fun rnode ->
          Scan.merge_hashes elts rnode wnode)))
    in
    Unix.unlink tname;
    printf "new delta %d\n" delta
    ) in
  (* printf "db: %s\n" dbname; *)
  Unix.unlink dbname

let update_act (sfile : SureFile.t) =
  printf "update: %s\n" (SureFile.show sfile)

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

(* TODO: Use atdgen to build real structures out of the json data.
 * For now, just use this awful parser. *)
(*
let assoc_get json key =
  let rec lookup = function
    | (k,v)::xs ->
        if String.(=) k key then v
        else lookup xs
    | [] -> failwith "Key not present" in
  match json with
    | `Assoc xs -> lookup xs
    | _ -> failwith "Unexpected json"

module Version = struct
  type t = {
    name : string;
    number : int;
    (*tags : string list;
    time : string*)
  }

  let from_json json =
    let name = match assoc_get json "name" with
      | `String text -> text
      | _ -> failwith "Unexpected json" in
    let number = match assoc_get json "number" with
      | `Int num -> num
      | _ -> failwith "Unexpected json" in
    { name; number }
end
*)

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
