(* SCCS based testing. *)

open Core

let sccs_check () =
  try
    let _ = Shell.run "sccs" ["--version"] in
    true
  with
    | Failure _ -> false

let simple_writer ofd : Weave.Stream.writer =
  object
    method write_lines lines = Out_channel.output_lines ofd lines
    method close = ()
    method name = failwith "No name support in test"
  end

module Sccs_store : Delta.STORE = struct
  type t = {
    tdir : string;
    plain : string;
    sfile : string;
    mutable delta : int;
  }

  let make tdir = {
    tdir;
    plain = tdir ^/ "tfile.dat";
    sfile = tdir ^/ "s.tfile.dat";
    delta = 0 }

  let next_delta s =
    let delta = s.delta + 1 in
    s.delta <- delta;
    delta

  let add_initial s ~f =
    Out_channel.with_file s.plain ~f:(fun ofd ->
      let wr = simple_writer ofd in
      f wr);
    Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["admin"; "-itfile.dat"; "-n"; "s.tfile.dat"];
    Unix.unlink s.plain;
    next_delta s

  (* TODO: factor this and the above. *)
  let add_delta s ~f =
    Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["get"; "-e"; "s.tfile.dat"];
    Out_channel.with_file s.plain ~f:(fun ofd ->
      let wr = simple_writer ofd in
      f wr);
    Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["delta"; "-yMessage"; "s.tfile.dat"];
    next_delta s

  (* Read a delta.  This uses the parser to test weaves reading. *)
  let read_delta s ~delta =
    let base = "s.tfile" in
    let path = s.tdir in
    let sn = Weave.Naming.simple_naming ~path ~base ~ext:"dat" ~compress:false in
    Weave.Naming.with_main_reader sn ~f:(fun rd ->
      Parse.Line_pusher.run rd ~delta ~ustate:[])

end

module Sccs_tester = Delta.Tester (Sccs_store)

let run_test () = Sccs_tester.run ()
