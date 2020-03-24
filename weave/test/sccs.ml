(* SCCS based testing. *)

open Core
module RS = Random.State

let sccs_check () =
  try
    let _ = Shell.run "sccs" ["--version"] in
    true
  with
    | Failure _ -> false

let nums_size = 1_000
let num_versions = 100

type sccs = {
  tdir : string;
  plain : string;
  sfile : string;
  mutable nums : int array list;
  rng : RS.t
}

let make_sccs () =
  let tdir = Filename.temp_dir ~in_dir:"/var/tmp" "weave" "" in
  let plain = tdir ^/ "tfile.dat" in
  let sfile = tdir ^/ "s.tfile.dat" in
  let nums = [Array.init nums_size ~f:(fun x -> x + 1)] in
  let rng = RS.make [|1;2;3|] in
  { tdir; plain; sfile; nums; rng }

let with_sccs ~f =
  let s = make_sccs () in
  let ex = try Ok (f s) with
    | ex -> Error ex in
  if Option.is_none (Sys.getenv "WEAVE_KEEP") then
    FileUtil.rm ~recurse:true [s.tdir];
  match ex with
    | Ok result -> result
    | Error ex -> raise ex

(* Write the contents of the nums to the tfile. *)
let write_tfile s =
  Out_channel.with_file s.plain ~f:(fun ofd ->
    Array.iter (List.hd_exn s.nums) ~f:(fun num -> fprintf ofd "%d\n" num))

let dup_nums s =
  s.nums <- Array.copy (List.hd_exn s.nums) :: s.nums

(* A simple pseudorandom shuffle of the numbers. *)
let shuffle s =
  dup_nums s;
  let top = List.hd_exn s.nums in
  let len = Array.length top in
  let a = RS.int s.rng len in
  let b = RS.int s.rng len in
  let a, b = if a > b then b, a else a, b in
  let rec loop a b =
    if a < b then begin
      Array.swap top a b;
      loop (a+1) (b-1)
    end in loop a b

let version1 s =
  write_tfile s;
  Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["admin"; "-itfile.dat"; "-n"; "s.tfile.dat"];
  Unix.unlink s.plain

let versionn s =
  Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["get"; "-e"; "s.tfile.dat"];
  write_tfile s;
  Shell.run ~working_dir:s.tdir ~echo:false "sccs" ["delta"; "-yMessage"; "s.tfile.dat"]

(* Use the weave parser to validate the versions that we have. *)
(* The nums are in reverse order of the versions, starting with the
 * largest number. *)
let validate s =
  let count = List.length s.nums in
  List.iteri s.nums ~f:(fun i nums ->
    let i = count - i in
    Weave.Parse.test_check s.sfile i nums)

let run_test () =
  with_sccs ~f:(fun s ->
    printf "tdir: %S\n" s.tdir;
    version1 s;
    for _ = 2 to num_versions do
      shuffle s;
      versionn s
    done;
    validate s)

let _ =
  printf "SCCS test";
  let present = sccs_check () in
  printf "Sccs present: %b\n" present;
  if present then run_test ()
