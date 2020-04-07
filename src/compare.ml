(* Comparators. *)

open Core

(* The comparison should never get to the end of the node data. *)
let next gen =
  match gen () with
    | None -> failwith "Erroneous end of file"
    | Some x -> x

(* Ensure that we are looking at a directory.  If so, return the name
 * and atts from it. *)
let must_dir = function
  | Node.Enter (name, atts) -> name, atts
  | _ -> failwith "Unexpected node type"

(* Report on a missing attribute.  We store the attributes we've seen
 * so we only will warn/report one time.  TODO: This should be logged
 * instead of just printed. *)
let missings = ref (Set.empty (module String))
let missing att side =
  let name = sprintf "%s/%s" att side in
  if not (Set.mem !missings name) then begin
    printf "Attribute [%s] missing from %s\n" att side;
    missings := Set.add !missings name
  end

(* Compare attributes from two entities. *)
let attcmp aatts batts name =
  let rec walk diffs = function
    | ([], []) -> diffs
    | ((ak, _) :: aa, []) -> missing ak "prior"; walk diffs (aa, [])
    | ([], (bk, _) :: bb) -> missing bk "current"; walk diffs ([], bb)
    | ((ak, _) :: aa, ((bk, _) :: _ as bb)) when String.(ak < bk) ->
        missing ak "prior"; walk diffs (aa, bb)
    | ((ak, _) :: _ as aa, ((bk, _) :: bb)) when String.(ak > bk) ->
        missing bk "current"; walk diffs (aa, bb)
    | (("ino", _) :: aa, _ :: bb) -> walk diffs (aa, bb)
    | (("ctime", _) :: aa, _ :: bb) -> walk diffs (aa, bb)
    | ((ak, av) :: aa, (_, bv) :: bb) ->
        if String.(av <> bv) then
          walk (ak::diffs) (aa, bb)
        else
          walk diffs (aa, bb)
  in
  let aatts = Map.to_alist aatts in
  let batts = Map.to_alist batts in
  let diffs = walk [] (aatts, batts) in
  if not (List.is_empty diffs) then begin
    let buf = Buffer.create 20 in
    List.iter (List.rev diffs) ~f:(fun name ->
      if Buffer.length buf > 0 then
        bprintf buf ",";
      bprintf buf "%s" name);
    printf "  [%-20s] %S\n" (Buffer.contents buf) name
  end

let print_alone change kind name =
  printf "%c %-20s   %S\n" change kind name

let compare agen bgen =

  let rec subdir dir =
    children dir (next agen, next bgen)
  and children dir = function
    | (Node.Sep, Node.Sep) -> files dir (next agen, next bgen)
    | (Enter (aname, _), Sep) ->
        print_alone '-' "dir" (dir ^/ aname); 
        consume agen;
        children dir (next agen, Sep)
    | (Sep, Enter (bname, _)) ->
        print_alone '+' "dir" (dir ^/ bname);
        consume bgen;
        children dir (Sep, next bgen)
    | (Enter (aname, _), (Enter (bname, _) as bb)) when String.(aname < bname) ->
        print_alone '-' "dir" (dir ^/ aname);
        consume agen;
        children dir (next agen, bb)
    | (Enter (aname, _) as aa, Enter (bname, _)) when String.(aname > bname) ->
        print_alone '+' "dir" (dir ^/ bname);
        consume bgen;
        children dir (aa, next bgen)
    | (Enter (aname, aatts), Enter (_, batts)) ->
        attcmp aatts batts (dir ^/ aname);
        subdir (dir ^/ aname);
        children dir (next agen, next bgen)
    | _ -> failwith "Invalid directory structure"
  and files dir = function
    | (Leave, Leave) -> ()
    | (File (aname, _), Leave) ->
        print_alone '-' "file" (dir ^/ aname);
        files dir (next agen, Leave)
    | (Leave, File (bname, _)) ->
        print_alone '+' "file" (dir ^/ bname);
        files dir (Leave, next bgen)
    | (File (aname, _), (File (bname, _) as bb)) when String.(aname < bname) ->
        print_alone '-' "file" (dir ^/ aname);
        files dir (next agen, bb)
    | (File (aname, _) as aa, File (bname, _)) when String.(aname > bname) ->
        print_alone '+' "file" (dir ^/ bname);
        files dir (aa, next bgen)
    | (File (aname, aatts), File (_, batts)) ->
        attcmp aatts batts (dir ^/ aname);
        files dir (next agen, next bgen)
    | _ -> failwith "Invalid file structure"
  and consume gen =
    let node = next gen in
    (* printf "consume: %s\n" (Node.show node); *)
    match node with
      | Node.Leave -> ()
      | Sep | File _ -> consume gen
      | Enter _ -> consume gen; consume gen
  in

  (* Both must start with an Enter node, for this to be meaningful. *)
  let a = next agen in
  let b = next bgen in

  let aname, aatts = must_dir a in
  let bname, batts = must_dir b in
  if String.(aname <> bname) then failwith "Root directories have differing names";
  attcmp aatts batts ".";
  subdir "."
