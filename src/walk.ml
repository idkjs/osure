(* Walk a directory into a sequence. *)

open Core

module Se = Sequence

let scan_dir path =
  let dirh = Unix.opendir path in
  let rec loop dirs files =
    match Unix.readdir_opt dirh with
      | None -> (dirs, files)
      | Some "." | Some ".." -> loop dirs files
      | Some name ->
          let sbuf = Unix.lstat @@ path ^/ name in
          begin match sbuf.st_kind with
            | Unix.S_DIR ->
                loop ((name, sbuf) :: dirs) files
            | _ ->
                loop dirs ((name, sbuf) :: files)
          end in
  let dirs, files = loop [] [] in
  Unix.closedir dirh;
  let comp (a, _) (b, _) = String.compare a b in
  let dirs = List.sort ~compare:comp dirs in
  let files = List.sort ~compare:comp files in
  (dirs, files)

let walk path =
  let root_stat = Unix.lstat path in
  let open Se.Generator in
  let rec walk path name stat =
    (* printf "Walk: %s\n" path; *)
    let dirs, files = scan_dir path in
    yield @@ Node.Enter (name, Node.stat_to_atts path stat) >>= fun () ->
    subdirs path dirs >>= fun () ->
    yield Node.Sep >>= fun () ->
    subfiles path files >>= fun () ->
    yield @@ Node.Leave
  and subdirs path = function
    | [] -> return ()
    | ((name, stat) :: xs) ->
        walk (path ^/ name) name stat >>= fun () ->
        subdirs path xs
  and subfiles path = function
    | [] -> return ()
    | ((name, stat) :: xs) ->
        yield @@ Node.File (name, Node.stat_to_atts path stat) >>= fun () ->
        subfiles path xs
  in
  run @@ walk path "__root__" root_stat
