(* Scanning *)

open Core

module Internal : sig

  (** Turn a fetching function into a sequence.  Sequences don't require
   * that the result be evaluated in order, so we have to put a little
   * work into making sure this works (using memoize). *)
  val to_seq : (unit -> 'a option) -> 'a Sequence.t

  (** Augment the node coming from a sure tree to track the full path. *)
  val track_path : string -> Node.t Sequence.t -> (Node.t * string) Sequence.t

  (** Determine if the given node should have a hash, but is missing
   * one.  The node must be a "file" not, with a kind of "file", and not
   * already have a sha1 attribute. *)
  val needs_hash : Node.t -> bool

end = struct

  let to_seq f =
    Sequence.unfold ~init:f ~f:(fun f ->
      Option.map (f ()) ~f:(fun node -> (node, f)))
    |> Sequence.memoize

  (** Reverse and combine the components into a path. *)
  let revcombine = function
    | [] -> "."
    | elts -> List.reduce_exn (List.rev elts) ~f:(^/)

  let track_path root seq =
    Sequence.folding_map seq ~init:([root], root) ~f:(fun (elts, path) node ->
      match node with
        | Node.Enter ("__root__", _) when List.length elts = 1 ->
            (* The root directory is a special case, we use the passed
             * in name instead of the "__root__" that will be in the
             * node.  There isn't anything preventing the name
             * "__root__" from occurring elsewhere, but it saves us from
             * having to compute the element list length every time. *)
            ((elts, path), (node, path))
        | Node.Enter (name, _) ->
            let elts = name :: elts in
            let path = revcombine elts in
            ((elts, path), (node, path))
        | Leave ->
            let elts = match elts with
              | [] -> failwith "Too many Leave nodes"
              | (_::elts) -> elts in
            let path = revcombine elts in
            ((elts, path), (node, path))
        | Sep ->
            ((elts, path), (node, path))
        | File (name, _) ->
            ((elts, path), (node, path ^/ name)))

  let needs_hash = function
    | Node.Enter _ | Leave | Sep -> false
    | File (_, atts) ->
        begin match Map.find atts "kind" with
          | Some "file" -> not (Map.mem atts "sha1")
          | _ -> false
        end
end

open Internal

let hash_update path db prior =
  printf "hash update\n";
  let nodes = to_seq prior in
  let nodes = track_path path nodes in
  Db.with_xact db ~f:(fun db ->
    let stmt = Sqlite3.prepare db "INSERT INTO hashes VALUES (?, ?)" in
    Sequence.iteri nodes ~f:(fun index (node, path) ->
      if needs_hash node then begin
        (*printf "Node: %s\n" (Node.show node);*)
        (*printf "  %S\n" path;*)
        let hash = try Some (Sha1.hash_file path) with
          | _ex ->
              eprintf "Warning: error hashing %S\n" path;
              None
        in
        (*printf "  %s\n" hash;*)

        Option.iter hash ~f:(fun hash ->
          Sqlite3.bind_int stmt 1 index |> Sqlite3.Rc.check;
          Sqlite3.bind_blob stmt 2 hash |> Sqlite3.Rc.check;
          begin match Sqlite3.step stmt with
            | DONE -> ()
            | err -> failwith (Sqlite3.Rc.to_string err)
          end;
          Sqlite3.reset stmt |> Sqlite3.Rc.check)
      end);
    Sqlite3.finalize stmt |> Sqlite3.Rc.check)

(** Update the hash within a node. *)
let node_newhash node newhash =
  let newhash = Sha1.hexlify newhash in
  match node with
    | Node.File (name, atts) ->
        Node.File (name, Map.set atts ~key:"sha1" ~data:newhash)
    | _ -> failwith "Node sequence error"

let merge_hashes elts rnode wnode =
  let input = to_seq rnode in
  let input = Sequence.mapi input ~f:(fun index node -> (index, node)) in
  let rec loop = function
    | (None, None) -> ()
    | (Some (_, anext), None) -> loop (Sequence.next anext, None)
    | (None, Some ((_, bnode), bnext)) ->
        wnode bnode;
        loop (None, Sequence.next bnext)
    | (Some ((aindex, aelt), anext) as aa, (Some ((bindex, belt), bnext) as bb)) ->
        if aindex < bindex then
          loop (Sequence.next anext, bb)
        else if aindex > bindex then begin
          wnode belt;
          loop (aa, Sequence.next bnext)
        end else begin
          (* Update *)
          let belt = node_newhash belt aelt in
          wnode belt;
          loop (Sequence.next anext, Sequence.next bnext)
        end
  in
  loop (Sequence.next elts, Sequence.next input)
