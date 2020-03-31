(* Parse testing. *)

open Core

module Line_sink : (Weave.Parse.Sink with type t = string list) = struct
  type t = string list
  let insert x _ = x
  let delete x _ = x
  let ending x _ = x
  let plain lines text keep = if keep then text :: lines else lines
end

module Line_pusher = Weave.Parse.Pusher (Line_sink)

(* Store that manages a weave *)

module Delta_store : Delta.STORE = struct
  type t = {
    ns : Weave.Naming.t;
    mutable index : int;
  }

  let make_tags t =
    let name = sprintf "delta #%d" t.index in
    t.index <- t.index + 1;
    [("key", "value"); ("name", name)]

  let make path =
    let ns = Weave.Naming.simple_naming ~path ~base:"ptest" ~ext:"dat" ~compress:true in
    { ns; index = 1 }

  let add_initial t ~f =
    let ((), delta) = Weave.Write.with_first_delta t.ns ~tags:(make_tags t) ~f in
    delta

  let add_delta t ~f =
    let ((), delta) = Weave.Write.with_new_delta t.ns ~tags:(make_tags t) ~f in
    delta

  let read_delta t ~delta =
    Weave.Naming.with_main_reader t.ns ~f:(fun rd ->
      Line_pusher.run rd ~delta ~ustate:[])
end

module Delta_tester = Delta.Tester (Delta_store)

let run_test () = Delta_tester.run ()
