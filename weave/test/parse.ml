(* Parse testing. *)

module Line_sink : (Weave.Parse.Sink with type t = string list) = struct
  type t = string list
  let insert x _ = x
  let delete x _ = x
  let ending x _ = x
  let plain lines text keep = if keep then text :: lines else lines
end

module Line_pusher = Weave.Parse.Pusher (Line_sink)
