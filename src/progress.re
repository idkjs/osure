/* Generalized progress meter. */

open Core;

type meter = {
  /*** The last message printed. */
  mutable message: string,
  /*** Generate a new message from current data. */
  mutable update: unit => string,
  /*** When we expect to update the message. */
  mutable next_update: Time.t,
};
type t = meter;

let next_interval = time => Time.add(time, Time.Span.of_ms(250.0));

let clear = meter => {
  let count = 1 + String.count(meter.message, ~f=ch => Char.(ch == '\n'));
  for (_ in 1 to count) {
    printf("\027[1A\027[2K");
  };
  printf("%!");
};

let show_meter = meter => {
  let message = meter.update();
  clear(meter);
  meter.message = message;
  printf("%s\n%!", message);
};

/** Update the meter, possibly showing the message, if it is time. */

let update = (meter, ~f) => {
  meter.update = f;
  let curtime = Time.now();
  if (Time.(curtime >= meter.next_update)) {
    show_meter(meter);
    meter.next_update = next_interval(curtime);
  };
};

let with_meter = (~f) => {
  let meter = {
    message: "",
    update: () => "",
    next_update: next_interval(Time.now()),
  };
  let result = f(meter);
  show_meter(meter);
  result;
};

let units = ["B  ", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"];

let humanize_size = size => {
  let size = Int64.to_float(size);
  let precision = value =>
    if (Float.(value < 10.0)) {
      3;
    } else if (Float.(value < 100.0)) {
      2;
    } else {
      1;
    };
  let rec loop = size =>
    fun
    | [_, ...uus] when Float.(size > 1024.0) => loop(size /. 1024.0, uus)
    | [uu, ..._] => sprintf("%.*f%s", precision(size), size, uu)
    | _ => failwith("Absurd size overflow");
  loop(size, units);
};

module DirState = {
  type t = {
    dirs: int64,
    files: int64,
    total_bytes: int64,
  };
  let make = () => {dirs: 0L, files: 0L, total_bytes: 0L};
  let show = (meter, {dirs, files, total_bytes}) =>
    update(meter, ~f=() =>
      sprintf(
        "scan: %Ld dirs %Ld files, %s bytes",
        dirs,
        files,
        humanize_size(total_bytes),
      )
    );
};

let att_size = atts =>
  switch (Map.find(atts, "size")) {
  | None => 0L
  | Some(text) => Int64.of_string(text)
  };

let scan_meter = (meter, seq) => {
  let state0 = DirState.make();
  let last_state = ref(state0);
  Sequence.folding_map(
    seq,
    ~init=state0,
    ~f=(state, node) => {
      let state =
        switch (node) {
        | Node.Enter(_) => {...state, dirs: Int64.(state.dirs + 1L)}
        | [@implicit_arity] Node.File(_, atts) => {
            ...state,
            files: Int64.(state.files + 1L),
            total_bytes: Int64.(state.total_bytes + att_size(atts)),
          }
        | Node.Sep => state
        | Node.Leave =>
          DirState.show(meter, state);
          state;
        };

      last_state := state;
      (state, node);
    },
  );
};
