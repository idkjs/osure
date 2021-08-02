/* Nodes. */

open Core;

open Escape;

type atts = Map.M(String).t(string);

let equal_atts = ats => Map.equal(String.equal, ats);

[@deriving eq]
type t =
  | Enter(string, atts)
  | File(string, atts)
  | Sep
  | Leave;

let build = (kind, name, atts) => {
  let buf = Buffer.create(32);
  Buffer.add_char(buf, kind);
  Buffer.add_string(buf, escape(name));
  Buffer.add_string(buf, " [");
  Map.iteri(
    atts,
    ~f=(~key, ~data) => {
      Buffer.add_string(buf, key);
      Buffer.add_char(buf, ' ');
      Buffer.add_string(buf, escape(data));
      Buffer.add_char(buf, ' ');
    },
  );
  Buffer.add_char(buf, ']');
  Buffer.contents(buf);
};

let show =
  fun
  | [@implicit_arity] Enter(name, atts) => build('d', name, atts)
  | [@implicit_arity] File(name, atts) => build('f', name, atts)
  | Leave => "u"
  | Sep => "-";

let decoder = line => {
  let len = String.length(line);

  /* Get a substring, up until the next space, skipping the space. */
  let tospace = pos => {
    let rec loop = p2 => {
      if (p2 >= len) {
        failwith("Unexpected end of string");
      };
      if (Char.(line.[p2] == ' ')) {
        (String.sub(line, ~pos, ~len=p2 - pos), p2 + 1);
      } else {
        loop(p2 + 1);
      };
    };
    loop(pos);
  };

  /* Expect a specific character. */
  let must = (pos, ch) => {
    if (pos >= len) {
      failwith("Unexpected end of string");
    };
    if (Char.(line.[pos] != ch)) {
      failwith("Unexpected character");
    };
    pos + 1;
  };

  /* Are we looking at a specific character? */
  let isat = (pos, ch) => {
    if (pos >= len) {
      failwith("Unexpected end of string");
    };
    Char.(line.[pos] == ch);
  };

  let atend = pos =>
    if (pos != len) {
      failwith("Unexpected text at end of string");
    };

  /* Start by fetching the name. */
  let pos = 1;
  let (name, pos) = tospace(pos);
  let pos = must(pos, '[');
  let rec loop = (atts, pos) =>
    if (isat(pos, ']')) {
      atend(pos + 1);
      atts;
    } else {
      let (key, pos) = tospace(pos);
      let (data, pos) = tospace(pos);
      loop(Map.add_exn(atts, ~key, ~data=Escape.unescape(data)), pos);
    };
  let atts = loop(Map.empty((module String)), pos);
  (Escape.unescape(name), atts);
};

let parse = text => {
  let len = String.length(text);
  if (len == 0) {
    failwith("Invalid blank line");
  };
  switch (len, text.[0]) {
  | (1, '-') => Sep
  | (1, 'u') => Leave
  | (_, 'f') =>
    let (name, atts) = decoder(text);
    [@implicit_arity] File(name, atts);
  | (_, 'd') =>
    let (name, atts) = decoder(text);
    [@implicit_arity] Enter(name, atts);
  | (_, _) =>
    eprintf("line: %S\n", text);
    failwith("Invalid input line");
  };
};

/* Attribute conversions to strings. */
let of_int = Int.to_string;
let of_int64 = Int64.to_string;

/* Add time info. */
let time_info = (atts, stat: Unix.stats) => {
  /* Just use the integer part of time. */
  let atts =
    Map.add_exn(
      atts,
      ~key="mtime",
      ~data=of_int64(Float.to_int64(stat.st_mtime)),
    );
  let atts =
    Map.add_exn(
      atts,
      ~key="ctime",
      ~data=of_int64(Float.to_int64(stat.st_ctime)),
    );
  atts;
};

/* Add device info.  This is defined in a macro, not easily
 * accessible. It is at least unlikely to change on Linux, but might
 * on other platforms. */
let add_dev = (atts, stat: Unix.stats) => {
  let atts =
    Map.add_exn(
      atts,
      ~key="devmaj",
      ~data=of_int(stat.st_rdev lsr 8 land 0xfff),
    );
  let atts =
    Map.add_exn(atts, ~key="devmin", ~data=of_int(stat.st_rdev land 0xff));
  atts;
};

/* Convert attributes, based on file type.  Uses the path to resolve
 * symlinks. */
let stat_to_atts = (path, stat: Unix.stats) => {
  let atts = Map.empty((module String));
  let atts = Map.add_exn(atts, ~key="uid", ~data=of_int(stat.st_uid));
  let atts = Map.add_exn(atts, ~key="gid", ~data=of_int(stat.st_gid));
  let atts = Map.add_exn(atts, ~key="perm", ~data=of_int(stat.st_perm));
  let atts =
    switch (stat.st_kind) {
    | Unix.S_DIR => Map.add_exn(atts, ~key="kind", ~data="dir")
    | Unix.S_REG =>
      let atts = Map.add_exn(atts, ~key="kind", ~data="file");
      let atts = Map.add_exn(atts, ~key="ino", ~data=of_int(stat.st_ino));
      let atts =
        Map.add_exn(atts, ~key="size", ~data=of_int64(stat.st_size));
      time_info(atts, stat);
    | Unix.S_LNK =>
      let atts = Map.add_exn(atts, ~key="kind", ~data="lnk");
      let targ =
        try(Unix.readlink(path)) {
        | Unix.Unix_error(_) =>
          printf("Warning: Unable to readlink %S\n", path);
          "???";
        };

      let atts = Map.add_exn(atts, ~key="targ", ~data=targ);
      atts;
    | Unix.S_FIFO => Map.add_exn(atts, ~key="kind", ~data="fifo")
    | Unix.S_SOCK => Map.add_exn(atts, ~key="kind", ~data="sock")
    | Unix.S_CHR =>
      let atts = Map.add_exn(atts, ~key="kind", ~data="chr");
      add_dev(atts, stat);
    | Unix.S_BLK =>
      let atts = Map.add_exn(atts, ~key="kind", ~data="blk");
      add_dev(atts, stat);
    };
  atts;
};
