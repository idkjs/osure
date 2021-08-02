/** Weave files will follow a naming convention.  This determines the
 * names of various temp files and other aspects.  The SCCS
 * conventions are not followed, because they are not safe (this code
 * will never write to a file that already exists). */;

open Core;

type t = {
  .
  new_temp: bool => Stream.writer,
  main_file: string,
  backup_file: string,
  is_compressed: bool,
};

/* Open a reader for the main file. */
let main_reader = sn => {
  let opener =
    if (sn#is_compressed) {
      Stream.gzip_in;
    } else {
      Stream.open_in;
    };
  opener(sn#main_file);
};

/* Open a reader, closing at the end. */
let with_main_reader = sn =>
  Stream.with_in(~gzip=sn#is_compressed, sn#main_file);

/* Open a new temp file, for writing, and invoke the function to write
 * the contents, and then close the file.  Returns the filename at the
 * end. */
let with_new_temp = (sn, ~compressed=true, ~f, ()) => {
  let wr = sn#new_temp(compressed);
  let name = wr#name;
  let res =
    try(Ok(f(wr))) {
    | ex => Error(ex)
    };
  wr#close;
  switch (res) {
  | Ok(result) => (name, result)
  | Error(ex) => raise(ex)
  };
};

/* The simple naming convention has a basename, with the main file
 * having a specified extension, the backup file having a ".bak"
 * extension, and the temp files using a numbered extension starting
 * with ".0".  If the names are intended to be compressed, a ".gz"
 * suffix can also be added.
 */

let simple_naming = (~path, ~base, ~ext, ~compress) => {
  let make_name = (ext, compressed) =>
    path ^/ sprintf("%s.%s%s", base, ext, if (compressed) {".gz"} else {""});

  /* Construct a new output file. */
  let temp_file = this_compress => {
    let make = if (this_compress) {Stream.gzip_out} else {Stream.create_out};
    let rec loop = n => {
      let name = make_name(Int.to_string(n), this_compress);
      let fd =
        try(Some(make(name))) {
        | Sys_error(_) => None
        };
      switch (fd) {
      | None => loop(n + 1)
      | Some(fd) => fd
      };
    };
    loop(0);
  };

  {
    as _;
    pub new_temp = this_compress => temp_file(this_compress);
    pub main_file = make_name(ext, compress);
    pub backup_file = make_name("bak", compress);
    pub is_compressed = compress
  };
};

let trial = () => {
  let sn = simple_naming(~path=".", ~base="haha", ~ext="dat", ~compress=true);
  let f1 = sn#new_temp(true);
  printf("name: %s\n", f1#name);
};
