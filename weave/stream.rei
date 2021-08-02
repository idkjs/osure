/* Streaming weaves. */

type writer = {
  .
  write_lines: list(string) => unit,
  close: unit,
  name: string,
};

type reader = {
  .
  read_line: option(string),
  close: unit,
  name: string,
};

let create_out: string => writer;
let open_in: string => reader;

let gzip_out: string => writer;
let gzip_in: string => reader;

let with_in: (~gzip: bool, ~f: reader => 'a, string) => 'a;
let with_out: (~gzip: bool, ~f: writer => 'a, string) => 'a;

/* Benchmark */
let bulk_io: unit => unit;
