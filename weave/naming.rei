/* Naming conventions */

type t = {
  .
  new_temp: bool => Stream.writer,
  main_file: string,
  backup_file: string,
  is_compressed: bool,
};

let simple_naming:
  (~path: string, ~base: string, ~ext: string, ~compress: bool) => t;

let main_reader: t => Stream.reader;
let with_main_reader: (t, ~f: Stream.reader => 'a) => 'a;
let with_new_temp:
  (t, ~compressed: bool=?, ~f: Stream.writer => 'a, unit) => (string, 'a);

let trial: unit => unit;
