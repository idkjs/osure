/** The store.
 * The store maps between a naming convention (directory, basename,
 * etc) weaves stored in that file, and provides the support necessary
 * to read and write sure data to them.
 */;

/** The store itself. */

type t;

/** Printer for the store */

let show: t => string;

/** Given a filename, decode the name, creating a 't' if possible. */

let parse: string => t;

/** Print a listing of the given store. */

let listing: t => unit;

/** Specifier for revision numbers. */

type revision = [ | `Latest | `Previous | `Num(int)];

/** Retrieve a revision, calling 'f' with a pull parser that returns
 * the lines of the revision. */

let with_rev: (t, revision, ~f: (unit => option(Node.t)) => 'a) => 'a;

/** Start writing nodes to a new temp file.  The function is called
 * with a writer argument that writes these nodes to a temp file.
 * Returns the function return value, along with the name of the temp
 * file. */

let with_temp: (t, ~f: (Node.t => unit) => 'a) => (string, 'a);

/** Read in a temp file as a node reader. */

let with_temp_in:
  (string, ~gzip: bool, ~f: (unit => option(Node.t)) => 'a) => 'a;

/** Create a new temp file, and invoke 'f' with an Sqlite database
 * handle opened on that file. */

let with_temp_db: (t, ~f: Sqlite3.db => 'a) => (string, 'a);

/** Call 'f' with a node writer that will make the first delta is
 * done. */

let with_first_delta:
  (~tags: Weave.Tags.t=?, t, ~f: (Node.t => unit) => 'a) => ('a, int);

/** Call 'f' with a node writer that will make a new delta. */

let with_added_delta:
  (~tags: Weave.Tags.t=?, t, ~f: (Node.t => unit) => 'a) => ('a, int);
