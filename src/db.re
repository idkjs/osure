/* Database operations. */

open Core;

let make_hash_schema = db =>
  Sqlite3.exec(db, "CREATE TABLE hashes ( id INTEGER PRIMARY KEY, hash BLOB)")
  |> Sqlite3.Rc.check;

let with_xact = (db, ~f) => {
  Sqlite3.exec(db, "BEGIN TRANSACTION") |> Sqlite3.Rc.check;
  let res =
    try(Ok(f(db))) {
    | ex => Error(ex)
    };
  switch (res) {
  | Ok(res) =>
    Sqlite3.exec(db, "COMMIT") |> Sqlite3.Rc.check;
    res;
  | Error(ex) =>
    Sqlite3.exec(db, "ROLLBACK") |> Sqlite3.Rc.check;
    raise(ex);
  };
};

/* Run f in a context where its argument is valid as a sequence to
 * retrieve the hashes (sorted by id). */
let with_hashes = (db, ~f) => {
  let stmt = Sqlite3.prepare(db, "SELECT id, hash FROM hashes ORDER BY id");
  let elts =
    Sequence.unfold(~init=(), ~f=() =>
      switch (Sqlite3.step(stmt)) {
      | ROW =>
        let id = Sqlite3.column_int(stmt, 0);
        let hash = Sqlite3.column_blob(stmt, 1);
        Some(((id, hash), ()));
      | DONE => None
      | err => failwith(Sqlite3.Rc.to_string(err))
      }
    );
  /* TODO: Handle and propagate exceptions here. */
  let result = f(elts);
  Sqlite3.finalize(stmt) |> Sqlite3.Rc.check;
  result;
};
