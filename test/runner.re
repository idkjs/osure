/* Test runner. */

open Core;

module RS = Random.State;

/* Validate that this buffer does not contain any invalid characters. */
let check_buf = buf =>
  String.iter(buf, ~f=ch =>
    assert(Char.(ch >= '!' && ch <= '~' && ch != '[' && ch != ']'))
  );

/* Test the escaping code. */
let test_escape = () => {
  let rng = RS.make([|1, 2, 3|]);
  let buf = Buffer.create(4096);
  for (_ in 1 to 4096) {
    Buffer.add_char(buf, RS.char(rng));
  };
  let buf = Buffer.contents(buf);
  let escaped = Osure.Escape.escape(buf);
  /* printf "%S\n" escaped; */
  check_buf(escaped);
  let regular = Osure.Escape.unescape(escaped);
  /* printf "old: %S\nnew: %S\n" buf regular; */
  assert(String.(==)(buf, regular));
};

let must_fail = (f, arg) => {
  let result =
    try({
      let _ = f(arg);
      `Failure;
    }) {
    | Failure(_) => `Raised
    };
  switch (result) {
  | `Failure => failwith("Failed to detect invalid hex")
  | `Raised => ()
  };
};

let test_bad_hex = () => {
  must_fail(Osure.Escape.unescape, "Hello=");
  must_fail(Osure.Escape.unescape, "Hello=4");
  must_fail(Osure.Escape.unescape, "Hello=5g");
};

let () = {
  printf("Testing escape\n");
  test_escape();
  printf("Testing escape bad hex\n");
  test_bad_hex();
  printf("Testing nodes\n");
  Nodes.run_test();
};
