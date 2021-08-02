/* Node testing. */

open Core;

module RS = Random.State;

/* Generate a random name (as a key). */
let gen_name = rng => {
  let len = 5 + RS.int(rng, 10);
  let buf = Buffer.create(len);
  for (_ in 1 to len) {
    Buffer.add_char(
      buf,
      Char.of_int_exn(RS.int(rng, 26) + Char.to_int('a')),
    );
  };
  Buffer.contents(buf);
};

/* Generate a longer name, with possibly invalid characters. */
let gen_harsh_name = rng => {
  let len = 1 + RS.int(rng, 32);
  let buf = Buffer.create(len);
  for (_ in 1 to len) {
    Buffer.add_char(buf, Char.of_int_exn(RS.int(rng, 256)));
  };
  Buffer.contents(buf);
};

/* Generate a random set of attributes. */
let gen_atts = rng => {
  let len = RS.int(rng, 8);
  let alist =
    List.init(
      len,
      ~f=_ => {
        let key = gen_name(rng);
        let value = gen_harsh_name(rng);
        (key, value);
      },
    );
  /* Fold with 'set' because we don't want an exception if we happen
   * to generate the same random key. */
  List.fold(alist, ~init=Map.empty((module String)), ~f=(m, (key, data)) =>
    Map.set(m, ~key, ~data)
  );
};

/** Generate a randomishy node */

let gen_node = (rng): Osure.Node.t =>
  switch (RS.int(rng, 4)) {
  | 0 => [@implicit_arity] Enter(gen_harsh_name(rng), gen_atts(rng))
  | 1 => [@implicit_arity] File(gen_harsh_name(rng), gen_atts(rng))
  | 2 => Sep
  | 3 => Leave
  | _ => failwith("Invalid")
  };

let run_test = () => {
  let rng = RS.make([|1, 2, 3|]);
  for (_ in 1 to 10_000) {
    let node = gen_node(rng);
    let node2 = Osure.Node.parse(Osure.Node.show(node));
    /* printf "exp %s\n" (Osure.Node.show node); */
    /* printf "got %s\n" (Osure.Node.show node2); */
    assert(Osure.Node.equal(node, node2));
  };
};
