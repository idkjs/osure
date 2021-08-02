/* Single channel queues. */

open Core;

/* Core hides the Caml Mutex module and replaces it with an empty one.
 */
module Mutex = Caml.Mutex;

/* But, use the Ocaml mutex/condition.  Async isn't all that useful
 * here, as our purpose is to make use of multiple CPUs in the hashing
 * code running with the runtime lock released. */

type t('a) = {
  bound: int,
  elts: Queue.t('a),
  lock: Mutex.t,
  reader: Condition.t,
  writer: Condition.t,
};

let create = (~bound) => {
  bound,
  elts: Queue.create(~capacity=bound, ()),
  lock: Mutex.create(),
  reader: Condition.create(),
  writer: Condition.create(),
};

let push = (t, elt) => {
  Mutex.lock(t.lock);
  while (Queue.length(t.elts) >= t.bound) {
    Condition.wait(t.writer, t.lock);
  };
  Queue.enqueue(t.elts, elt);
  Condition.signal(t.reader);
  Mutex.unlock(t.lock);
};

let pop = t => {
  Mutex.lock(t.lock);
  let elt = {
    let rec loop = () =>
      switch (Queue.dequeue(t.elts)) {
      | Some(elt) => elt
      | None =>
        Condition.wait(t.reader, t.lock);
        loop();
      };
    loop();
  };

  Condition.signal(t.writer);
  Mutex.unlock(t.lock);
  elt;
};
