/* Weave tags */

type t = list((string, string));

let to_json: t => Yojson.Safe.t;
let from_json: Yojson.Safe.t => t;

/* Parse a "key=value" tag. */
let from_equal: string => (string, string);
