/* Hash */

open Core;

external hash_file: string => string = "hash_file";

let hexlify = hash => {
  let buf = Buffer.create(String.length(hash) * 2);
  String.iter(
    hash,
    ~f=ch => {
      let ch = Char.to_int(ch);
      bprintf(buf, "%02x", ch);
    },
  );
  Buffer.contents(buf);
};
