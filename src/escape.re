/* Escaping */

open Core;

let escape = text => {
  let buf = Buffer.create(String.length(text));
  String.iter(text, ~f=ch =>
    if (Char.(ch >= '!' && ch <= '~' && ch != '[' && ch != ']' && ch != '=')) {
      Buffer.add_char(buf, ch);
    } else {
      Buffer.add_string(buf, sprintf("=%02x", Char.to_int(ch)));
    }
  );
  Buffer.contents(buf);
};

let dehex = ch =>
  if (Char.(ch >= '0' && ch <= '9')) {
    Char.to_int(ch) - Char.to_int('0');
  } else if (Char.(ch >= 'a' && ch <= 'f')) {
    Char.to_int(ch) - Char.to_int('a') + 10;
  } else {
    failwith("Invalid hex char");
  };

let unescape = text => {
  let len = String.length(text);
  let buf = Buffer.create(len);
  let rec loop = pos =>
    if (pos == len) {
      Buffer.contents(buf);
    } else if (Char.(text.[pos] == '=')) {
      if (pos + 2 >= len) {
        failwith("Unterminated '=' escape");
      };
      let num = dehex(text.[pos + 1]) * 16 + dehex(text.[pos + 2]);
      Buffer.add_char(buf, Char.of_int_exn(num));
      loop(pos + 3);
    } else {
      Buffer.add_char(buf, text.[pos]);
      loop(pos + 1);
    };
  loop(0);
};
