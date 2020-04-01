(* Escaping *)

open Core

let escape text =
  let buf = Buffer.create (String.length text) in
  String.iter text ~f:(fun ch ->
    if Char.(ch >= '!' && ch <= '~' && ch <> '[' && ch <> ']' && ch <> '=') then
      Buffer.add_char buf ch
    else
      Buffer.add_string buf (sprintf "=%02x" (Char.to_int ch)));
  Buffer.contents buf

let dehex ch =
  if Char.(ch >= '0' && ch <= '9') then
    Char.to_int ch - Char.to_int '0'
  else if Char.(ch >= 'a' && ch <= 'f') then
    Char.to_int ch - Char.to_int 'a' + 10
  else
    failwith "Invalid hex char"

let unescape text =
  let len = String.length text in
  let buf = Buffer.create len in
  let rec loop pos =
    if pos = len then
      Buffer.contents buf
    else begin
      if Char.(text.[pos] = '=') then begin
        if pos + 2 >= len then failwith "Unterminated '=' escape";
        let num = dehex text.[pos+1] * 16 + dehex text.[pos+2] in
        Buffer.add_char buf (Char.of_int_exn num);
        loop (pos + 3)
      end else begin
        Buffer.add_char buf text.[pos];
        loop (pos + 1)
      end
    end in
  loop 0
