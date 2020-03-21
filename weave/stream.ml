(* These are simple input or output streams.  A stream can be
 * considered for line-based I/O, or for uncompressed streams, it is
 * possible to close the stream, and just use the filename (say as a
 * database).
 *)

open Core
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

type writer = <
  write_lines : string list -> unit;
  close : unit;
  to_name : string >

type reader = <
  read_line : string option;
  close : unit;
  to_name : string >

(* Open a new file.  Note that we fail if the destination exists. *)
let create_out name =
  let fd = OC.create ~fail_if_exists:true ~perm:0o644 name in
  object
    method write_lines lines = OC.output_lines fd lines
    method close = OC.close fd
    method to_name = OC.close fd; name
  end

(* Open a file for input. *)
let open_in name =
  let fd = IC.create name in
  object
    method read_line = IC.input_line fd
    method close = IC.close fd
    method to_name = IC.close fd; name
  end

let zdump (z : 'a Zlib.t) tag =
  printf "---%s---:\nin_ofs = %d\nin_len = %d\nout_ofs = %d\nout_len = %d\n"
    tag z.in_ofs z.in_len z.out_ofs z.out_len

(* The Zlib buffers work something like this:
  *
  * *_buf - the buffer itself.  A Bigstring.t holding the data
  * *_ofs - the offset for zlib to use for the buffer.
  * *_len - the length of buffer that zlib can use.
  *
  * For buffers that are consumed by zlib, this is sufficient data.
  * For buffers consumed by us, if we are able to consume the entire
  * buffer, this is also sufficient.  However, if we need to be able
  * to consume only part of the buffer, we will have to maintain an
  * additional value, tracking where in the buffer, between 0 and
  * *_ofs our data is.
  *
  * It is also desirable to avoid allocations when possible.  Because
  * the file i/o operations work on regular Bytes.t instead of
  * Bigstrings, we need a separate buffer for file I/O, that we blit
  * to.
  *)

(* Set up the buffers, making them all initially allocated, with the
 * output buffer fully available, and the input buffer empty. *)
let setup (zbuf : 'a Zlib.t) =
  zbuf.out_buf <- Bigstring.create 4096;
  zbuf.out_ofs <- 0;
  zbuf.out_len <- Bigstring.length zbuf.out_buf;
  zbuf.in_buf <- Bigstring.create 4096;
  zbuf.in_ofs <- 0;
  zbuf.in_len <- 0

(* Run zlib's flate, accepting only "Ok" as the response, and raising
 * an exception if it does something else. *)
let flate zbuf =
  (* zdump zbuf "flate"; *)
  match Zlib.flate zbuf Zlib.No_flush with
    | Zlib.Ok -> ()
    | _ -> failwith "Problem running zlib"

(* Reset the input buffer if that makes sense. *)
let reset_in (zbuf : 'a Zlib.t) =
  if zbuf.in_ofs = Bigstring.length zbuf.in_buf then begin
    zbuf.in_ofs <- 0;
    zbuf.in_len <- 0
  end

(* When Zlib.flate returns Ok, we must then either remove some of the
 * output, or add new input.  It isn't particularly picky about how it
 * gets used (do we try to fill it up, etc), and there are kind of two
 * ways we can use it: 1. we can keep filling the input buffer until
 * the output buffer is full, or we are out of data to write. 2.
 * Consume the output buffer as soon as it is generated, filling the
 * input buffer only when it is drained.  We will use approach 1 for
 * writing (deflate), and approach 2 when reading (inflate).  This is
 * specific to our use case of reading and writing lines to the files.
 *)

(** Query if the output buffer is full. *)
let is_out_full (zbuf : 'a Zlib.t) = zbuf.out_len = 0

(** Flush the output buffer to the given channel, and reset the buffer
 * so that it is available.  The Bytes buffer (which must be at least
 * as large as the zbuf.out_buf) will be overwritten, used as an
 * intermediary. *)
let write_out (zbuf : 'a Zlib.t) (chan: OC.t) (tmp : Bytes.t) =
  let len = zbuf.out_ofs in
  Bigstring.To_bytes.blit ~src:zbuf.out_buf ~src_pos:0
    ~dst:tmp ~dst_pos:0 ~len;
  OC.output chan ~buf:tmp ~pos:0 ~len;
  zbuf.out_ofs <- 0;
  zbuf.out_len <- Bigstring.length zbuf.out_buf

type drain = Zlib.deflate Zlib.t -> unit

(* Attempt to add bytes to the buffer.  Runs flate if this buffer
 * fills up.  If the output buffer is full, will call 'drain', but
 * tries to only call this when the output buffer is full.
 *)
let send_bytes (zbuf : 'a Zlib.t) ~data ~(drain : drain) =
  let rec loop ofs len =
    (* zdump zbuf (sprintf "send: ofs:%d len:%d" ofs len); *)
    if len = 0 then ()
    else begin
      let this = min len (Bigstring.length zbuf.in_buf - (zbuf.in_len + zbuf.in_ofs)) in
      if this = 0 then begin
        (* No room, try flushing some. *)
        flate zbuf;
        if is_out_full zbuf then drain zbuf;
        reset_in zbuf;
        loop ofs len
      end else begin
        Bigstring.From_string.blit
          ~src:data ~src_pos:ofs
          ~dst:zbuf.in_buf ~dst_pos:(zbuf.in_ofs + zbuf.in_len)
          ~len:this;
        zbuf.in_len <- zbuf.in_len + this;
        loop (ofs + this) (len - this)
      end
    end in
  loop 0 (String.length data)

(* A drain can take data out of the sync.  It is given a segment of a
 * Bigstring, and can consume however much data it wishes to.  If it
 * does not consume the entire buffer, the process operation will end.
 * Returns the number of bytes it has consumed.
 *)

(* type fill = Bigstring.t -> pos:int -> len:int -> int *)

(* Open a new file, intended for compression. *)
let gzip_out name =
  let fd = OC.create ~binary:true ~fail_if_exists:true ~perm:0o644 name in
  let zbuf = Zlib.create_deflate ~level:3 ~window_bits:31 () in
  setup zbuf;
  let tmp_buf = Bytes.create (Bigstring.length zbuf.out_buf) in
  let drain zbuf = write_out zbuf fd tmp_buf in

  Zlib.set_header zbuf.state {
    text = false;
    mtime = 0l;
    os = 3;
    xflags = 0;
    extra = None;
    name = Some name;
    comment = None
  };

  object
    method write_lines lines =
      List.iter lines ~f:(fun line ->
        send_bytes zbuf ~data:line ~drain;
        send_bytes zbuf ~data:"\n" ~drain
      )
    method close =
      let rec loop () =
        match Zlib.flate zbuf Zlib.Finish with
          | Zlib.Ok -> drain zbuf; loop ()
          | Zlib.Stream_end -> drain zbuf
          | _ -> failwith "Unexpected return" in
      loop ();
      OC.close fd
    method to_name = failwith "to_name unsupported with zlib"
  end
;;

(* Open an existing gzipped file. *)
let gzip_in name =
  let fd = IC.create name in
  let zbuf = Zlib.create_inflate ~window_bits:31 () in
  zbuf.in_len <- 0;
  zbuf.out_buf <- Bigstring.create 4096;
  zbuf.out_ofs <- 0;
  zbuf.out_len <- Bigstring.length zbuf.out_buf;
  let fill () =
    let block = Bytes.create 4096 in
    let len = IC.input fd ~buf:block ~pos:0 ~len:(Bytes.length block) in
    zbuf.in_buf <- Bigstring.of_bytes block ~pos:0 ~len:len;
    zbuf.in_ofs <- 0;
    zbuf.in_len <- len in
  object
    method read_line =
      if zbuf.in_len = 0 then fill ();
      if zbuf.in_len = 0 then None
      else begin
        let result = Buffer.create 64 in
        let rec outer_loop () =
          zdump zbuf "flate";
          match Zlib.flate zbuf Zlib.No_flush with
            | Zlib.Ok ->
                zdump zbuf "ok!";
                let rec loop () =
                  if zbuf.out_len = 0 then outer_loop ()
                  else begin
                    let ch = Bigstring.get zbuf.in_buf zbuf.in_ofs in
                    zbuf.in_ofs <- zbuf.in_ofs + 1;
                    zbuf.in_len <- zbuf.in_len - 1;
                    match ch with
                      | '\n' ->
                          printf "Return %S\n" (Buffer.contents result);
                          Some (Buffer.contents result)
                      | ch ->
                          Buffer.add_char result ch;
                          if zbuf.in_len = 0 then fill ();
                          loop ()
                  end in
                loop ()
            | Zlib.Buf_error -> zdump zbuf "buf error"; failwith "Buf error"
            | _ -> failwith "Decompress error"
        in outer_loop ()
      end
    method close = IC.close fd
    method to_name = failwith "to_name unsupported with zlib"
  end

(* A bulk test, to get an idea of timing. *)
let bulk_io () =
  let count = 1_000_000 in
  let fname = "sample1.txt" in

  printf "Writing file\n"; OC.flush stdout;
  (* let ofd = create_out fname in *)
  let ofd = gzip_out fname in
  for i = 1 to count do
    ofd#write_lines [sprintf "This is a line %d in the file" i]
  done;
  ofd#close;

  printf "Reading file\n"; OC.flush stdout;
  (* let ifd = open_in fname in *)
  let ifd = gzip_in fname in
  let rec loop i =
    match ifd#read_line with
      | None -> i
      | Some line ->
          assert String.(line = sprintf "This is a line %d in the file" i);
          loop (i + 1) in
  assert (loop 1 = count + 1);
  ifd#close;

  Unix.unlink fname
