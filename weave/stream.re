/* These are simple input or output streams.  A stream can be
 * considered for line-based I/O, or for uncompressed streams, it is
 * possible to close the stream, and just use the filename (say as a
 * database).
 */

open Core;
module OC = Stdio.Out_channel;
module IC = Stdio.In_channel;

type writer = {
  .
  write_lines: list(string) => unit,
  close: unit,
  name: string,
};

type reader = {
  .
  read_line: option(string),
  close: unit,
  name: string,
};

/* Open a new file.  Note that we fail if the destination exists. */
let create_out = name => {
  let fd = OC.create(~fail_if_exists=true, ~perm=0o644, name);
  {
    as _;
    pub write_lines = lines => OC.output_lines(fd, lines);
    pub close = OC.close(fd);
    pub name = name
  };
};

/* Open a file for input. */
let open_in = name => {
  let fd = IC.create(name);
  {
    as _;
    pub read_line = IC.input_line(fd);
    pub close = IC.close(fd);
    pub name = name
  };
};

/*
 let zdump (z : 'a Zlib.t) tag =
   printf "---%s---:\nin_ofs = %d\nin_len = %d\nout_ofs = %d\nout_len = %d\n"
     tag z.in_ofs z.in_len z.out_ofs z.out_len
 */

/* The Zlib buffers work something like this:
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
 */

/* Set up the buffers, making them all initially allocated, with the
 * output buffer fully available, and the input buffer empty. */
let setup = (zbuf: Zlib.t('a)) => {
  zbuf.out_buf = Bigstring.create(4096);
  zbuf.out_ofs = 0;
  zbuf.out_len = Bigstring.length(zbuf.out_buf);
  zbuf.in_buf = Bigstring.create(4096);
  zbuf.in_ofs = 0;
  zbuf.in_len = 0;
};

/* Run zlib's flate, accepting only "Ok" as the response, and raising
 * an exception if it does something else. */
let flate = zbuf =>
  /* zdump zbuf "flate"; */
  switch (Zlib.flate(zbuf, Zlib.No_flush)) {
  | Zlib.Ok => ()
  | Zlib.Stream_end => ()
  | _ => failwith("Problem running zlib")
  };

/* Reset the input buffer if that makes sense. */
let reset_in = (zbuf: Zlib.t('a)) =>
  if (zbuf.in_ofs == Bigstring.length(zbuf.in_buf)) {
    zbuf.in_ofs = 0;
    zbuf.in_len = 0;
  };

/* When Zlib.flate returns Ok, we must then either remove some of the
 * output, or add new input.  It isn't particularly picky about how it
 * gets used (do we try to fill it up, etc), and there are kind of two
 * ways we can use it: 1. we can keep filling the input buffer until
 * the output buffer is full, or we are out of data to write. 2.
 * Consume the output buffer as soon as it is generated, filling the
 * input buffer only when it is drained.  We will use approach 1 for
 * writing (deflate), and approach 2 when reading (inflate).  This is
 * specific to our use case of reading and writing lines to the files.
 */

/** Query if the output buffer is full. */

let is_out_full = (zbuf: Zlib.t('a)) => zbuf.out_len == 0;

/** Flush the output buffer to the given channel, and reset the buffer
 * so that it is available.  The Bytes buffer (which must be at least
 * as large as the zbuf.out_buf) will be overwritten, used as an
 * intermediary. */

let write_out = (zbuf: Zlib.t('a), chan: OC.t, tmp: Bytes.t) => {
  let len = zbuf.out_ofs;
  Bigstring.To_bytes.blit(
    ~src=zbuf.out_buf,
    ~src_pos=0,
    ~dst=tmp,
    ~dst_pos=0,
    ~len,
  );
  OC.output(chan, ~buf=tmp, ~pos=0, ~len);
  zbuf.out_ofs = 0;
  zbuf.out_len = Bigstring.length(zbuf.out_buf);
};

type drain = Zlib.t(Zlib.deflate) => unit;

/* Attempt to add bytes to the buffer.  Runs flate if this buffer
 * fills up.  If the output buffer is full, will call 'drain', but
 * tries to only call this when the output buffer is full.
 */
let send_bytes = (zbuf: Zlib.t('a), ~data, ~drain: drain) => {
  let rec loop = (ofs, len) =>
    /* zdump zbuf (sprintf "send: ofs:%d len:%d" ofs len); */
    if (len == 0) {
      ();
    } else {
      let this =
        min(
          len,
          Bigstring.length(zbuf.in_buf) - (zbuf.in_len + zbuf.in_ofs),
        );
      if (this == 0) {
        /* No room, try flushing some. */
        flate(zbuf);
        if (is_out_full(zbuf)) {
          drain(zbuf);
        };
        reset_in(zbuf);
        loop(ofs, len);
      } else {
        Bigstring.From_string.blit(
          ~src=data,
          ~src_pos=ofs,
          ~dst=zbuf.in_buf,
          ~dst_pos=zbuf.in_ofs + zbuf.in_len,
          ~len=this,
        );
        zbuf.in_len = zbuf.in_len + this;
        loop(ofs + this, len - this);
      };
    };
  loop(0, String.length(data));
};

/* Open a new file, intended for compression. */
let gzip_out = name => {
  let fd = OC.create(~binary=true, ~fail_if_exists=true, ~perm=0o644, name);
  let zbuf = Zlib.create_deflate(~level=3, ~window_bits=31, ());
  setup(zbuf);
  let tmp_buf = Bytes.create(Bigstring.length(zbuf.out_buf));
  let drain = zbuf => write_out(zbuf, fd, tmp_buf);

  Zlib.set_header(
    zbuf.state,
    {
      text: false,
      mtime: 0l,
      os: 3,
      xflags: 0,
      extra: None,
      name: Some(name),
      comment: None,
    },
  );

  {
    as _;
    pub write_lines = lines =>
      List.iter(
        lines,
        ~f=line => {
          send_bytes(zbuf, ~data=line, ~drain);
          send_bytes(zbuf, ~data="\n", ~drain);
        },
      );
    pub close = {
      let rec loop = () =>
        switch (Zlib.flate(zbuf, Zlib.Finish)) {
        | Zlib.Ok =>
          drain(zbuf);
          loop();
        | Zlib.Stream_end => drain(zbuf)
        | _ => failwith("Unexpected return")
        };
      loop();
      OC.close(fd);
    };
    pub name = name
  };
};

/* inflating (reading from a gzip file) has slightly different buffing
 * requirements.  We will fill the input buffer when it is drained,
 * AND when there is no unprocessed input left.  In order to partially
 * process the input, we need to carry an additional reference to the
 * read position (called rpos in the code) of the next character to
 * read. */

/* Open an existing gzipped file. */
let gzip_in = name => {
  let fd = IC.create(name);
  let zbuf = Zlib.create_inflate(~window_bits=31, ());
  setup(zbuf);
  let rpos = ref(0);
  let tmp_buf = Bytes.create(Bigstring.length(zbuf.in_buf));

  /* Is the input buffer empty? */
  let input_empty = () => zbuf.in_len == 0;

  /* Assuming the input buffer is empty, read as much as we can into
   * the input buffer.  Returns the bytes read, with 0 indicating end
   * of file.  Because we only call this when we've exhausted the
   * output buffer, this should always indicate an end of file
   * condition. */
  let fill_in = () => {
    let len =
      IC.input(fd, ~buf=tmp_buf, ~pos=0, ~len=Bigstring.length(zbuf.in_buf));
    if (len > 0) {
      Bigstring.From_bytes.blit(
        ~src=tmp_buf,
        ~src_pos=0,
        ~dst=zbuf.in_buf,
        ~dst_pos=0,
        ~len,
      );
      zbuf.in_ofs = 0;
      zbuf.in_len = len;
    };
    len;
  };

  /* Read as many bytes that we can out of the output buffer, stopping
   * when we reach a newline.  Return `Done if we have read the
   * newline, and `More if there are more bytes needed.  On `Done, the
   * final newline is not placed into the dest buffer, but it is
   * skipped. */
  let get_line = dest => {
    let rec loop = () =>
      if (rpos^ == zbuf.out_ofs) {
        `More;
      } else {
        let ch = Bigstring.get(zbuf.out_buf, rpos^);
        incr(rpos);
        if (Char.(==)(ch, '\n')) {
          `Done;
        } else {
          Buffer.add_char(dest, ch);
          loop();
        };
      };

    loop();
  };

  /* Get a whole line from input.  This process is a bit tricky
   * because: 1. We only want to read from the input when our buffer
   * is drained, and 2. zlib considers it an error to run flate if it
   * isn't possible to do any work. To make this work, the simple
   * rules we follow each pass through the loop are: only fill the
   * input buffer when it is empty, and only run flate when the output
   * buffer is empty. */
  let run_line = () => {
    let dest = Buffer.create(64);
    let rec fill_step = () =>
      /* zdump zbuf "fill"; */
      if (rpos^ == zbuf.out_ofs) {
        /* There is no more output for us to process.  Fill the input
         * buffer if that makes sense. */
        if (input_empty()) {
          if (fill_in() == 0) {
            /* If we have exhausted our input, and nothing has been
             * read, this is the good EOF scenario.  Otherwise, we have
             * a partial buffer, without a newline. */
            if (Buffer.length(dest) == 0) {
              None;
            } else {
              failwith("Missing NL at EOF");
            };
          } else {
            /* We read data, go back which will run deflate a bit. */
            fill_step();
          };
        } else {
          /* There is input, but the output is empty.  Run flate to
           * decompress some more data */
          rpos := 0;
          zbuf.out_ofs = 0;
          zbuf.out_len = Bigstring.length(zbuf.out_buf);
          flate(zbuf);
          drain_step();
        };
      } else {
        /* If we have output to process, just process it, don't fill
         * or decompress. */
        drain_step();
      }

    and drain_step = () =>
      /* zdump zbuf (sprintf "drain rpos:%d" !rpos); */
      /* Copy out what we can. */
      switch (get_line(dest)) {
      | `More =>
        /* Didn't get the newline, repeat, getting new input. */
        fill_step()
      | `Done => Some(Buffer.contents(dest))
      };

    fill_step();
  };

  {
    as _;
    pub read_line = run_line();
    pub close = IC.close(fd);
    pub name = name
  };
};

let with_gen = (opener, ~f, path) => {
  let fd = opener(path);
  let res =
    try(Ok(f(fd))) {
    | ex => Error(ex)
    };
  fd#close;
  switch (res) {
  | Ok(result) => result
  | Error(ex) => raise(ex)
  };
};

let with_in = (~gzip, ~f, path) =>
  with_gen(if (gzip) {gzip_in} else {open_in}, ~f, path);
let with_out = (~gzip, ~f, path) =>
  with_gen(if (gzip) {gzip_out} else {create_out}, ~f, path);

/* A bulk test, to get an idea of timing. */
let bulk_io = () => {
  let count = 1_000_000;
  let fname = "sample1.txt";

  printf("Writing file\n");
  OC.flush(stdout);
  /* let ofd = create_out fname in */
  let ofd = gzip_out(fname);
  for (i in 1 to count) {
    ofd#write_lines([sprintf("This is a line %d in the file", i)]);
  };
  ofd#close;

  printf("Reading file\n");
  OC.flush(stdout);
  /* let ifd = open_in fname in */
  let ifd = gzip_in(fname);
  let rec loop = i =>
    switch (ifd#read_line) {
    | None => i
    | Some(line) =>
      /* printf "line: %S\n" line; */
      assert(String.(line == sprintf("This is a line %d in the file", i)));
      loop(i + 1);
    };
  assert(loop(1) == count + 1);
  ifd#close;

  Unix.unlink(fname);
};
