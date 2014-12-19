structure File_Io =
struct
  (* this is nicer to deal with that IO.Io *)
  exception io_exn of string

  fun write_string file s = let
    val outstream = TextIO.openOut file
  in
    (TextIO.output (outstream, s);
     TextIO.closeOut outstream)
  end
  handle IO.Io {cause as (OS.SysErr (msg:string,_)) : exn, ...} =>
          raise io_exn ("Could not write to \""^file^"\": "^msg)
       | IO.Io {name : string, function : string, ...} =>
          raise io_exn ("Could not write to \""^file^
                        "\": unknown error in "^function^
                        " of "^name)

  fun write_json file j = write_string file (Json.encode j)
  fun write_pretty file p = write_string file (Pretty.string_of p)


  fun read_string file = let
    val instream = TextIO.openIn file
    val text = TextIO.inputAll instream
    val _ = TextIO.closeIn instream
  in
    text
  end
  handle IO.Io {cause as (OS.SysErr (msg:string,_)) : exn, ...} =>
          raise io_exn ("Could not read from \""^file^"\": "^msg)
       | IO.Io {name : string, function : string, ...} =>
          raise io_exn ("Could not read from \""^file^
                        "\": unknown error in "^function^
                        " of "^name)

  fun read_json file = Json.read_file file
  handle IO.Io {cause as (OS.SysErr (msg:string,_)) : exn, ...} =>
          raise io_exn ("Could not read from \""^file^"\": "^msg)
       | IO.Io {name : string, function : string, ...} =>
          raise io_exn ("Could not read from \""^file^
                        "\": unknown error in "^function^
                        " of "^name)
end
