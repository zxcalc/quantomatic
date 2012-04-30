signature WRITER =
sig
  type T
  val write_to_string : T -> string
  val write_to_file : string -> T -> unit
end

(* ------------------------------------------------------- *)
functor Writer(type T val to_string : T -> string) : WRITER
= struct
  type T = T
  val write_to_string = to_string;
  fun write_to_file file x = 
      let val s = write_to_string x;
          val outstream = TextIO.openOut file
      in 
        (TextIO.output (outstream, s);
         TextIO.closeOut outstream)
      end;
end;

(* ------------------------------------------------------- *)
structure WriteString : WRITER = Writer(
  type T = string 
  val to_string = I);
structure WritePretty : WRITER = Writer(
  type T = Pretty.T 
  val to_string = Pretty.string_of);
structure XMLWriter : WRITER = Writer(
  type T = XML.tree
  val to_string = (prefix XML.header) o XML.string_of)
