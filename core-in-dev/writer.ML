signature WRITER =
sig
  type T
  val write_to_string : T -> string
  val write_to_file : string -> T -> unit
end

(* ------------------------------------------------------- *)

(* save typing *)
structure WriteString
=
struct
fun write file str = 
    let val outstream =  TextIO.openOut file
    in 
      (
       TextIO.output (outstream, str);
       TextIO.closeOut outstream
      )
    end;
end

(* ------------------------------------------------------- *)

structure PrettyWriter : WRITER
=
struct
type T = Pretty.T;
open WriteString;

val write_to_string = Pretty.string_of

fun write_to_file file output = 
    write file (write_to_string output)

end

(* ------------------------------------------------------- *)

structure XMLWriter : WRITER
=
struct
type T = XML.tree;
open WriteString;

val write_to_string = XML.string_of

fun write_to_file file output = 
    write file (write_to_string output)

end

