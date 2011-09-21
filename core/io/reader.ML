
signature READER =
sig
  type T
  exception bad_input_exp of string
  val read_from_string : string -> T
  val read_from_file : string -> T
end

structure XMLReader : READER  =
struct

type T = XML.tree;
exception bad_input_exp of string;
     
fun read_from_string s = (XML.parse s)
              handle Library.ERROR msg => raise bad_input_exp msg;
    
fun read_from_file file = 
    let val instream =  TextIO.openIn file
    in 
      let fun read str =
	      case TextIO.inputLine instream
		 of  SOME s => read (str ^ s)
		   | NONE => str
	in 
	  let val s = read ""
	  in 
	    (
	     TextIO.closeIn instream ; 
	     read_from_string s
	    )
	  end
	end
      end;
      
end
