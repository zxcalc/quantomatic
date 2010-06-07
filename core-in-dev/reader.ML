
signature READER =
sig
  type T
  val read_from_string : string -> T
  val read_from_file : string -> T
end

structure XMLReader : READER  =
struct

type T = XML.tree;
     
val read_from_string = XML.parse;
    
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
