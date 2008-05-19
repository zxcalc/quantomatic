(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Console based controller for graph manipulations *)
(* based on the IsaPlanner interface *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Controller =
struct

structure Graph = RGGraph
structure V = RGGraph.Vertex
structure E = RGGraph.Edge

exception NOT_IMPLEMENTED of string
exception ILLEGAL_COMMAND of string

val blank = Pretty.str "";

(* many more to be added *)
datatype commands = NOOP
	    | Quit 
	    | New
	    | Open of string
	    | Save of string
	    | Undo
	    | Red 
	    | Green
	    | H
	    | Boundary
	    | Edge of V.name * V.name
	    | Angle of V.name * AngleExpr.T
	    | ShowRewrites of V.NSet.T

fun parse_command s = 
    case s of "" => Quit
	    | "q\n" => Quit 
	    | " \n" => NOOP
	    | "\n" => NOOP
	    | "n\n" => New
	    | "u\n" => Undo
	    |_ => raise ILLEGAL_COMMAND s

fun do_command g hist c = 
    case c of
      NOOP => (g, hist, blank)
    | Quit => OS.Process.exit OS.Process.success
    | New => (Graph.empty, g::hist, blank)
    | Undo => (case hist of 
		[] => (g, hist, blank)
	      | h::t => (h, t, Pretty.str "Undo!")
	      )
    | _ => raise NOT_IMPLEMENTED "nothing useful implemented"



fun run_loop g hist = 
	  let 
            val _ = TextIO.print "Quanto> "
	    val sopt = TextIO.inputLine(TextIO.stdIn) 
            val s = (case sopt of NONE => "" | SOME s => s)
	    val (g', hist', output) = ((do_command g hist) o parse_command) s
		handle NOT_IMPLEMENTED s => 
		       (g, hist, Pretty.str ("Not Implemented:" ^ s))
		     | ILLEGAL_COMMAND s => 
		       (g, hist, Pretty.str ("Illegal command:" ^ s))
	  in
	    (
	     if output <> blank then Pretty.writeln output else () ; 
	     run_loop g' hist'
	    )
	  end
	  
fun init () = run_loop Graph.empty [];

end; 
