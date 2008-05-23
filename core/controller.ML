(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Console based controller for graph manipulations *)
(* based on the IsaPlanner interface *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Controller =
struct

structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge

exception NOT_IMPLEMENTED of string
exception ILLEGAL_COMMAND of string

val blank = Pretty.str "";

val fresh_red = V.IVert (V.Xnd AngleExpr.zero);
val fresh_green = V.IVert (V.Znd AngleExpr.zero);
val fresh_h = V.IVert V.Hnd ;
val fresh_bnd = V.BVert ()

val log = ref TextIO.stdErr;

(* what language are we outputting our graphs --------------------------------- *)
datatype language = DOT | XML;
val dump_lang = ref XML;
val dumper = ref RGGraphXML.output_graph
val write_to_file = ref RGGraphXMLWriter.output_to_file
fun switch_dump_lang () = 
    case !dump_lang of DOT => (dump_lang := XML ; 
			       dumper := RGGraphXML.output_graph ;
			       write_to_file := RGGraphXMLWriter.output_to_file
			      )
		     | XML => (dump_lang := DOT ; 
			       dumper := RGGraphDot.output_graph ;
			       write_to_file := RGGraphDotWriter.output_to_file)

fun write_log s = (TextIO.output ((!log) , s); TextIO.flushOut (!log)) ;


(* are we talking to a toplevel or a pipe? ---------------------------------*)
val is_out_pipe = ref true;
fun out_toplevel out = Pretty.writeln out			
fun out_pipe out = 
    (
     Pretty.writeln out;
     Pretty.writeln (Pretty.str "stop")
    )
val do_output = ref out_pipe;			  
fun switch_mode () = 
    if !is_out_pipe 
    then (do_output := out_toplevel ; is_out_pipe := false)
    else (do_output := out_pipe ; is_out_pipe := true)



(* Working with user commands --------------------------------- *)
(* many more to be added *)
datatype commands = NOOP
	    | Quit 
	    | HELO
	    | MODE
	    | DUMP
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
	    | DelVertex of V.name
	    | UpVertex of V.name * V.data
	    | DelEdge of E.name

val kill_newlines  = 
    String.translate (fn #"\n" => "" | c => (str c)) 

(* does not parse angleexpr yet *)
fun parse_node_data [] = raise (ILLEGAL_COMMAND "up ")
  | parse_node_data (bnd::t) = 
    if bnd = "true" then fresh_bnd
    else if bnd = "false" then 
      case t of [] =>  raise ILLEGAL_COMMAND ("up "^bnd)
	      | col::_ => 
		case col of "red" => fresh_red
			  | "green" => fresh_green
			  | "h" => fresh_h
			  | _ => raise ILLEGAL_COMMAND ("up false "^col)
    else raise ILLEGAL_COMMAND ("up "^bnd)


fun parse_command s = 
    let val _ = write_log s
	val toks = String.tokens (fn #" " => true | _ => false) (kill_newlines s)
    in
      if s = "" 
      then Quit
      else case toks of [] => NOOP
		      | cmd::args => 
			case cmd  of  "Q" => Quit 
				    | "M" => MODE (* switch between output modes *)
				    | "H" => HELO (*print staus *)
				    | "D" => DUMP (* output the current graph *)
				    | "S" => Save (hd args)
				    | "O" => Open (hd args)
				    | "u" => Undo
				    | "n" => New (* new graph *)
				    | "r" => Red 
				    | "g" => Green
				    | "h" => H
				    | "b" => Boundary
				    | "e" => Edge ((hd args),(( hd o tl) args))
				    | "d" => DelVertex (hd args)		
				    | "de" => DelEdge (hd args)
				    | "up" => UpVertex (hd args ,
							parse_node_data (tl args))
				    |_ => raise ILLEGAL_COMMAND s
    end;

    
fun do_command g hist c = 
    case c of
      NOOP => (g, hist, blank, true)	      
    | DUMP => (g, hist, (!dumper) g, true)	      
    | Save filename => ( 
      (!write_to_file) filename g ;
      (g, hist, Pretty.str ("Wrote graph to file "^filename), true)	      
      )
    | Open _ => raise NOT_IMPLEMENTED "Open"
    | MODE => (switch_mode () ;
	       (g, hist, Pretty.str "Changed output mode.", true)
	      )
    | Quit => (g, hist, Pretty.str "Quitting.....\n",  false)
    | HELO => let val msg  = Pretty.chunks [Pretty.str"Hello from QUANTOMATIC",
					    Pretty.str "Let's have some fun!"]
	      in 
		(g, hist, msg, true)
	      end
    | New => (G.empty, 
	      g::hist, 
	      Pretty.str "New Graph.",
	      true)
    | Undo => (case hist of 
		[] => (g, hist, blank,true)
	      | h::t => (h, t, Pretty.str "Undo!",true)
	      )
    | Red => 
      let val (_,g') = G.add_vertex fresh_red g
      in 
	(g', g::hist, Pretty.str "Red added.",true)
      end
    | Green => 
      let val (_,g') = G.add_vertex fresh_green g
      in 
	(g', g::hist, Pretty.str "Green added.",true)
      end
    | H => 
      let val (_,g') = G.add_vertex fresh_h g
      in 
	(g', g::hist, Pretty.str "H added.",true)
      end
    | Boundary => 
      let val (_,g') = G.add_vertex fresh_bnd g
      in 
	(g', g::hist, Pretty.str "Boundary added.",true)
      end
    | Edge (v1, v2) =>
      let val (_,g') = G.add_edge () v1 v2 g
      in
	(g', g::hist, Pretty.str "Edge added.",true)
      end 
    | DelVertex v => 
      let val g' = G.delete_vertex v g
      in 
	(g', g::hist, Pretty.str "Vertex Deleted.",true)
      end
    | DelEdge e => 
      let val g' = G.delete_edge e g
      in 
	(g', g::hist, Pretty.str "Edge Deleted.",true)
      end
    | UpVertex (v,d) => 
      let val g' = G.set_vertex d v g
      in 
	(g', g::hist, Pretty.str "Vertex Updated.",true)
      end
      
    | _ => raise NOT_IMPLEMENTED "nothing useful implemented"


(*----------------------- MAIN RUN LOOP  ----------------------------------------- *)
fun run_loop g hist = 
	  let 
            val _ = if not (!is_out_pipe) then TextIO.print "quanto:> " else ()
	    val sopt = TextIO.inputLine(TextIO.stdIn) 
            val s = (case sopt of NONE => "" | SOME s => s)
	    val (g', hist', output, continue) = ((do_command g hist) o parse_command) s
		handle NOT_IMPLEMENTED s => 
		       (g, hist, Pretty.str ("Not Implemented:" ^ s), true)
		     | ILLEGAL_COMMAND s => 
		       (g, hist, Pretty.str ("Illegal command:" ^ s), true)
	  in
	    (
	     if output <> blank then ((!do_output) output) else () ;
	     if continue then 
	       run_loop g' hist'
	     else 
	       (
		write_log "Exiting run loop";
		TextIO.closeOut (!log); 
		g'
		)
	    )
	  end
	  
fun die msg = 
    (
     write_log ("Crash --> "^msg^"\n") ;
     TextIO.closeOut (!log) ; 
     (!do_output) (Pretty.str ("<message><crash>\n"^msg^"\n</crash></message>")) ;
     if !is_out_pipe then 
       OS.Process.exit (OS.Process.failure)
     else
       ()       
    )
    handle _ => OS.Process.exit (OS.Process.failure)

fun init () = (
    log := (TextIO.openAppend "/Users/rwd/Desktop/quantolog") ;
    write_log "Starting up...\n" ; 
    run_loop G.empty []
)
    handle _ => (
	   die "ML threw an Exception... better RESTART.";
	   G.empty (* just to keep the compiler happy *)
	   )

end; 
