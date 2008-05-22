(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Console based controller for graph manipulations *)
(* based on the IsaPlanner interface *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Controller =
struct

structure G = RGGraph
structure V = G.Vertex
structure BV = G.BVertex
structure RGV = G.RGVertex
structure E = G.Edge

exception NOT_IMPLEMENTED of string
exception ILLEGAL_COMMAND of string

val blank = Pretty.str "";

val fresh_red = BV.IVert (RGV.Xnd AngleExpr.zero);
val fresh_green = BV.IVert (RGV.Znd AngleExpr.zero);
val fresh_h = BV.IVert RGV.Hnd ;
val fresh_bnd = BV.BVert ()

val log = ref TextIO.stdErr;

(* what language are we outputting our graphs --------------------------------- *)
datatype language = DOT | XML;
val dump_lang = ref XML;
val dumper = ref RGGraphXML.output_graph
fun switch_dump_lang () = 
    case !dump_lang of DOT => (dump_lang := XML ; dumper := RGGraphXML.output_graph)
		     | XML => (dump_lang := DOT ; dumper := RGGraphDot.output_graph)

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

val kill_newlines  = 
    String.translate (fn #"\n" => "" | c => (str c)) 

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
				    | "D" => DUMP
				    | "u" => Undo
				    | "n" => New (* new graph *)
				    | "r" => Red 
				    | "g" => Green
				    | "h" => H
				    | "b" => Boundary
				    | "e" => Edge ((hd args),(( hd o tl) args))
				    |_ => raise ILLEGAL_COMMAND s
    end;

fun do_command g hist c = 
    case c of
      NOOP => (g, 
	       hist, 
	       blank, 
	       true)	      
    | DUMP => (g, 
	       hist, 
	       (!dumper) g, 
	       true)	      
    | MODE => (switch_mode () ;
	       (g, 
		hist, 
		Pretty.str "Changed output mode.", 
		true))
    | Quit => (g, 
	       hist, 
	       Pretty.str "Quitting.....\n", 
	       false)
    | HELO => (g, 
	       hist, 
	       Pretty.str "Hello from QUANTOMATIC\nLet's have some fun!",
	       true)
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
	  
fun init () = (
    log := (TextIO.openAppend "/Users/rwd/Desktop/quantolog") ;
    write_log "Starting up...\n" ; 
    run_loop G.empty []
);


end; 
