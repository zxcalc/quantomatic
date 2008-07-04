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
exception COMMAND_NOT_APPLICABLE_NOW of string
exception QUIT of G.T

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

(* -- this doesn't belong here but it'll do for now -- *)
(* args =  rule_name fused lhs rhs new_vnames n howmany  *)
fun send_rewrite rule_name fused lhs rhs howmany n  =  (
    (!do_output) (Pretty.str ("<rewrite>\n<rulename>\n"^"\n</rulename>\n"));
    (!dumper) fused ;
    (!do_output) (Pretty.str ("<lhs>\n")) ;
    (!dumper) lhs ;
    (!do_output) (Pretty.str ("</lhs>\n<rhs>")) ;
    (!dumper) rhs ;
    (!do_output) (Pretty.str ("</rhs>\n<total>"^ (Int.toString howmany) ^ "</total>\n" ^
			      "<total>"^ (Int.toString n) ^ "</total>\n" ^
			      "</rewrite>"))
)


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
	    | AbortRewrites (* these require some state*)
	    | DoRewrite     (* which only exists after ShowRewrites *)
	    | NextRewrite   (* has been called *)
	    | PrevRewrite   
	    | DelVertex of V.name
	    | UpVertex of V.name * V.data
	    | DelEdge of E.name

val kill_newlines  = 
    String.translate (fn #"\n" => "" | c => (str c)) 

fun parse_node_list [] = V.NSet.empty
  | parse_node_list (h::t) = V.NSet.ins_fresh h (parse_node_list t)

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
				    | "RWshow" => ShowRewrites (parse_node_list args)
				    | "RWyes" => DoRewrite
				    | "RWnext" => NextRewrite
				    | "RWprev" => PrevRewrite
				    | "RWabort" => AbortRewrites
				    |_ => raise ILLEGAL_COMMAND s
    end;

(* ------- a mini run loop / dispatcher for choosing rewrites ---*)

fun interactive_rw_loop g [] [] = (Pretty.writeln (Pretty.str "No applicable rewrites"); g)
  | interactive_rw_loop g [] rwlist_bkd = interactive_rw_loop g (List.rev rwlist_bkd) [] 
  | interactive_rw_loop g (rw::rwlist_fwd) rwlist_bkd = 
    (
     Pretty.writeln (Pretty.str "Rendering rw") ;
     let val (rule_name, rule) = rw
	 val fused_g = Rule.add_rhs rule g 			   
	 val n = (List.length rwlist_bkd) + 1
	 val howmany = n + (List.length rwlist_fwd) -1
	 val _ = send_rewrite rule_name fused_g 
			      (Rule.get_lhs rule) 
			      (Rule.get_rhs rule) 
			      howmany n
	 val sopt = TextIO.inputLine(TextIO.stdIn) 
	 val s = (case sopt of NONE => "" | SOME s => s) 
	 val c = parse_command s 
     in 
       case c of 
	 NOOP => interactive_rw_loop g rwlist_fwd rwlist_bkd 
       | Quit => raise QUIT g
       | AbortRewrites => (Pretty.writeln (Pretty.str "abort rewrite") ;g )
       | NextRewrite => (
	 Pretty.writeln (Pretty.str "next rewrite") ; 
	 interactive_rw_loop g rwlist_fwd (rw::rwlist_bkd)
	 )
       | PrevRewrite => 
	 (Pretty.writeln (Pretty.str "PREV rewrite") ; 
	  case rwlist_bkd of 
	    [] => let val rwl = List.rev rwlist_fwd  in
		    interactive_rw_loop g [hd rwl] (tl rwl)
		  end
	  | h::t => interactive_rw_loop g (h::rw::rwlist_fwd) t 
	 )
       | DoRewrite => (Pretty.writeln (Pretty.str ("Doing rewrite"^rule_name)) ;
		       let val g' = Rule.del_lhs rule fused_g
		       in 
			 ((!dumper) g'; g')
		       end
		      )
       | _ => raise COMMAND_NOT_APPLICABLE_NOW "finish rewriting first"
     end
    )

fun interactive_rewrite g hist vset = 
    (
     Pretty.writeln  (Pretty.str "start rw loop") ;
     let fun f1 x [] = [] | f1 x (h::t) = (x,h)::(f1 x t)
	 fun f0 (x,t) = f1 x t
	 fun f2 f [] = [] | f2 f ((x,y)::t) = (x,f y)::(f2 f t)
	 val rules = rule_list_hack;  (* temporary *)
	 val mtchr = fn x => Seq.list_of (Rule.rule_matches x g)
	 val rws = Library.flat (List.map f0 (f2 mtchr rules) )
     in
       (* small bug here - updates history even if the rewrite was aborted *)
       (interactive_rw_loop g rws [], g::hist, Pretty.str("rewrite done."))
     end
)


(* =======================================================*)
(* this is the command dispatcher for the top level run loop *)
fun do_command g hist c = 
    case c of
      NOOP => (g, hist, blank)	      
    | DUMP => (g, hist, (!dumper) g)
    | Save filename => ( 
      (!write_to_file) filename g ;
      (g, hist, Pretty.str ("Wrote graph to file "^filename))
      )
    | Open _ => raise NOT_IMPLEMENTED "Open"
    | MODE => (switch_mode () ;
	       (g, hist, Pretty.str "Changed output mode.")
	      )
    | Quit => raise QUIT g
    | HELO => let val msg  = Pretty.chunks [Pretty.str"Hello from QUANTOMATIC",
					    Pretty.str "Let's have some fun!"]
	      in 
		(g, hist, msg)
	      end
    | New => (G.empty, 
	      g::hist, 
	      Pretty.str "New Graph.")
    | Undo => (case hist of 
		[] => (g, hist, blank)
	      | h::t => (h, t, Pretty.str "Undo!")
	      )
    | Red => 
      let val (_,g') = G.add_vertex fresh_red g
      in 
	(g', g::hist, Pretty.str "Red added.")
      end
    | Green => 
      let val (_,g') = G.add_vertex fresh_green g
      in 
	(g', g::hist, Pretty.str "Green added.")
      end
    | H => 
      let val (_,g') = G.add_vertex fresh_h g
      in 
	(g', g::hist, Pretty.str "H added.")
      end
    | Boundary => 
      let val (_,g') = G.add_vertex fresh_bnd g
      in 
	(g', g::hist, Pretty.str "Boundary added.")
      end
    | Edge (v1, v2) =>
      let val (_,g') = G.add_edge () v1 v2 g
      in
	(g', g::hist, Pretty.str "Edge added.")
      end 
    | DelVertex v => 
      let val g' = G.delete_vertex v g
      in 
	(g', g::hist, Pretty.str "Vertex Deleted.")
      end
    | DelEdge e => 
      let val g' = G.delete_edge e g
      in 
	(g', g::hist, Pretty.str "Edge Deleted.")
      end
    | UpVertex (v,d) => 
      let val g' = G.set_vertex d v g
      in 
	(g', g::hist, Pretty.str "Vertex Updated.")
      end
    | ShowRewrites vset => interactive_rewrite g hist vset
    | AbortRewrites => raise COMMAND_NOT_APPLICABLE_NOW "do show rewrites first"
    | DoRewrite => raise COMMAND_NOT_APPLICABLE_NOW "do show rewrites first"
    | NextRewrite => raise COMMAND_NOT_APPLICABLE_NOW "do show rewrites first"
    | PrevRewrite => raise COMMAND_NOT_APPLICABLE_NOW "do show rewrites first"
    | _ => raise NOT_IMPLEMENTED "nothing useful implemented"



(*----------------------- MAIN RUN LOOP  ----------------------------------------- *)
fun run_loop g hist = 
	  let 
            val _ = if not (!is_out_pipe) then TextIO.print "quanto:> " else ()
	    val sopt = TextIO.inputLine(TextIO.stdIn) 
            val s = (case sopt of NONE => "" | SOME s => s)
	    val (g', hist', output) = ((do_command g hist) o parse_command) s
		handle NOT_IMPLEMENTED s => 
		       (g, hist, Pretty.str ("Not Implemented:" ^ s))
		     | ILLEGAL_COMMAND s => 
		       (g, hist, Pretty.str ("Illegal command:" ^ s))
	  in
	    (
	     if output <> blank then ((!do_output) output) else () ;
	     run_loop g' hist'
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

fun init () = 
    let val quanto_dir = case OS.Process.getEnv "HOME" 
                          of NONE => ".quantomatic"
                           | SOME s => s ^ "/.quantomatic"
    in
      log := (TextIO.openAppend (quanto_dir ^ "quantolog.txt"));
      write_log "Starting up...\n" ; 
      run_loop G.empty []
    end
    handle QUIT g => (
	   Pretty.writeln (Pretty.str "Quitting...");
	   write_log "Exiting run loop";
	   TextIO.closeOut (!log);		   
	   g
	   )
    handle _ => (
	   die "ML threw an Exception... better RESTART.";
	   G.empty (* just to keep the compiler happy *)
	   )

end; 
