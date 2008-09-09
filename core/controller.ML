(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Console based controller for graph manipulations *)
(* based on the IsaPlanner interface *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Controller =
struct

structure G = RGGraph
structure V = G.RGVertex
structure E = G.Edge

exception quit_exp of G.T

structure MkV 
= struct
  val red = V.IVert (V.Xnd AngleExpr.zero);
  val green = V.IVert (V.Znd AngleExpr.zero);
  val h = V.IVert V.Hnd;
  val bnd = V.BVert ()
end;

val log = ref TextIO.stdErr;
val log_stderr = ref true;
fun close_log () = (if !log_stderr then () else TextIO.closeOut (!log));
fun write_log s = (TextIO.output ((!log), "<!-- Log: " ^ s ^ " -->\n"); TextIO.flushOut (!log)) ;

(* what language are we outputting our graphs ----------------------------- *)
datatype language = DOT | XML;
val dump_lang = ref XML;
val pretty_graph = ref RGGraphXML.output_graph
val write_graph_to_file = ref RGGraphXMLWriter.output_to_file

(* are we talking to a toplevel or a pipe? -------------------------------- *)
val is_out_pipe = ref true;
fun out_toplevel out = Pretty.writeln out     
fun out_pipe out = 
    (Pretty.writeln out;
     Pretty.writeln (Pretty.str "stop"))
val do_output = ref out_pipe;       
fun switch_mode () = 
    if !is_out_pipe 
    then (do_output := out_toplevel ; is_out_pipe := false)
    else (do_output := out_pipe ; is_out_pipe := true);



(* -- this doesn't belong here but it'll do for now -- *)
(* args =  rule_name fused lhs rhs new_vnames n howmany  *)
fun pretty_rewrite rule_name fused lhs rhs howmany n  = 
    Pretty.block 
      [(Pretty.str ("<rewrite>\n<rulename>\n"^rule_name^"\n</rulename>\n")),
       (!pretty_graph) fused,
       (Pretty.str ("<lhs>\n")), 
       (!pretty_graph) lhs, 
       (Pretty.str ("</lhs>\n<rhs>")), 
       (!pretty_graph) rhs, 
       (Pretty.str ("</rhs>\n<total>"^ (Int.toString howmany) ^ "</total>\n" ^
                                 "<index>"^ (Int.toString n) ^ "</index>\n" ^
                                 "</rewrite>"))];


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
      | Hadamard
      | Boundary
      | Edge of V.name * V.name
      | Angle of V.name * AngleExpr.T (* set angle expr of vertex *)
      | ShowRewrites of V.NSet.T
      | AbortRewrites (* these require some state*)
      | DoRewrite     (* which only exists after ShowRewrites *)
      | NextRewrite   (* has been called *)
      | PrevRewrite   
      | DelVertex of V.name
      | UpVertex of V.name * V.data (* update vertex value/content/data etc *)
      | DelEdge of E.name
      | BadCommand of string


fun pretty_command NOOP = Pretty.str "NOOP"
  | pretty_command Quit = Pretty.str "QUIT"
  | pretty_command HELO = Pretty.str "HELO"
  | pretty_command MODE = Pretty.str "MODE"
  | pretty_command DUMP = Pretty.str "DUMP"
  | pretty_command New = Pretty.str "New"
  | pretty_command (Open s) = 
    Pretty.block [Pretty.str "Open(", Pretty.str s, Pretty.str ")"]
  | pretty_command (Save s) = 
    Pretty.block [Pretty.str "Save(", Pretty.str s, Pretty.str ")"]
  | pretty_command Undo = Pretty.str "Undo"
  | pretty_command Red = Pretty.str "Red"
  | pretty_command Green = Pretty.str "Green"
  | pretty_command Hadamard = Pretty.str "Hadamard"
  | pretty_command Boundary = Pretty.str "Boundary"
  | pretty_command (Edge (vn1,vn2)) = 
    Pretty.block [Pretty.str "Edge(", V.pretty_name vn1, V.pretty_name vn2, Pretty.str ")"]
  | pretty_command (Angle (vn, expr)) = 
    Pretty.block [Pretty.str "Angle(", V.pretty_name vn, AngleExpr.pretty expr, Pretty.str ")"]
  | pretty_command (ShowRewrites vset) = 
    Pretty.block [Pretty.str "ShowRewrites(", V.NSet.pretty vset, Pretty.str ")"]
  | pretty_command AbortRewrites = Pretty.str "AbortRewrites"
  | pretty_command DoRewrite = Pretty.str "DoRewrite"
  | pretty_command NextRewrite = Pretty.str "NextRewrite"
  | pretty_command PrevRewrite = Pretty.str "PrevRewrite"
  | pretty_command (DelVertex vn) = 
    Pretty.block [Pretty.str "DelVertex(", V.pretty_name vn, Pretty.str ")"]
  | pretty_command (UpVertex (vn, d)) = 
    Pretty.block [Pretty.str "UpVertex(", V.pretty_name vn, V.pretty_data d, Pretty.str ")"]
  | pretty_command (DelEdge en) = 
    Pretty.block [Pretty.str "DelEdge(", E.pretty_name en, Pretty.str ")"]
  | pretty_command (BadCommand s) = 
    Pretty.block [Pretty.str "BadCommand(", Pretty.str s, Pretty.str ")"];

fun parse_vname_list [] = V.NSet.empty
  | parse_vname_list (h::t) = V.NSet.ins_fresh h (parse_vname_list t)

(* does not parse angleexpr yet *)
fun parse_vertex_data ["true"] = SOME MkV.bnd
  | parse_vertex_data ["false", "red"] = SOME MkV.red
  | parse_vertex_data ["false", "green"] = SOME MkV.green
  | parse_vertex_data ["false", "h"] = SOME MkV.h
  | parse_vertex_data _ = NONE;

(* convert a command to a token list *)
val tokenise_command = 
    String.tokens (fn #" " => true 
                    | #"\t" => true 
                    | #"\r" => true 
                    | #"\n" => true 
                    | _ => false);

fun parse_command s = 
    let 
      (* change returns to spaces makes everything cleaner to debug *)
      val s = String.translate (fn #"\n" => " " | c => Char.toString c) s;
      val _ = write_log ("Command-string: " ^ s);
      val toks = tokenise_command s
    in
      if s = "" then Quit else
      case toks 
       of [] => NOOP
        | cmd::args => 
          (case cmd of 
             "Q" => Quit 
           | "M" => MODE (* switch between output modes *)
           | "H" => HELO (*print staus *)
           | "D" => DUMP (* output the current graph *)
           | "S" => (case args of [n] => Save n | _ => BadCommand s)
           | "O" => (case args of [n] => Open n | _ => BadCommand s)
           | "u" => Undo
           | "n" => New (* new graph *)
           | "r" => Red 
           | "g" => Green
           | "h" => Hadamard
           | "b" => Boundary
           | "e" => (case args of [n1,n2] => Edge (n1,n2) | _ => BadCommand s)
           | "d" => (case args of [n] => DelVertex n | _ => BadCommand s)
           | "de" => (case args of [n] => DelEdge n | _ => BadCommand s)
           | "up" => (case args of 
                        (n::t) => (case parse_vertex_data t 
                                    of NONE => BadCommand s
                                     | SOME d => UpVertex (n, d))
                      | _ => BadCommand s)
           | "RWshow" => ShowRewrites (parse_vname_list args)
           | "RWyes" => DoRewrite
           | "RWnext" => NextRewrite
           | "RWprev" => PrevRewrite
           | "RWabort" => AbortRewrites
           | _ => BadCommand s)
    end;
    
(* ------- a mini run loop / dispatcher for choosing rewrites ---*)

fun interactive_rw_loop g [] [] = 
    (write_log "No applicable rewrites"; 
     (!do_output)
       (Pretty.str ("<rewrite><total>"^ (Int.toString 0) ^ "</total></rewrite>"));
     NONE)
  | interactive_rw_loop g [] rwlist_bkd = 
    interactive_rw_loop g (List.rev rwlist_bkd) [] 
  | interactive_rw_loop g ((rw as (rule_name, rule, fused_g))::rwlist_fwd) 
                        rwlist_bkd = 
    let 
         val n = (List.length rwlist_bkd) + 1
         val howmany = n + (List.length rwlist_fwd) -1
         val p = pretty_rewrite rule_name fused_g 
                                (Rule.get_lhs rule) 
                                (Rule.get_rhs rule) 
                                howmany n
         val _ = (!do_output) p
         val sopt = TextIO.inputLine(TextIO.stdIn) 
         val s = (case sopt of NONE => "" | SOME s => s) 
         val c = parse_command s 
     in 
       case c of 
         NOOP => interactive_rw_loop g (rw::rwlist_fwd) rwlist_bkd 
       | Quit => raise quit_exp g
       | AbortRewrites => (write_log "aborted rewrite"; NONE)
       | NextRewrite => 
         (write_log "next rewrite"; 
          interactive_rw_loop g rwlist_fwd (rw::rwlist_bkd))
       | PrevRewrite => 
         (write_log "prev rewrite"; 
          case rwlist_bkd of 
            [] => let val rwl = List.rev rwlist_fwd 
                  in interactive_rw_loop g [hd rwl] (tl rwl) end
          | h::t => interactive_rw_loop g (h::rw::rwlist_fwd) t 
         )
       | DoRewrite => (write_log ("Doing rewrite"^rule_name);
                       let val g' = Rule.del_lhs rule fused_g
                       in SOME g' end)
       | _ => 
         (write_log ("COMMAND_NOT_APPLICABLE_NOW: " ^ 
                     (Pretty.string_of (pretty_command c))); 
          interactive_rw_loop g (rw::rwlist_fwd) rwlist_bkd)
     end;

(* FIXME: use vset as selected vertices for rewriting  *)
fun interactive_rewrite vset (st as (g, hist)) = 
    let 
      val rws = 
          maps (fn (rulename,rule) => 
                   (map (fn rule' => (rulename, rule', Rule.add_rhs rule' g))
                        (Seq.list_of (Rule.rule_matches rule g))))
               TheoryHack.rules
    in case interactive_rw_loop g rws [] of 
         NONE => st
       | SOME g' => (g', g::hist)
    end;

(* =======================================================*)
(* this is the command dispatcher for the top level run loop *)
fun do_command (st as (g, hist)) c = 
    (write_log ("doing command: " ^ (Pretty.string_of (pretty_command c)));
     case c of
       NOOP => st
     | DUMP => ((!do_output) ((!pretty_graph) g); st)
     | Save filename => ((!write_graph_to_file) filename g;st)
     | MODE => (switch_mode (); st)
     | Quit => raise quit_exp g
     | HELO => ((!do_output) (Pretty.str "Hello from QUANTOMATIC\nLet's have some fun!"); st)
     | New => ((G.empty, g::hist))
     | Undo => (case hist of 
                  [] => (write_log "Already at start of history. "; st)
                | h::t => (h, t))
     | Red => 
       let val (_,g') = G.add_vertex MkV.red g
       in (g', g::hist) end
     | Green => 
       let val (_,g') = G.add_vertex MkV.green g
       in (g', g::hist) end
     | Hadamard => 
       let val (_,g') = G.add_vertex MkV.h g
       in (g', g::hist) end
     | Boundary => 
       let val (_,g') = G.add_vertex MkV.bnd g
       in (g', g::hist) end
     | Edge (v1, v2) =>
       let val (_,g') = G.add_edge () v1 v2 g
       in (g', g::hist) end 
     | DelVertex v => 
       let val g' = G.delete_vertex v g
       in (g', g::hist) end
     | DelEdge e => 
       let val g' = G.delete_edge e g
       in (g', g::hist) end
     | UpVertex (v,d) => 
       let val g' = G.set_vertex d v g
       in (g', g::hist) end
     | ShowRewrites vset => interactive_rewrite vset st
     | _ => (write_log ("ERROR: Not Implemented! ignoring command: " ^
                        (Pretty.string_of (pretty_command c))); st));


(* *)
fun start_logfile () = 
    let 
      val quanto_dir = case OS.Process.getEnv "HOME" 
                        of NONE => ".quantomatic"
                         | SOME s => s ^ "/.quantomatic"
    in
      (if ((OS.FileSys.isDir quanto_dir) handle OS.SysErr _ => false) 
       then () else OS.FileSys.mkDir quanto_dir; 
       log := (TextIO.openAppend (quanto_dir ^ "/quantolog.txt")))
      handle OS.SysErr (s,erropt) => 
             (log := TextIO.stdErr;
              write_log ("Cannot create log dir and logfile: " ^ 
                         quanto_dir ^ "/quantolog.txt" ^ "\n Error: " ^ 
                         s))
    end;

(*----------------------- MAIN RUN LOOP  ----------------------------------------- *)
fun run_loop st = 
    let 
      val _ = if not (!is_out_pipe) then TextIO.print "quanto:> " else ()
      val sopt = TextIO.inputLine(TextIO.stdIn) 
      val s = (case sopt of NONE => "" | SOME s => s)
    in run_loop (do_command st (parse_command s)) end;

(* start quantomatic! *)
fun init () = 
    let 
      val _ = log := TextIO.stdErr
      val _ = write_log "Starting up..." ; 
      val outg = run_loop (G.empty, []);
      val _ = write_log "Closing down..." ; 
      val _ = close_log ();
    in outg end
    handle quit_exp g => 
           (write_log "Quit command received...";
            write_log "Exited run loop";
            close_log (); g)
    handle e => 
           (write_log ("Crash --> exception thrown: " ^ (PolyML.makestring e));
            close_log (); 
            (!do_output) (Pretty.str ("<message><crash>\nException thrown: "^
                                      (PolyML.makestring e)^
                                      "\n</crash></message>")) ;
            G.empty);


(* function to change to using DOT output for graphs *)
fun switch_pretty_lang () = 
    case !dump_lang of DOT => (dump_lang := XML ; 
             pretty_graph := RGGraphXML.output_graph ;
             write_graph_to_file := RGGraphXMLWriter.output_to_file
            )
         | XML => (dump_lang := DOT ; 
             pretty_graph := RGGraphDot.output_graph ;
             write_graph_to_file := RGGraphDotWriter.output_to_file)

end; 
