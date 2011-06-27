

fun dot_to_svg (ins,outs) dot = let
  val () = TextIO.output (outs, dot)
  (* eat the <?xml.. tag and DOCTYPE *)
  val _ = (TextIO.inputLine ins; TextIO.inputLine ins; TextIO.inputLine ins)
  fun read_all () = case TextIO.inputLine ins
                      of SOME "</svg>\n" => "</svg>\n"
                       | SOME ln => ln ^ (read_all ())
                       | NONE => ""
  val svg = read_all ()
in svg
end

fun addGraph tdata io dom_element graph = 
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addGraph"
                     [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
                      jsffi.arg.string (dot_to_svg io ((TheoryData.get_dotfun tdata) graph))])


fun addRule tdata io dom_element name (lhs,rhs) =
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addRule"
                     [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
		      jsffi.arg.string (RuleName.string_of_name name),
                      jsffi.arg.string (dot_to_svg io ((TheoryData.get_dotfun tdata) lhs)),
                      jsffi.arg.string (dot_to_svg io ((TheoryData.get_dotfun tdata) rhs))])

fun addContainer dom_element title expanders =
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addContainer"
                     [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
                      jsffi.arg.string title,
                      jsffi.arg.bool expanders])

fun collapseContainer dom_element =
  jsffi.exec_js "window|" "collapseContainer" [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element)]
                      
fun addCodebox dom_element text =
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addCodebox"
                     [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
                      jsffi.arg.string text])
                      
fun clearFloats dom_element =
                  jsffi.exec_js "window|" "clearFloats"
                   [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element)]

val gens = GHZW_Gens.gen_list 3 [GHZW_VertexData.GHZ,GHZW_VertexData.W];

val content_div = the (DOM.getElementById DOM.document "cosy_content")
                  handle Option => DOM.HTMLElement "NULL"


fun run_dot () = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
fun close_dot (ins,outs) = (TextIO.closeIn ins; TextIO.closeOut outs)


fun output_synth tdata (synth as ((ins,outs,verts,plugs), class_tab)) = let
  val (num_classes, num_congs, num_redexes) = (TheoryData.get_stats tdata) synth
  val parent = addContainer content_div
    ((TheoryData.get_name tdata) ^ " SYNTH ("^(Int.toString ins)^","^(Int.toString outs)^","^
               (Int.toString verts)^","^(Int.toString plugs)^")") true
  val details =
    "SYNTHESIS RESULTS\n"^
    "-----------------------------------------\n"^
    "  "^(Int.toString ins)^" inputs\n"^
    "  "^(Int.toString outs)^" outputs\n"^
    "  "^(Int.toString verts)^" max vertices\n"^
    "  "^(Int.toString plugs)^" max pluggings\n"^
    "-----------------------------------------\n"^
    "  Found "^(Int.toString num_classes)^" equivalence classes.\n"^
    "  Average class size: "^(Int.toString ((num_congs + num_redexes) div num_classes))^".\n"^
    "-----------------------------------------\n"
  val io = run_dot ()
  val _ = addCodebox parent details
  fun output_class (tensor_string, class) i = let
    val container = addContainer parent ("Class " ^ (Int.toString i)) false
    val _ = addCodebox container tensor_string
    val c_container = addContainer container "Congruences" false
    val r_container = addContainer container "Reducible Expressions" false
    val (congruences, redexes) = (EqClass.get_congs class, EqClass.get_redexes class)
    fun output_graph len c (i, gr) = if i = 100 then (clearFloats c; addCodebox c (Int.toString (len - 100) ^ " more..."))
                                     else (if i < 100 then addGraph tdata io c gr else c)
    val _ = case (EqClass.get_rep class) of SOME rep => (addGraph tdata io c_container rep; ()) | NONE => ()
    val _ = map_index (output_graph (length congruences) c_container) congruences
    val _ = map_index (output_graph (length redexes) r_container) redexes
  in i+1
  end
  val _ = fold (output_class) ((TheoryData.get_class_list tdata) synth) 0
  val _ = close_dot io
in ()
end

fun output_ruleset tdata rs = let
  val rs_pairs = (TheoryData.get_rs_pairs tdata) rs
  val container = addContainer content_div ((TheoryData.get_name tdata) ^ " RULESET") false
  val io = run_dot ()
  val _ = RuleName.NTab.map_all (addRule tdata io container) rs_pairs
  val _ = close_dot io
in ()
end

fun output_rule tdata lhs rhs = let
  val c = addContainer content_div "RULE" false
  val io = run_dot ()
  val _ = addRule tdata io c (RuleName.mk "*") (lhs,rhs)
  val _ = close_dot io
in ()
end

fun output_string string = let
  val _ = addCodebox content_div string
in ()
end

fun output_graph tdata graph = let
  val c = addContainer content_div "GRAPH" false
  val io = run_dot ()
  val _ = addGraph tdata io c graph
  val _ = close_dot io
in ()
end

fun output_gens tdata = let
  val c = addContainer content_div (TheoryData.get_name tdata ^ " Generators") false
  val io = run_dot ()
  val _ = map (fn (gr,_,_) => addGraph tdata io c gr) (TheoryData.get_gens tdata)
  val _ = close_dot io
in ()
end








