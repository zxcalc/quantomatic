
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

fun addGraph io dom_element graph = 
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addGraph"
                     [jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
                      jsffi.arg.string (dot_to_svg io (Cosy.output_dot graph))])


fun addRule io dom_element name rule =
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addRule" [
                      jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
		                  jsffi.arg.string (R.string_of_name name),
                      jsffi.arg.string (dot_to_svg io (Cosy.output_dot (Cosy.Theory.Rule.get_lhs rule))),
                      jsffi.arg.string (dot_to_svg io (Cosy.output_dot (Cosy.Theory.Rule.get_rhs rule)))
                  ])

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

val gens = GHZW_Gens.gen_list 2 [GHZW_VertexData.GHZ,GHZW_VertexData.W];

val content_div = the (DOM.getElementById DOM.document "cosy_content")
                  handle Option => DOM.HTMLElement "NULL"


fun run_dot () = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
fun close_dot (ins,outs) = (TextIO.closeIn ins; TextIO.closeOut outs)


fun output_synth (synth as ((ins,outs,verts,plugs), class_tab)) = let
  val (num_classes, num_congs, num_redexes) = Cosy.Synth.stats synth
  val parent = addContainer content_div
    (Cosy.Theory.theory_name ^ " Synth ("^(Int.toString ins)^","^(Int.toString outs)^","^
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
  fun output_class (tensor, class) i = let
    val container = addContainer parent ("Class " ^ (Int.toString i)) false
    val _ = addCodebox container (Cosy.Tensor.to_string tensor)
    val c_container = addContainer container "Congruences" false
    val r_container = addContainer container "Reducible Expressions" false
    val (congruences, redexes) = (EqClass.get_congs class, EqClass.get_redexes class)
    fun output_graph len c (i, gr) = if i = 100 then (clearFloats c; addCodebox c (Int.toString (len - 100) ^ " more..."))
                                     else (if i < 100 then addGraph io c gr else c)
    val _ = case (EqClass.get_rep class) of SOME rep => (addGraph io c_container rep; ()) | NONE => ()
    val _ = map_index (output_graph (length congruences) c_container) congruences
    val _ = map_index (output_graph (length redexes) r_container) redexes
  in i+1
  end
  (*val _ = fold (output_class) ((TheoryData.get_class_list tdata) synth) 0*)
  val _ = Cosy.Synth.eqclass_fold output_class synth 0
  val _ = close_dot io
in ()
end

fun output_graph graph = let
  val c = addContainer content_div "GRAPH" false
  val io = run_dot ()
  val _ = addRule io c graph
  val _ = close_dot io
in ()
end

fun output_ruleset rs = let
  (*val rs_pairs = (TheoryData.get_rs_pairs tdata) rs*)
  val container = addContainer content_div (Cosy.Theory.theory_name ^ " Ruleset") false
  val io = run_dot ()
  val _ = R.NTab.map_all (addRule io container) (Cosy.Theory.Ruleset.get_allrules rs)
  val _ = close_dot io
in ()
end

fun output_rule rule = let
  val c = addContainer content_div "Rule" false
  val io = run_dot ()
  val _ = addRule io c (R.mk "*") rule
  val _ = close_dot io
in ()
end

fun output_string string = let
  val _ = addCodebox content_div string
in ()
end

fun output_graph graph = let
  val c = addContainer content_div "Graph" false
  val io = run_dot ()
  val _ = addGraph io c graph
  val _ = close_dot io
in ()
end

fun output_gens () = let
  val c = addContainer content_div (Cosy.Theory.theory_name ^ " Generators") false
  val io = run_dot ()
  val _ = map (fn (gr,_,_) => addGraph io c gr) (Cosy.gens)
  val _ = close_dot io
in ()
end

fun out (Cosy.SYNTH s) = output_synth s
  | out (Cosy.RS rs) = output_ruleset rs
  | out (Cosy.RULE r) = output_rule r
  | out (Cosy.GRAPH g) = output_graph g
  | out (Cosy.ERR e) = output_string e




(***********************)
(*   SYNTH FUNCTIONS   *)
(***********************)

fun synth run = Cosy.SYNTH (Cosy.Synth.synth Cosy.gens run)
fun synth_with_rs (Cosy.RS rs) run = Cosy.SYNTH (Cosy.Synth.synth_with_rs rs Cosy.gens run)

fun update (Cosy.SYNTH s) (Cosy.RS rs) = Cosy.RS (rs |> Cosy.RSBuilder.update s)
fun reduce (Cosy.RS rs) = Cosy.RS (Cosy.RSBuilder.reduce rs)

fun update_redex run rs = rs |> update (synth_with_rs rs run) |> reduce
fun update_naive run rs = rs |> update (synth run)




(*************************)
(*   RULESET FUNCTIONS   *)
(*************************)

fun size (Cosy.RS rs) = R.NTab.cardinality (Cosy.Theory.Ruleset.get_allrules rs)
fun rule_matches_rule (Cosy.RULE r1) (Cosy.RULE r2) = Cosy.RSBuilder.rule_matches_rule r1 r2

fun match_rule (Cosy.RULE rule) (Cosy.GRAPH target) = Cosy.rule_matches_graph rule target


fun get_rule (Cosy.RS rs) name =
  case Cosy.Theory.Ruleset.lookup_rule rs (R.mk name)
    of SOME r => Cosy.RULE r 
     | _ => Cosy.ERR "Rule not found."

fun get_lhs (Cosy.RULE rule) = Cosy.GRAPH (Cosy.Theory.Rule.get_lhs rule)
fun get_rhs (Cosy.RULE rule) = Cosy.GRAPH (Cosy.Theory.Rule.get_rhs rule)


val default_runs = [(0,0,3,3),(0,1,3,3),(1,0,3,3),(0,2,3,3),(1,1,3,3),
                    (2,0,3,3),(0,3,3,3),(1,2,3,3),(2,1,3,3),(3,0,3,3)]
val short_runs = [(0,0,2,2),(0,1,2,2)]
val long_runs = default_runs @ [(2,2,4,4)]

fun process updater ruleset runs = let
  fun do_update (run as (r1,r2,r3,r4)) rs = let
    val _ = output_string ("Updating for: ("^
                           Int.toString r1^","^
                           Int.toString r2^","^
                           Int.toString r3^","^
                           Int.toString r4^")...")
    val rs' = rs |> updater run
  in (size rs', rs')
  end
  val (sizes, final_rs) = fold_map do_update runs ruleset
  val _ = output_string "Done."
in (sizes, final_rs)
end

fun as_data list = fold2
  (fn i => fn d => fn str => str^"("^Int.toString i^","^Int.toString d^")\n") 
  (0 upto (length list - 1)) list ""




(********************)
(*   IO FUNCTIONS   *)
(********************)

fun escape str = let
  fun esc #"<" = "&lt;"
    | esc #">" = "&gt;"
    | esc #"&" = "&amp;"
    | esc #"\"" = "&quot;"
    | esc c = String.str c
in String.translate esc str
end

fun to_xml (Cosy.RULE rule) = Cosy.Theory.IO_Xml.Output.Rule.output rule
  | to_xml (Cosy.GRAPH graph) = Cosy.Theory.IO_Xml.Output.Graph.output graph

fun xml item = output_string (escape (XMLWriter.write_to_string (to_xml item)))

fun save file item = XMLWriter.write_to_file file (to_xml item)

fun load file = let
  val data = XMLReader.read_from_file file
in if String.isSuffix ".rule" file then
     Cosy.RULE (Cosy.Theory.IO_Xml.Input.Rule.input data)
   else if String.isSuffix ".graph" file then
     Cosy.GRAPH (Cosy.Theory.IO_Xml.Input.Graph.input data)
   else Cosy.ERR "Unknown file extension"
end







