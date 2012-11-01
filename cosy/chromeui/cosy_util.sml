functor CosyUtil(
  structure Enum : GRAPH_ENUM
  val data_list : Enum.Theory.OVData.IData.data list
  val output_dot : Enum.Theory.Graph.T -> string
) =
struct

structure Enum = Enum
structure Theory = Enum.Theory
structure Spiders = SpiderRewrites(structure Theory = Theory)

fun gen_list max_arity data_list = let
    fun alist 0 0 = []
      | alist k 0 = (0,k)::alist (k-1) (k-1)
      | alist k i = (i,k-i)::alist k (i-1)
    fun gen d (iw,ow) = (Theory.OVData.NVert d,iw,ow)
  in (fold_product (cons oo gen) data_list (alist max_arity max_arity) [])
  end

val gens = gen_list 3 data_list

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
                      jsffi.arg.string (dot_to_svg io (output_dot graph))])


fun addRule io dom_element name rule =
  DOM.HTMLElement (jsffi.exec_js_r "window|" "addRule" [
                      jsffi.arg.reference (DOM.fptr_of_HTMLElement dom_element),
                      jsffi.arg.string (R.string_of_name name),
                      jsffi.arg.string (dot_to_svg io (output_dot (Theory.Rule.get_lhs rule))),
                      jsffi.arg.string (dot_to_svg io (output_dot (Theory.Rule.get_rhs rule)))
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

(*val content_div = ref (DOM.HTMLElement "NULL")*)


fun run_dot () = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
fun close_dot (ins,outs) = (TextIO.closeIn ins; TextIO.closeOut outs)


fun output_graph content_div graph = let
  val c = addContainer content_div "GRAPH" false
  val io = run_dot ()
  val _ = addRule io c graph
  val _ = close_dot io
in ()
end

fun output_ruleset content_div rs = let
  (*val rs_pairs = (TheoryData.get_rs_pairs tdata) rs*)
  val container = addContainer content_div (Theory.theory_name ^ " Ruleset") false
  val io = run_dot ()
  val _ = R.NTab.map_all (addRule io container) (Theory.Ruleset.get_allrules rs)
  val _ = close_dot io
in ()
end

fun output_rule content_div rule = let
  val c = addContainer content_div "Rule" false
  val io = run_dot ()
  val _ = addRule io c (R.mk "*") rule
  val _ = close_dot io
in ()
end

fun output_string content_div string = let
  val _ = addCodebox content_div string
in ()
end

fun output_graph content_div graph = let
  val c = addContainer content_div "Graph" false
  val io = run_dot ()
  val _ = addGraph io c graph
  val _ = close_dot io
in ()
end

val initial_rs = Spiders.ruleset_from_vdata data_list

fun synth content_div sz =
let
  val eqt = Enum.tab_update gens sz (Enum.EqClassTab.mk initial_rs)
in
  output_ruleset
    content_div
    (Enum.EqClassTab.get_ruleset eqt)
end


end

val rg_data_list = [RG_InternVData.Xnd LinratAngleExpr.zero,
                    RG_InternVData.Znd LinratAngleExpr.zero]
structure RGCosy = CosyUtil(
  structure Enum = RG_Enum
  val data_list = rg_data_list
  val output_dot = RG_GraphicalTheoryIO.OutputGraphDot.output
)

(*local
  open RGCosy
in
fun rg_synth content_div sz =
  output_ruleset content_div
    (RSBuilder.get_ruleset
      (Enum.tab_update gens sz
        (RG_Spiders.eq_class_tab rg_data_list)))
end*)