functor CosyUtil(
  structure Enum : GRAPH_ENUM
  val output_dot : Enum.Theory.Graph.T -> string
) =
struct

structure Theory = Enum.Theory

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

val content_div = the (DOM.getElementById DOM.document "cosy_content")
                  handle Option => DOM.HTMLElement "NULL"


fun run_dot () = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
fun close_dot (ins,outs) = (TextIO.closeIn ins; TextIO.closeOut outs)

end
