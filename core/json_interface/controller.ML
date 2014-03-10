signature JSON_CONTROLLER =
sig
  structure Theory : GRAPHICAL_THEORY
  val dispatch : (string * string) * Json.json -> Json.json
end


functor JsonController(
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata) =
struct

open JsonControllerUtil

structure Theory = Theory
structure CModTest = CModTest(Theory)
structure CModRewrite = CModRewrite(
	structure Theory = Theory
	structure GraphComponentDataIO = GraphComponentDataIO)

val mtab = Symtab.make [
  ("test", (CModTest.ftab, "Test functions")),
  ("rewrite", (CModRewrite.ftab, "Graph rewriting"))
]

end
