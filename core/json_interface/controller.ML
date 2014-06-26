signature JSON_CONTROLLER =
sig
  structure Theory : GRAPHICAL_THEORY
  val dispatch : (string * string) * Json.json -> Json.json
  type simproc = Theory.Graph.T -> ((R.name * Theory.Rule.T) * Theory.Graph.T) Seq.seq
  val register_simproc : string * simproc -> unit
end


functor JsonController(
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata) =
struct

open JsonControllerUtil

structure Theory = Theory
structure CModTest = CModTest(
  structure Theory = Theory
  structure GraphComponentDataIO = GraphComponentDataIO)

structure CModRewrite = CModRewrite(structure Theory = Theory)
structure CModSimplify = CModSimplify(structure Theory = Theory)

type simproc = CModSimplify.simproc
val register_simproc = CModSimplify.register_simproc

val mtab = Symtab.make [
  ("test", (CModTest.ftab, "Test functions")),
  ("rewrite", (CModRewrite.ftab, "Graph rewriting")),
  ("simplify", (CModSimplify.ftab, "Graph simprocs"))
]

end
