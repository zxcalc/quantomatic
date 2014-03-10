signature CMOD_REWRITE =
sig
  include CONTROLLER_MODULE
end

functor CModRewrite(
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata) : CMOD_REWRITE =
struct

open JsonControllerUtil

structure RewriteTable = NameTable(structure Dom = SStrName)
structure Rule = Theory.Rule
structure Graph = Theory.Graph
structure IO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = Theory
  structure GraphComponentDataIO = GraphComponentDataIO)

val rewrite_table: (((Rule.T * Graph.T) Seq.seq) RewriteTable.T) Synchronized.var =
    Synchronized.var "rewrite_table" RewriteTable.empty
  
val ftab = Symtab.empty

val ftab = ftab |> register
{
  name   = "find_rewrites",
  doc    = "Search for rewrites within the given set of vertices and\n"^
           "return a named lazy list.",
  input  = N ["arg1" -: string_t, "arg2" -: string_t],
  output = S string_t
} (fn x => (

let
  val graph = arg_json x "graph"
  val rule = arg_json x "rule"
in Json.String ""
end  
  
))


end
