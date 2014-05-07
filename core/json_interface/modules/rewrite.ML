signature CMOD_REWRITE =
sig
  include CONTROLLER_MODULE
end

functor CModRewrite(
  structure Theory : GRAPHICAL_THEORY
) : CMOD_REWRITE =
struct

open JsonControllerUtil

structure RewriteTable = NameTable(structure Dom = SStrName)
structure Rule = Theory.Rule
structure Graph = Theory.Graph

val rewrite_table: (((Rule.T * Graph.T) Seq.seq) RewriteTable.T) Synchronized.var =
    Synchronized.var "rewrite_table" RewriteTable.empty
  
val ftab = Symtab.empty

val ftab = ftab |> register
{
  name   = "find_rewrites",
  doc    = "Search for rewrites within the given set of vertices and\n"^
           "return a named lazy list.",
  input  = N ["graph" -: graph_t, "rule" -: rule_t, "vertices" -: list_t vertexname_t],
  output = S string_t
} (fn x => (

let
  val graph = Theory.GraphJSON.input (arg_json x "graph")
  val rule = Theory.RuleJSON.input (arg_json x "rule")
  val vertices = case arg_json x "vertices"
                   of Json.Array vs =>
                        map (fn v => case v of Json.String s => V.mk s
                                             | _ => raise user_exn "expected vertex name") vs
                    | _ => raise user_exn "expected array of vertex names"
  val stack = Synchronized.change_result rewrite_table (RewriteTable.store (
    Theory.Rewriter.find_rewrites rule graph (*(V.NSet.of_list vertices)*)
  ))
in Json.String (SStrName.dest stack)
end  
  
))

val ftab = ftab |> register
{
  name   = "pull_rewrite",
  doc    = "Pull a rewrite from the given stack. Return null if\n"^
           "stack is empty or doesn't exist.",
  input  = N ["stack" -: string_t],
  output = S json_t
} (fn x => (

let
  val stack = SStrName.mk (arg_str x "stack")
  val nextOpt = Synchronized.change_result rewrite_table (fn tab => (
    case RewriteTable.get_opt tab stack
      of SOME rws => (
           case Seq.pull rws
             of SOME ((rule, graph), rws') =>
                  (SOME (rule, graph), tab |> RewriteTable.set (stack, rws'))
              | NONE =>
                  (NONE, tab |> RewriteTable.delete stack)
           )
       | NONE => (NONE, tab)
  ))
in case nextOpt
     of SOME (rule, graph) =>
          Json.mk_object [
            ("graph", Theory.GraphJSON.output graph),
            ("rule", Theory.RuleJSON.output rule)
          ]
      | NONE => Json.Null
end  
  
))

val ftab = ftab |> register
{
  name   = "delete_rewrite_stack",
  doc    = "Delete a rewrite stack.",
  input  = N ["stack" -: string_t],
  output = S string_t
} (fn x => (

let
  val stack = SStrName.mk (arg_str x "stack")
  val _ = Synchronized.change rewrite_table (RewriteTable.delete stack)
in Json.String "OK"
end  
  
))


end
