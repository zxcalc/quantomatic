signature CMOD_SIMPLIFY =
sig
  include CONTROLLER_MODULE
  structure Theory : GRAPHICAL_THEORY
  type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
  type simproc = Theory.Graph.T -> dstep Seq.seq
  val register_simproc : string * simproc -> unit
end

functor CModSimplify(
  structure Theory : GRAPHICAL_THEORY
) : CMOD_SIMPLIFY =
struct

open JsonControllerUtil

structure Theory = Theory

structure SimpTable = NameTable(structure Dom = SStrName)
structure Rule = Theory.Rule
structure Graph = Theory.Graph

type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
type simproc = Theory.Graph.T -> dstep Seq.seq

val simproc_table: (simproc SimpTable.T) Synchronized.var =
    Synchronized.var "simproc_table" SimpTable.empty

val simp_table: ((dstep Seq.seq) SimpTable.T) Synchronized.var =
    Synchronized.var "simproc_table" SimpTable.empty

fun register_simproc (name, simproc) =
  Synchronized.change simproc_table (SimpTable.set (SStrName.mk name, simproc))

val ftab = Symtab.empty

val ftab = ftab |> register
{
  name   = "simplify",
  doc    = "Simplifies with the given simproc. Returns a stack name\n"^
           "where steps can be pulled.",
  input  = N ["graph" -: graph_t, "simproc" -: string_t],
  output = S string_t
} (fn x => (

let
  val graph = Theory.GraphJSON.input (arg_json x "graph")
  val simproc = SimpTable.get (Synchronized.value simproc_table) (SStrName.mk (arg_str x "simproc"))
  val stack = Synchronized.change_result simp_table (SimpTable.store (simproc graph))
in Json.String (SStrName.dest stack)
end  
  
))

val ftab = ftab |> register
{
  name   = "list",
  doc    = "Lists all of the registered simprocs.",
  input  = N [],
  output = S (list_t string_t)
} (fn x => (

let
  val simprocs = SimpTable.get_dom_set (Synchronized.value simproc_table)
in Json.Array (map (Json.String o SStrName.dest) (SStrName.NSet.list_of simprocs))
end  
  
))

val ftab = ftab |> register
{
  name   = "pull_next_step",
  doc    = "Pull a rewrite from the given stack. Return null if\n"^
           "stack is empty or doesn't exist.",
  input  = N ["stack" -: string_t],
  output = S json_t
} (fn x => (

let
  val stack = SStrName.mk (arg_str x "stack")
  val nextOpt = Synchronized.change_result simp_table (fn tab => (
    case SimpTable.get_opt tab stack
      of SOME steps => (
           case Seq.pull steps
             of SOME (step, steps') =>
                  (SOME step, tab |> SimpTable.set (stack, steps'))
              | NONE =>
                  (NONE, tab |> SimpTable.delete stack)
           )
       | NONE => (NONE, tab)
  ))
in case nextOpt
     of SOME ((rname, rule), graph) =>
          Json.mk_object [
            ("rule_name", Json.String (R.dest rname)),
            ("rule", Theory.RuleJSON.output rule),
            ("graph", Theory.GraphJSON.output graph)
          ]
      | NONE => Json.Null
end  
  
))

val ftab = ftab |> register
{
  name   = "delete_stack",
  doc    = "Delete a stack of simplification steps.",
  input  = N ["stack" -: string_t],
  output = S string_t
} (fn x => (

let
  val stack = SStrName.mk (arg_str x "stack")
  val _ = Synchronized.change simp_table (SimpTable.delete stack)
in Json.String "OK"
end  
  
))


end
