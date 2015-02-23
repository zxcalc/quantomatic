signature CMOD_TEST =
sig
  include CONTROLLER_MODULE
end

functor CModTest(
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata
) : CMOD_TEST =
struct

open JsonControllerUtil

structure IO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = Theory
  structure GraphComponentDataIO = GraphComponentDataIO)

  
val ftab = Symtab.empty

(* test function *)
val ftab = ftab |> register
{
  name   = "echo",
  doc    = "Echoes JSON input",
  input  = S json_t,
  output = S json_t
} (fn x => (
  x
))

(* test graph parsing *)
val ftab = ftab |> register
{
  name   = "echo_graph",
  doc    = "Echoes graph as JSON",
  input  = S graph_t,
  output = S graph_t
} (fn x => (
  
let
  val graph = IO.InputGraphJSON.input x
                   handle IO.InputGraphJSON.bad_input_exp (error , item) =>
                     raise user_exn ("Error parsing graph/" ^ item ^" : " ^ error)
  val json = IO.OutputGraphJSON.output graph
in
  json
end

))

(* test named args *)
val ftab = ftab |> register
{
  name   = "concat",
  doc    = "Concatenates the given arguments",
  input  = N ["arg1" -: string_t, "arg2" -: string_t],
  output = S string_t
} (fn x => (

let
  val s1 = arg_str x "arg1"
  val s2 = arg_str x "arg2"
in Json.String (s1 ^ s2)
end  
  
))

(* for testing job cancellation *)
val ftab = ftab |> register
{
  name   = "diverge",
  doc    = "Run until cancelled",
  input  = N [],
  output = S string_t
} (fn x => (

let
  fun loop () = loop ()
in (loop (); Json.Null)
end  
  
))

end
