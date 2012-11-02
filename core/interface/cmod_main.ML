signature CMOD_MAIN =
sig
  include CONTROLLER_MODULE
end

functor CModMain(Theory : GRAPHICAL_THEORY) : CMOD_MAIN =
struct

open ControllerUtil
  
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

end
