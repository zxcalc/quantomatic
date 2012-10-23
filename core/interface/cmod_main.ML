signature CMOD_MAIN =
sig
  include CONTROLLER_MODULE
end

functor CModMain(Theory : GRAPHICAL_THEORY) : CMOD_MAIN =
struct
  open ControllerUtil
  
  val ftab = Symtab.empty
  
  val ftab = ftab |> register
  {
    name = "echo",
    doc = "Echoes JSON input",
    input = S json_t,
    output = S json_t
  } (fn x => (
    x
  ))
  
end
