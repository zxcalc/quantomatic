signature CONTROLLER_REGISTRY =
sig
  val ctab : controller_moduletab Symtab.table
  (*
    dispatch takes:
       ((controller, module, function), JSON input)
    and returns:
       (status, JSON output)
  *)
  val dispatch : (string * string * string * string) * Json.json -> string * Json
end

structure ControllerRegistry =
struct

structure RG_Controller = Controller(RG_Theory)

val ctab = Symtab.make [
 (RG_Controller.Theory.theory_name, RG_Controller.mtab)
]
  
fun dispatch ((controller, module, function), json_input) = ("OK", Json.Null)
    
end