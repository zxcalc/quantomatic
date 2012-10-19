signature CONTROLLER_REGISTRY =
sig
  (*
    dispatch takes:
       ((controller, module, function), JSON input)
    and returns:
       (status, JSON output)
  *)
  val dispatch : (string * string * string) * Json.json -> string * Json.json
end

structure ControllerRegistry =
struct

structure RG_Controller = Controller(RG_Theory)

val ctab = Symtab.make [
 (RG_Controller.Theory.theory_name, RG_Controller.dispatch)
]

fun dispatch ((controller, module, function), json_input) =
  case Symtab.lookup ctab controller
    of SOME cdisp => cdisp ((module, function), json_input)
     | NONE => ("ERROR", Json.mk_record [("message", "Controller not found: " ^ controller)])

    
end