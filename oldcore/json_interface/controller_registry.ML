signature JSON_CONTROLLER_REGISTRY =
sig
  (*
    dispatch takes:
       ((controller, module, function), JSON input)
    and returns:
       JSON output
  *)
  val dispatch : (string * string * string) * Json.json -> Json.json
end

structure JsonControllerRegistry (*: JSON_CONTROLLER_REGISTRY*) =
struct

open JsonControllerUtil

structure RG_Controller = JsonController(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO
)

structure GHZW_Controller = JsonController(
  structure Theory = GHZW_Theory
  structure GraphComponentDataIO = GHZW_ComponentDataIO
)

val ctab = Symtab.make [
 (RG_Controller.Theory.theory_name, RG_Controller.mtab),
 (GHZW_Controller.Theory.theory_name, GHZW_Controller.mtab)
]

fun dispatch ((controller, module, function), json_input) =
let
  val mtab = case Symtab.lookup ctab controller of SOME x => x
                | NONE => raise user_exn ("Controller not found: " ^ controller)
  val modl = case Symtab.lookup mtab module of SOME x => x
             | NONE => raise user_exn ("Module not found: " ^ module)
  val func = case Symtab.lookup (fst modl) function of SOME x => x
             | NONE => raise user_exn ("Function not found: " ^ module ^ "." ^ function)
in (fst func) json_input
end

end