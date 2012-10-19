signature CONTROLLER =
sig
  structure Theory : GRAPHICAL_THEORY
  val dispatch : (string * string) * Json.json -> string * Json.json
end


functor Controller(Theory : GRAPHICAL_THEORY) =
struct

structure Theory = Theory
structure CModMain = CModMain(Theory)

val mtab = Symtab.make [
  ("Main", (CModMain.ftab, "Utility functions"))
]

exception dispatch_exn of string

fun dispatch ((module, function), json_input) =
let
  val ftab = case Symtab.lookup mtab module of SOME x => x
                | NONE => raise dispatch_exn ("Module not found: " ^ module)
  val func = case Symtab.lookup (fst ftab) function of SOME x => x
                | NONE => raise dispatch_exn ("Function not found: " ^ function)
in (fst func) json_input
end handle dispatch_exn msg => ("ERROR", Json.mk_record [("message", msg)])

end
