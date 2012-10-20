signature CONTROLLER =
sig
  structure Theory : GRAPHICAL_THEORY
  val dispatch : (string * string) * Json.json -> string * Json.json
end


functor Controller(Theory : GRAPHICAL_THEORY) =
struct

open ControllerUtil

structure Theory = Theory
structure CModMain = CModMain(Theory)

val mtab = Symtab.make [
  ("Main", (CModMain.ftab, "Utility functions"))
]

fun help x = let
  val module = arg_str x "module"
  val opt_function = optarg_str x "function"
  val modl = case Symtab.lookup mtab module of SOME x => x
                  | NONE => raise protocol_exn ("Module not found: " ^ module)
in
	case opt_function of NONE => ("OK", Json.String (snd modl))
	   | SOME function => (
	   	   let
	   	     val func = case Symtab.lookup (fst modl) function of SOME x => x
                      | NONE => raise protocol_exn ("Function not found: " ^ module ^ "::" ^ function)
         in ("OK", Json.String (snd func))
         end)
end

fun dispatch (("!!", function), json_input) = 
      ((case function
      	  of "help"    => help json_input
      	   | "version" => ("OK", Json.String "2.01")
	         | _         => raise protocol_exn ("Function not found: !!::" ^ function))
       handle protocol_exn msg => ("ERROR", Json.mk_record [("message", msg)]))
  | dispatch ((module, function), json_input) =
      let
        val modl = case Symtab.lookup mtab module of SOME x => x
                   | NONE => raise protocol_exn ("Module not found: " ^ module)
        val func = case Symtab.lookup (fst modl) function of SOME x => x
                   | NONE => raise protocol_exn ("Function not found: " ^ module ^ "::" ^ function)
      in (fst func) json_input
      end
      handle protocol_exn msg => ("ERROR", Json.mk_record [("message", msg)])

end
