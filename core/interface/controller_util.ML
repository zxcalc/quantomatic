structure ControllerUtil =
struct

exception protocol_exn of string

(* {{{ JSON argument helpers *)

fun optarg_str x arg =
let
  val obj = case x of Json.Object y => y | _ => raise protocol_exn "Expected: JSON object"
in case Json.lookup obj arg of SOME (Json.String y) => SOME y
     | SOME _ => raise protocol_exn "Expected: JSON string"
     | NONE   => NONE
end

fun arg_str x arg = case optarg_str x arg of SOME y => y
                       | NONE => raise protocol_exn ("Could not find arg: " ^ arg)
(* }}} *)

end

