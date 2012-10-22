structure ControllerUtil =
struct

(* got bad input from the protocol, should crash *)
exception protocol_exn of string

(* got bad input from the user, should report error and carry on *)
exception user_exn of string

(* {{{ JSON argument helpers *)
fun optarg_json x arg =
let
  val obj = case x of Json.Object y => y | _ => raise protocol_exn "Expected: JSON object"
in Json.lookup obj arg
end

fun optarg_str x arg =
  case optarg_json x arg
    of SOME (Json.String y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE
     
fun optarg_str x arg =
  case optarg_json x arg
    of SOME (Json.String y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE

fun optarg_int x arg =
  case optarg_json x arg
    of SOME (Json.Int y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE


fun arg_json x arg = case optarg_json x arg of SOME x => x
                     | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

fun arg_str x arg = case optarg_str x arg of SOME y => y
                    | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

fun arg_int x arg = case optarg_int x arg of SOME y => y
                    | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

(* }}} *)

end

