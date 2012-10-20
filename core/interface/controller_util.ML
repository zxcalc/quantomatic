structure ControllerUtil =
struct

exception protocol_exn of string

(* {{{ Input helpers *)

(* Returns the string as far as the next ESC and the terminator. *)
local
  fun readToEscape' (soFar: string) (terminator: char) : string =
    case input1 instream
      of SOME #"\027" =>
          (
            case input1 instream of
              NONE => raise protocol_exn "Unexpected end of file"
            | SOME #"\027" => (* Escaped ESC. *)
                                readToEscape' (soFar ^ str #"\027") terminator
            | SOME ch => if ch = terminator
                         then soFar
                         else raise protocol_exn ("Expected escape char: [" ^ str terminator ^ "], got: [" ^ str ch ^ "]")
          )
       | SOME ch => readToEscape' (soFar ^ str ch) terminator
       | NONE => raise protocol_exn "Unexpected end of file"
in
  val readToEscape = readToEscape' ""
end

fun readChar () =
  case input1 instream
    of NONE => raise protocol_exn "Unexpected end of file"
     | SOME ch => ch
(* }}} *)

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

