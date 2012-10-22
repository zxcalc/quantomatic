signature CONTROLLER_PROTOCOL =
sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
end

structure ControllerProtocol =
struct

open ControllerUtil

(* {{{ Input helpers *)

(* Returns the string as far as the next ESC and the terminator. *)
fun read_to_esc instream terminator =
let
  fun rd accum =
    case TextIO.input1 instream
      of SOME #"\027" =>
          (
            case TextIO.input1 instream of
              NONE => raise protocol_exn "Unexpected end of file"
            | SOME #"\027" => (* Escaped ESC. *)
                                rd (accum ^ str #"\027")
            | SOME ch => if ch = terminator
                         then accum
                         else raise protocol_exn ("Expected escape char: [" ^ str terminator ^ "], got: [" ^ str ch ^ "]")
          )
       | SOME ch => rd (accum ^ str ch)
       | NONE => raise protocol_exn "Unexpected end of file"
in rd ""
end

fun dispatch rid cmd = 
  Json.mk_object [
    ("request_id", Json.Int rid),
    ("success",    Json.Bool true),
    ("output",     ControllerRegistry.dispatch cmd)
  ]
handle user_exn msg =>
  Json.mk_object [
    ("request_id", Json.Int rid),
    ("success",    Json.Bool false),
    ("message",    Json.String msg)
  ]


fun run_in_textstreams (instream, outstream) =
let
  fun protocol_loop src =
  let
    val (json_list, src') = Source.get src
    val json = case json_list of [x] => x | _ =>
      raise protocol_exn "Expected: exactly one JSON object"
    val rid = arg_int json "request_id"
    val controller = arg_str json "controller"
    val module = arg_str json "module"
    val function = arg_str json "function"
    val json_input = arg_json json "input"
    val json_output = dispatch rid ((controller, module, function), json_input)
  in (TextIO.output (outstream, Json.encode json_output);
      TextIO.flushOut outstream;
      protocol_loop src')
  end
in protocol_loop (Json.json_source instream)
end
handle protocol_exn msg => 
  (TextIO.output (outstream,
    Json.encode (Json.mk_object [("message", Json.String msg)]));
   TextIO.flushOut outstream)

(*fun run_in_textstreams (instream, outstream) = let
  val _   = read_to_esc instream #"<"
  val req = read_to_esc instream #","
  val controller = read_to_esc instream #","
  val module     = read_to_esc instream #","
  val function   = read_to_esc instream #","
  val json_in    = Json.of_string (read_to_esc instream #">")
  val (status, json_out) = ControllerRegistry.dispatch ((controller, module, function), json_in)
in (
	    TextIO.output (outstream, "\027<" ^ req ^ "\027," ^ status ^ "\027," ^
	    	Json.encode json_out ^ "\027>");
      TextIO.flushOut outstream;
	    run_in_textstreams (instream, outstream)
	 )
end
handle protocol_exn msg => 
  TextIO.output (outstream, "\027<-1\027,ERROR\027," ^
  	Json.encode (Json.mk_record [("message", msg)]) ^ "\027>")
*)

fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                   of SOME _ => (TextIO.input1 TextIO.stdIn;())
                    | NONE => ()

fun run_in_stdio () = (run_in_textstreams (TextIO.stdIn,TextIO.stdOut))


end