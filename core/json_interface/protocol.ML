signature JSON_CONTROLLER_PROTOCOL =
sig
  val run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val run_in_stdio : unit -> unit
  val parallel_run_in_textstreams : TextIO.instream * TextIO.outstream -> unit 
  val parallel_run_in_stdio : unit -> unit
end

structure JsonControllerProtocol =
struct

open JsonControllerUtil

(*
fun dispatch (("!!", function), json_input) = 
      (case function
         of "help"    => help json_input
          | "version" => Json.String "2.01"
          | _         => raise user_exn ("Function not found: !!." ^ function))
*)


fun system "help" x =
    let
      val controller = arg_str x "controller"
      val module = arg_str x "module"
      val opt_function = optarg_str x "function"
      val mtab = case Symtab.lookup JsonControllerRegistry.ctab controller of SOME x => x
                      | NONE => raise user_exn ("Controller not found: " ^ controller)
      val modl = case Symtab.lookup mtab module of SOME x => x
                      | NONE => raise user_exn ("Module not found: " ^ module)
    in
      case opt_function of NONE => Json.String (snd modl)
         | SOME function => (
             let
               val fd = snd (case Symtab.lookup (fst modl) function of SOME x => x
                             | NONE => raise user_exn ("Function not found: " ^ module ^ "." ^ function))
             in Json.String (
                  module ^ "." ^ #name fd ^ ": " ^
                  string_of_pargs (#input fd) ^ " -> " ^
                  string_of_pargs (#output fd) ^ "\n  " ^
                  #doc fd
                )
             end)
    end
  | system "kill" x =
      let
        val job = arg_int x "job"
      in Json.mk_object [
        ("message", Json.String ("Job " ^ Int.toString job ^ "killed.")),
        ("job", Json.Int job)
      ]
      end
  | system "version" _ = Json.String "2.1"
  | system _ _ = raise user_exn ("Unrecognized system function. (options: help, kill, version)")

fun dispatch json = let
  val rid = arg_int json "request_id"
  val controller = arg_str json "controller"
  val module = arg_str json "module"
  val function = arg_str json "function"
  val json_input = arg_json json "input"
in
  Json.mk_object [
    ("request_id", Json.Int rid),
    ("success",    Json.Bool true),
    ("output",     if controller = "!!" then
                     (case module of "system" => system function json_input
                                   | _ => raise user_exn "Module for !! controller must be 'system'.")
                   else JsonControllerRegistry.dispatch ((controller,module,function),json_input))
  ]
  handle user_exn msg =>
  Json.mk_object [
    ("request_id", Json.Int rid),
    ("success",    Json.Bool false),
    ("output",     Json.mk_object[("message", Json.String msg), ("code", Json.Int 0)])
  ]
end


fun run_in_textstreams (instream, outstream) =
let
  fun protocol_loop src =
  let
    val (json_list, src') = Source.get src
    val json = case json_list of [x] => x | _ =>
      raise protocol_exn "Expected: exactly one JSON object"
    val json_output = dispatch json
  in (TextIO.output (outstream, Json.encode json_output);
      TextIO.flushOut outstream;
      protocol_loop src')
  end
in protocol_loop (Json.json_source instream)
end
handle protocol_exn msg => 
  (TextIO.output (outstream,
    Json.encode (Json.mk_object [
      ("request_id", Json.Int ~1),
      ("success",    Json.Bool false),
      ("output",     Json.mk_object [("message", Json.String msg), ("code", Json.Int ~1)])
    ]));
   TextIO.flushOut outstream)


fun parallel_run_in_textstreams (instream, outstream) =
let
  val output_buffer: (Json.json list) Synchronized.var = Synchronized.var "output_buffer" []
  fun push_output json = Synchronized.change output_buffer (cons json)
  fun output_loop () =
  let
    fun flush_all buf =
      (fold_rev (fn json => fn () => TextIO.output (outstream, Json.encode json)) buf (); [])
  in (Synchronized.change output_buffer flush_all; TextIO.flushOut outstream; output_loop ())
  end

  fun input_loop src =
  let
    val (json_list, src') = Source.get src
    val json = case json_list of [x] => x | _ =>
      raise protocol_exn "Expected: exactly one JSON object"
  in (Future.fork (fn () => push_output (dispatch json)); input_loop src')
  end
in (Future.fork output_loop; input_loop (Json.json_source instream))
end
handle protocol_exn msg => 
  (TextIO.output (outstream,
    Json.encode (Json.mk_object [
      ("request_id", Json.Int ~1),
      ("success",    Json.Bool false),
      ("output",     Json.mk_object [("message", Json.String msg), ("code", Json.Int ~1)])
    ]));
   TextIO.flushOut outstream)


fun flushIn () = case TextIO.canInput (TextIO.stdIn, 1)
                   of SOME _ => (TextIO.input1 TextIO.stdIn;())
                    | NONE => ()

fun run_in_stdio () = (run_in_textstreams (TextIO.stdIn,TextIO.stdOut))
fun parallel_run_in_stdio () = (parallel_run_in_textstreams (TextIO.stdIn,TextIO.stdOut))


end