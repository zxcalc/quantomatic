(*  Title:      Pure/Tools/debugger.ML
    Author:     Makarius

Interactive debugger for Isabelle/ML.
*)

signature DEBUGGER =
sig
  val writeln_message: string -> unit
  val warning_message: string -> unit
  val error_message: string -> unit
end;

structure Debugger: DEBUGGER =
struct

(** global state **)

(* output messages *)

val _ = Session.protocol_handler "isabelle.Debugger$Handler";

fun output_message kind msg =
  if msg = "" then ()
  else
    Output.protocol_message
      (Markup.debugger_output (Standard_Thread.the_name ()))
      [Markup.markup (kind, Markup.serial_properties (serial ())) msg];

val writeln_message = output_message Markup.writelnN;
val warning_message = output_message Markup.warningN;
val error_message = output_message Markup.errorN;

fun error_wrapper e = e ()
  handle exn =>
    if Exn.is_interrupt exn then Exn.reraise exn
    else error_message (Runtime.exn_message exn);


(* thread input *)

val thread_input =
  Synchronized.var "Debugger.state" (NONE: string list Queue.T Symtab.table option);

fun init_input () = Synchronized.change thread_input (K (SOME Symtab.empty));
fun exit_input () = Synchronized.change thread_input (K NONE);

fun input thread_name msg =
  if null msg then error "Empty input"
  else
    Synchronized.change thread_input
      (Option.map (Symtab.map_default (thread_name, Queue.empty) (Queue.enqueue msg)));

fun get_input thread_name =
  Synchronized.guarded_access thread_input
    (fn NONE => SOME ([], NONE)
      | SOME input =>
          (case Symtab.lookup input thread_name of
            NONE => NONE
          | SOME queue =>
              let
                val (msg, queue') = Queue.dequeue queue;
                val input' =
                  if Queue.is_empty queue' then Symtab.delete_safe thread_name input
                  else Symtab.update (thread_name, queue') input;
              in SOME (msg, SOME input') end));


(* global break *)

local
  val break = Synchronized.var "Debugger.break" false;
in

fun is_break () = Synchronized.value break;
fun set_break b = Synchronized.change break (K b);

end;



(** thread state **)

(* stack frame during debugging *)

val debugging_var = Thread_Data.var () : PolyML.DebuggerInterface.debugState list Thread_Data.var;

fun get_debugging () = the_default [] (Thread_Data.get debugging_var);
val is_debugging = not o null o get_debugging;

fun with_debugging e =
  Thread_Data.setmp debugging_var (SOME (PolyML.DebuggerInterface.debugState (Thread.self ()))) e ();

fun the_debug_state thread_name index =
  (case get_debugging () of
    [] => error ("Missing debugger information for thread " ^ quote thread_name)
  | states =>
      if index < 0 orelse index >= length states then
        error ("Bad debugger stack index " ^ signed_string_of_int index ^ " for thread " ^
          quote thread_name)
      else nth states index);


(* flags for single-stepping *)

datatype stepping = Stepping of bool * int;  (*stepping enabled, stack depth limit*)

val stepping_var = Thread_Data.var () : stepping Thread_Data.var;

fun get_stepping () = the_default (Stepping (false, ~1)) (Thread_Data.get stepping_var);
fun put_stepping stepping = Thread_Data.put stepping_var (SOME (Stepping stepping));

fun is_stepping () =
  let
    val stack = PolyML.DebuggerInterface.debugState (Thread.self ());
    val Stepping (stepping, depth) = get_stepping ();
  in stepping andalso (depth < 0 orelse length stack <= depth) end;

fun continue () = put_stepping (false, ~1);
fun step () = put_stepping (true, ~1);
fun step_over () = put_stepping (true, length (get_debugging ()));
fun step_out () = put_stepping (true, length (get_debugging ()) - 1);



(** eval ML **)

local

val context_attempts =
  map ML_Lex.read
   ["Context.put_generic_context (SOME (Context.Theory ML_context))",
    "Context.put_generic_context (SOME (Context.Proof ML_context))",
    "Context.put_generic_context (SOME ML_context)"];

fun evaluate {SML, verbose} =
  ML_Context.eval
    {SML = SML, exchange = false, redirect = false, verbose = verbose,
      debug = SOME false, writeln = writeln_message, warning = warning_message}
    Position.none;

fun eval_setup thread_name index SML context =
  context
  |> Context_Position.set_visible_generic false
  |> ML_Env.add_name_space {SML = SML}
      (PolyML.DebuggerInterface.debugNameSpace (the_debug_state thread_name index));

fun eval_context thread_name index SML toks =
  let
    val context = Context.the_generic_context ();
    val context1 =
      if SML orelse forall (fn Antiquote.Text tok => ML_Lex.is_improper tok | _ => false) toks
      then context
      else
        let
          val context' = context
            |> eval_setup thread_name index SML
            |> ML_Context.exec (fn () =>
                evaluate {SML = SML, verbose = true} (ML_Lex.read "val ML_context = " @ toks));
          fun try_exec toks =
            try (ML_Context.exec (fn () => evaluate {SML = false, verbose = false} toks)) context';
        in
          (case get_first try_exec context_attempts of
            SOME context2 => context2
          | NONE => error "Malformed context: expected type theory, Proof.context, Context.generic")
        end;
  in context1 |> eval_setup thread_name index SML end;

in

fun eval thread_name index SML txt1 txt2 =
  let
    val (toks1, toks2) = apply2 (ML_Lex.read_source SML o Input.string) (txt1, txt2);
    val context = eval_context thread_name index SML toks1;
  in Context.setmp_generic_context (SOME context) (evaluate {SML = SML, verbose = true}) toks2 end;

fun print_vals thread_name index SML txt =
  let
    val toks = ML_Lex.read_source SML (Input.string txt)
    val context = eval_context thread_name index SML toks;
    val space = PolyML.DebuggerInterface.debugNameSpace (the_debug_state thread_name index);

    fun print x =
      Pretty.from_polyml
        (PolyML.NameSpace.Values.printWithType
          (x, FixedInt.fromInt (ML_Print_Depth.get_print_depth ()), SOME space));
    fun print_all () =
      #allVal (PolyML.DebuggerInterface.debugLocalNameSpace (the_debug_state thread_name index)) ()
      |> sort_by #1 |> map (Pretty.item o single o print o #2)
      |> Pretty.chunks |> Pretty.string_of |> writeln_message;
  in Context.setmp_generic_context (SOME context) print_all () end;

end;



(** debugger loop **)

local

fun debugger_state thread_name =
  Output.protocol_message (Markup.debugger_state thread_name)
   [get_debugging ()
    |> map (fn st =>
      (Position.properties_of
        (Exn_Properties.position_of_polyml_location (PolyML.DebuggerInterface.debugLocation st)),
       PolyML.DebuggerInterface.debugFunction st))
    |> let open XML.Encode in list (pair properties string) end
    |> YXML.string_of_body];

fun debugger_command thread_name =
  (case get_input thread_name of
    [] => (continue (); false)
  | ["continue"] => (continue (); false)
  | ["step"] => (step (); false)
  | ["step_over"] => (step_over (); false)
  | ["step_out"] => (step_out (); false)
  | ["eval", index, SML, txt1, txt2] =>
     (error_wrapper (fn () =>
        eval thread_name (Value.parse_int index) (Value.parse_bool SML) txt1 txt2); true)
  | ["print_vals", index, SML, txt] =>
     (error_wrapper (fn () =>
        print_vals thread_name (Value.parse_int index) (Value.parse_bool SML) txt); true)
  | bad =>
     (Output.system_message
        ("Debugger: bad input " ^ ML_Syntax.print_list ML_Syntax.print_string bad); true));

in

fun debugger_loop thread_name =
  Thread_Attributes.uninterruptible (fn restore_attributes => fn () =>
    let
      fun loop () =
        (debugger_state thread_name; if debugger_command thread_name then loop () else ());
      val result = Exn.capture (restore_attributes with_debugging) loop;
      val _ = debugger_state thread_name;
    in Exn.release result end) ();

end;



(** protocol commands **)

val _ =
  Isabelle_Process.protocol_command "Debugger.init"
    (fn [] =>
     (init_input ();
      PolyML.DebuggerInterface.setOnBreakPoint
        (SOME (fn (_, break) =>
          if not (is_debugging ()) andalso (! break orelse is_break () orelse is_stepping ())
          then
            (case Standard_Thread.get_name () of
              SOME thread_name => debugger_loop thread_name
            | NONE => ())
          else ()))));

val _ =
  Isabelle_Process.protocol_command "Debugger.exit"
    (fn [] => (PolyML.DebuggerInterface.setOnBreakPoint NONE; exit_input ()));

val _ =
  Isabelle_Process.protocol_command "Debugger.break"
    (fn [b] => set_break (Value.parse_bool b));

val _ =
  Isabelle_Process.protocol_command "Debugger.breakpoint"
    (fn [node_name, id0, breakpoint0, breakpoint_state0] =>
      let
        val id = Value.parse_int id0;
        val breakpoint = Value.parse_int breakpoint0;
        val breakpoint_state = Value.parse_bool breakpoint_state0;

        fun err () = error ("Bad exec for command " ^ Value.print_int id);
      in
        (case Document.command_exec (Document.state ()) node_name id of
          SOME (eval, _) =>
            if Command.eval_finished eval then
              let
                val st = Command.eval_result_state eval;
                val ctxt = Toplevel.presentation_context_of st
                  handle Toplevel.UNDEF => err ();
              in
                (case ML_Env.get_breakpoint (Context.Proof ctxt) breakpoint of
                  SOME (b, _) => b := breakpoint_state
                | NONE => err ())
              end
            else err ()
        | NONE => err ())
      end);

val _ =
  Isabelle_Process.protocol_command "Debugger.input"
    (fn thread_name :: msg => input thread_name msg);

end;
