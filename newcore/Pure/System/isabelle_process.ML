(*  Title:      Pure/System/isabelle_process.ML
    Author:     Makarius

Isabelle process wrapper, based on private fifos for maximum
robustness and performance, or local socket for maximum portability.
*)

signature ISABELLE_PROCESS =
sig
  val is_active: unit -> bool
  val protocol_command: string -> (string list -> unit) -> unit
  val reset_tracing: Document_ID.exec -> unit
  val crashes: exn list Synchronized.var
  val init_fifos: string -> string -> unit
  val init_socket: string -> unit
end;

structure Isabelle_Process: ISABELLE_PROCESS =
struct

(* print mode *)

val isabelle_processN = "isabelle_process";

fun is_active () = Print_Mode.print_mode_active isabelle_processN;

val _ = Output.add_mode isabelle_processN Output.default_output Output.default_escape;
val _ = Markup.add_mode isabelle_processN YXML.output_markup;


(* protocol commands *)

local

val commands =
  Synchronized.var "Isabelle_Process.commands"
    (Symtab.empty: (string list -> unit) Symtab.table);

in

fun protocol_command name cmd =
  Synchronized.change commands (fn cmds =>
   (if not (Symtab.defined cmds name) then ()
    else warning ("Redefining Isabelle protocol command " ^ quote name);
    Symtab.update (name, cmd) cmds));

fun run_command name args =
  (case Symtab.lookup (Synchronized.value commands) name of
    NONE => error ("Undefined Isabelle protocol command " ^ quote name)
  | SOME cmd =>
      (Runtime.debugging NONE cmd args handle exn =>
        error ("Isabelle protocol command failure: " ^ quote name ^ "\n" ^
          Runtime.exn_message exn)));

end;


(* restricted tracing messages *)

val tracing_messages =
  Synchronized.var "tracing_messages" (Inttab.empty: int Inttab.table);

fun reset_tracing exec_id =
  Synchronized.change tracing_messages (Inttab.delete_safe exec_id);

fun update_tracing () =
  (case Position.parse_id (Position.thread_data ()) of
    NONE => ()
  | SOME exec_id =>
      let
        val ok =
          Synchronized.change_result tracing_messages (fn tab =>
            let
              val n = the_default 0 (Inttab.lookup tab exec_id) + 1;
              val ok = n <= Options.default_int "editor_tracing_messages";
            in (ok, Inttab.update (exec_id, n) tab) end);
      in
        if ok then ()
        else
          let
            val (text, promise) = Active.dialog_text ();
            val _ =
              writeln ("Tracing paused.  " ^ text "Stop" ^ ", or continue with next " ^
                text "100" ^ ", " ^ text "1000" ^ ", " ^ text "10000" ^ " messages?")
            val m = Markup.parse_int (Future.join promise)
              handle Fail _ => error "Stopped";
          in
            Synchronized.change tracing_messages
              (Inttab.map_default (exec_id, 0) (fn k => k - m))
          end
      end);


(* output channels *)

val serial_props = Markup.serial_properties o serial;

fun init_channels channel =
  let
    val _ = TextIO.StreamIO.setBufferMode (TextIO.getOutstream TextIO.stdOut, IO.LINE_BUF);
    val _ = TextIO.StreamIO.setBufferMode (TextIO.getOutstream TextIO.stdErr, IO.LINE_BUF);

    val msg_channel = Message_Channel.make channel;

    fun message name props body =
      Message_Channel.send msg_channel (Message_Channel.message name props body);

    fun standard_message props name body =
      if forall (fn s => s = "") body then ()
      else
        message name
          (fold_rev Properties.put props (Position.properties_of (Position.thread_data ()))) body;
  in
    Output.status_fn := standard_message [] Markup.statusN;
    Output.report_fn := standard_message [] Markup.reportN;
    Output.result_fn :=
      (fn props => fn s => standard_message (props @ serial_props ()) Markup.resultN s);
    Output.writeln_fn := (fn s => standard_message (serial_props ()) Markup.writelnN s);
    Output.tracing_fn :=
      (fn s => (update_tracing (); standard_message (serial_props ()) Markup.tracingN s));
    Output.warning_fn := (fn s => standard_message (serial_props ()) Markup.warningN s);
    Output.error_message_fn :=
      (fn (i, s) => standard_message (Markup.serial_properties i) Markup.errorN s);
    Output.system_message_fn := message Markup.systemN [];
    Output.protocol_message_fn := message Markup.protocolN;
    Output.urgent_message_fn := ! Output.writeln_fn;
    Output.prompt_fn := ignore;
    message Markup.initN [] [Session.welcome ()];
    msg_channel
  end;


(* protocol loop -- uninterruptible *)

val crashes = Synchronized.var "Isabelle_Process.crashes" ([]: exn list);

local

fun recover crash =
  (Synchronized.change crashes (cons crash);
    Output.physical_stderr
      "Recovered from Isabelle process crash -- see also Isabelle_Process.crashes\n");

fun read_chunk channel len =
  let
    val n =
      (case Int.fromString len of
        SOME n => n
      | NONE => error ("Isabelle process: malformed header " ^ quote len));
    val chunk = System_Channel.inputN channel n;
    val i = size chunk;
  in
    if i <> n then
      error ("Isabelle process: bad chunk (unexpected EOF after " ^
        string_of_int i ^ " of " ^ string_of_int n ^ " bytes)")
    else chunk
  end;

fun read_command channel =
  (case System_Channel.input_line channel of
    NONE => raise Runtime.TERMINATE
  | SOME line => map (read_chunk channel) (space_explode "," line));

fun task_context e =
  Future.task_context "Isabelle_Process.loop" (Future.new_group NONE) e ();

in

fun loop channel =
  let val continue =
    (case read_command channel of
      [] => (Output.system_message "Isabelle process: no input"; true)
    | name :: args => (task_context (fn () => run_command name args); true))
    handle Runtime.TERMINATE => false
      | exn => (Runtime.exn_system_message exn handle crash => recover crash; true);
  in
    if continue then loop channel
    else (Future.shutdown (); Execution.reset (); ())
  end;

end;


(* init *)

val default_modes1 =
  [Syntax_Trans.no_bracketsN, Syntax_Trans.no_type_bracketsN, Graph_Display.active_graphN];
val default_modes2 = [Symbol.xsymbolsN, isabelle_processN, Pretty.symbolicN];

val init = uninterruptible (fn _ => fn rendezvous =>
  let
    val _ = SHA1_Samples.test ()
      handle exn as Fail msg => (Output.physical_stderr (msg ^ "\n"); reraise exn);
    val _ = Output.physical_stderr Symbol.STX;

    val _ = Printer.show_markup_default := true;
    val _ = Context.set_thread_data NONE;
    val _ =
      Unsynchronized.change print_mode
        (fn mode => (mode @ default_modes1) |> fold (update op =) default_modes2);

    val channel = rendezvous ();
    val msg_channel = init_channels channel;
    val _ = Session.init_protocol_handlers ();
    val _ = (loop |> Unsynchronized.setmp Toplevel.quiet true) channel;
  in Message_Channel.shutdown msg_channel end);

fun init_fifos fifo1 fifo2 = init (fn () => System_Channel.fifo_rendezvous fifo1 fifo2);
fun init_socket name = init (fn () => System_Channel.socket_rendezvous name);

end;

