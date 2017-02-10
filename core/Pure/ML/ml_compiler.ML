(*  Title:      Pure/ML/ml_compiler.ML
    Author:     Makarius

Runtime compilation and evaluation.
*)

signature ML_COMPILER =
sig
  type flags =
    {SML: bool, exchange: bool, redirect: bool, verbose: bool,
      debug: bool option, writeln: string -> unit, warning: string -> unit}
  val debug_flags: bool option -> flags
  val flags: flags
  val verbose: bool -> flags -> flags
  val eval: flags -> Position.T -> ML_Lex.token list -> unit
end;

structure ML_Compiler: ML_COMPILER =
struct

(* flags *)

type flags =
  {SML: bool, exchange: bool, redirect: bool, verbose: bool,
    debug: bool option, writeln: string -> unit, warning: string -> unit};

fun debug_flags opt_debug : flags =
  {SML = false, exchange = false, redirect = false, verbose = false,
    debug = opt_debug, writeln = writeln, warning = warning};

val flags = debug_flags NONE;

fun verbose b (flags: flags) =
  {SML = #SML flags, exchange = #exchange flags, redirect = #redirect flags, verbose = b,
    debug = #debug flags, writeln = #writeln flags, warning = #warning flags};


(* parse trees *)

fun breakpoint_position loc =
  let val pos = Position.no_range_position (Exn_Properties.position_of_polyml_location loc) in
    (case Position.offset_of pos of
      NONE => pos
    | SOME 1 => pos
    | SOME j =>
        Position.properties_of pos
        |> Properties.put (Markup.offsetN, Value.print_int (j - 1))
        |> Position.of_properties)
  end;

fun report_parse_tree redirect depth name_space parse_tree =
  let
    val is_visible =
      (case Context.get_generic_context () of
        SOME context => Context_Position.is_visible_generic context
      | NONE => true);
    fun is_reported pos = is_visible andalso Position.is_reported pos;


    (* syntax reports *)

    fun reported_types loc types =
      let val pos = Exn_Properties.position_of_polyml_location loc in
        is_reported pos ?
          let
            val xml =
              PolyML.NameSpace.Values.printType (types, depth, SOME name_space)
              |> Pretty.from_polyml |> Pretty.string_of
              |> Output.output |> YXML.parse_body;
          in cons (pos, fn () => Markup.ML_typing, fn () => YXML.string_of_body xml) end
      end;

    fun reported_entity kind loc decl =
      let
        val pos = Exn_Properties.position_of_polyml_location loc;
        val def_pos = Exn_Properties.position_of_polyml_location decl;
      in
        (is_reported pos andalso pos <> def_pos) ?
          let
            fun markup () =
              (Markup.entityN, (Markup.kindN, kind) :: Position.def_properties_of def_pos);
          in cons (pos, markup, fn () => "") end
      end;

    fun reported_entity_id def id loc =
      let
        val pos = Exn_Properties.position_of_polyml_location loc;
      in
        is_reported pos ?
          let
            fun markup () =
              (Markup.entityN, [(if def then Markup.defN else Markup.refN, Value.print_int id)]);
          in cons (pos, markup, fn () => "") end
      end;

    fun reported_completions loc names =
      let val pos = Exn_Properties.position_of_polyml_location loc in
        if is_reported pos andalso not (null names) then
          let
            val completion = Completion.names pos (map (fn a => (a, ("ML", a))) names);
            val xml = Completion.encode completion;
          in cons (pos, fn () => Markup.completion, fn () => YXML.string_of_body xml) end
        else I
      end;

    fun reported _ (PolyML.PTnextSibling tree) = reported_tree (tree ())
      | reported _ (PolyML.PTfirstChild tree) = reported_tree (tree ())
      | reported loc (PolyML.PTdefId id) = reported_entity_id true (FixedInt.toLarge id) loc
      | reported loc (PolyML.PTrefId id) = reported_entity_id false (FixedInt.toLarge id) loc
      | reported loc (PolyML.PTtype types) = reported_types loc types
      | reported loc (PolyML.PTdeclaredAt decl) = reported_entity Markup.ML_defN loc decl
      | reported loc (PolyML.PTopenedAt decl) = reported_entity Markup.ML_openN loc decl
      | reported loc (PolyML.PTstructureAt decl) = reported_entity Markup.ML_structureN loc decl
      | reported loc (PolyML.PTcompletions names) = reported_completions loc names
      | reported _ _ = I
    and reported_tree (loc, props) = fold (reported loc) props;

    val persistent_reports = reported_tree parse_tree [];

    fun output () =
      persistent_reports
      |> map (fn (pos, markup, text) => Position.reported_text pos (markup ()) (text ()))
      |> Output.report;
    val _ =
      if not (null persistent_reports) andalso redirect andalso Multithreading.enabled ()
      then
        Execution.print
          {name = "ML_Compiler.report", pos = Position.thread_data (), pri = Task_Queue.urgent_pri}
          output
      else output ();


    (* breakpoints *)

    fun breakpoints _ (PolyML.PTnextSibling tree) = breakpoints_tree (tree ())
      | breakpoints _ (PolyML.PTfirstChild tree) = breakpoints_tree (tree ())
      | breakpoints loc (PolyML.PTbreakPoint b) =
          let val pos = breakpoint_position loc in
            if is_reported pos then
              let val id = serial ();
              in cons ((pos, Markup.ML_breakpoint id), (id, (b, pos))) end
            else I
          end
      | breakpoints _ _ = I
    and breakpoints_tree (loc, props) = fold (breakpoints loc) props;

    val all_breakpoints = rev (breakpoints_tree parse_tree []);
    val _ = Position.reports (map #1 all_breakpoints);
 in map (fn (_, (id, (b, pos))) => (id, (b, Position.dest pos))) all_breakpoints end;


(* eval ML source tokens *)

fun eval (flags: flags) pos toks =
  let
    val opt_context = Context.get_generic_context ();

    val env as {debug, name_space, add_breakpoints} =
      (case (ML_Recursive.get (), #SML flags orelse #exchange flags) of
        (SOME env, false) => env
      | _ =>
       {debug =
          (case #debug flags of
            SOME debug => debug
          | NONE => ML_Options.debugger_enabled opt_context),
        name_space = ML_Env.make_name_space {SML = #SML flags, exchange = #exchange flags},
        add_breakpoints = ML_Env.add_breakpoints});


    (* input *)

    val location_props = op ^ (YXML.output_markup (":", #props (Position.dest pos)));

    val input_explode =
      if #SML flags then String.explode
      else maps (String.explode o Symbol.esc) o Symbol.explode;

    val input_buffer =
      Unsynchronized.ref (toks |> map (`(input_explode o ML_Lex.check_content_of)));

    fun get () =
      (case ! input_buffer of
        (c :: cs, tok) :: rest => (input_buffer := (cs, tok) :: rest; SOME c)
      | ([], _) :: rest => (input_buffer := rest; SOME #" ")
      | [] => NONE);

    fun get_pos () =
      (case ! input_buffer of
        (_ :: _, tok) :: _ => ML_Lex.pos_of tok
      | ([], tok) :: _ => ML_Lex.end_pos_of tok
      | [] => Position.none);


    (* output *)

    val writeln_buffer = Unsynchronized.ref Buffer.empty;
    fun write s = Unsynchronized.change writeln_buffer (Buffer.add s);
    fun output_writeln () = #writeln flags (trim_line (Buffer.content (! writeln_buffer)));

    val warnings = Unsynchronized.ref ([]: string list);
    fun warn msg = Unsynchronized.change warnings (cons msg);
    fun output_warnings () = List.app (#warning flags) (rev (! warnings));

    val error_buffer = Unsynchronized.ref Buffer.empty;
    fun err msg = Unsynchronized.change error_buffer (Buffer.add msg #> Buffer.add "\n");
    fun flush_error () = #writeln flags (trim_line (Buffer.content (! error_buffer)));
    fun raise_error msg = error (trim_line (Buffer.content (Buffer.add msg (! error_buffer))));

    fun message {message = msg, hard, location = loc, context = _} =
      let
        val pos = Exn_Properties.position_of_polyml_location loc;
        val txt =
          (if hard then "ML error" else "ML warning") ^ Position.here pos ^ ":\n" ^
          Pretty.string_of (Pretty.from_polyml msg);
      in if hard then err txt else warn txt end;


    (* results *)

    val depth = FixedInt.fromInt (ML_Print_Depth.get_print_depth ());

    fun apply_result {fixes, types, signatures, structures, functors, values} =
      let
        fun display disp x =
          if depth > 0 then
            (write (disp x |> Pretty.from_polyml |> Pretty.string_of); write "\n")
          else ();

        fun apply_fix (a, b) =
          (#enterFix name_space (a, b);
            display PolyML.NameSpace.Infixes.print b);
        fun apply_type (a, b) =
          (#enterType name_space (a, b);
            display PolyML.NameSpace.TypeConstrs.print (b, depth, SOME name_space));
        fun apply_sig (a, b) =
          (#enterSig name_space (a, b);
            display PolyML.NameSpace.Signatures.print (b, depth, SOME name_space));
        fun apply_struct (a, b) =
          (#enterStruct name_space (a, b);
            display PolyML.NameSpace.Structures.print (b, depth, SOME name_space));
        fun apply_funct (a, b) =
          (#enterFunct name_space (a, b);
            display PolyML.NameSpace.Functors.print (b, depth, SOME name_space));
        fun apply_val (a, b) =
          (#enterVal name_space (a, b);
            display PolyML.NameSpace.Values.printWithType (b, depth, SOME name_space));
      in
        List.app apply_fix fixes;
        List.app apply_type types;
        List.app apply_sig signatures;
        List.app apply_struct structures;
        List.app apply_funct functors;
        List.app apply_val values
      end;

    exception STATIC_ERRORS of unit;

    fun result_fun (phase1, phase2) () =
     ((case phase1 of
        NONE => ()
      | SOME parse_tree =>
          add_breakpoints (report_parse_tree (#redirect flags) depth name_space parse_tree));
      (case phase2 of
        NONE => raise STATIC_ERRORS ()
      | SOME code =>
          apply_result
            ((code
              |> Runtime.debugging opt_context
              |> Runtime.toplevel_error (err o Runtime.exn_message)) ())));


    (* compiler invocation *)

    val parameters =
     [PolyML.Compiler.CPOutStream write,
      PolyML.Compiler.CPNameSpace name_space,
      PolyML.Compiler.CPErrorMessageProc message,
      PolyML.Compiler.CPLineNo (the_default 0 o Position.line_of o get_pos),
      PolyML.Compiler.CPLineOffset (the_default 0 o Position.offset_of o get_pos),
      PolyML.Compiler.CPFileName location_props,
      PolyML.Compiler.CPPrintDepth ML_Print_Depth.get_print_depth,
      PolyML.Compiler.CPCompilerResultFun result_fun,
      PolyML.Compiler.CPPrintInAlphabeticalOrder false,
      PolyML.Compiler.CPDebug debug,
      PolyML.Compiler.CPBindingSeq serial];

    val _ =
      (while not (List.null (! input_buffer)) do
        ML_Recursive.recursive env (fn () => PolyML.compiler (get, parameters) ()))
      handle exn =>
        if Exn.is_interrupt exn then Exn.reraise exn
        else
          let
            val exn_msg =
              (case exn of
                STATIC_ERRORS () => ""
              | Runtime.TOPLEVEL_ERROR => ""
              | _ => "Exception- " ^ Pretty.string_of (Runtime.pretty_exn exn) ^ " raised");
            val _ = output_warnings ();
            val _ = output_writeln ();
          in raise_error exn_msg end;
  in
    if #verbose flags then (output_warnings (); flush_error (); output_writeln ())
    else ()
  end;

end;
