(*  Title:      Pure/ML/ml_compiler_polyml.ML
    Author:     Makarius

Runtime compilation and evaluation -- Poly/ML version.
*)

structure ML_Compiler: ML_COMPILER =
struct

open ML_Compiler;


(* parse trees *)

fun report_parse_tree redirect depth space parse_tree =
  let
    val is_visible =
      (case Context.thread_data () of
        SOME context => Context_Position.is_visible_generic context
      | NONE => true);
    fun is_reported pos = is_visible andalso Position.is_reported pos;

    fun reported_types loc types =
      let val pos = Exn_Properties.position_of loc in
        is_reported pos ?
          let
            val xml =
              PolyML.NameSpace.displayTypeExpression (types, depth, space)
              |> pretty_ml |> Pretty.from_ML |> Pretty.string_of
              |> Output.output |> YXML.parse_body;
          in cons (pos, fn () => Markup.ML_typing, fn () => YXML.string_of_body xml) end
      end;

    fun reported_entity kind loc decl =
      let val pos = Exn_Properties.position_of loc in
        is_reported pos ?
          let
            val def_pos = Exn_Properties.position_of decl;
            fun markup () =
              (Markup.entityN, (Markup.kindN, kind) :: Position.def_properties_of def_pos);
          in cons (pos, markup, fn () => "") end
      end;

    fun reported loc (PolyML.PTtype types) = reported_types loc types
      | reported loc (PolyML.PTdeclaredAt decl) = reported_entity Markup.ML_defN loc decl
      | reported loc (PolyML.PTopenedAt decl) = reported_entity Markup.ML_openN loc decl
      | reported loc (PolyML.PTstructureAt decl) = reported_entity Markup.ML_structureN loc decl
      | reported _ (PolyML.PTnextSibling tree) = reported_tree (tree ())
      | reported _ (PolyML.PTfirstChild tree) = reported_tree (tree ())
      | reported _ _ = I
    and reported_tree (loc, props) = fold (reported loc) props;

    val persistent_reports = reported_tree parse_tree [];

    fun output () =
      persistent_reports
      |> map (fn (pos, markup, text) => Position.reported_text pos (markup ()) (text ()))
      |> Output.report;
  in
    if not (null persistent_reports) andalso redirect andalso Multithreading.enabled ()
    then
      Execution.print
        {name = "ML_Compiler.report", pos = Position.thread_data (), pri = 1} output
    else output ()
  end;


(* eval ML source tokens *)

fun eval (flags: flags) pos toks =
  let
    val _ = Secure.secure_mltext ();
    val space = ML_Env.name_space {SML = #SML flags, exchange = #exchange flags}
    val opt_context = Context.thread_data ();


    (* input *)

    val location_props = op ^ (YXML.output_markup (":", #props (Position.dest pos)));

    val input_buffer =
      Unsynchronized.ref (toks |> map
        (`(maps (String.explode o Symbol.esc) o Symbol.explode o ML_Lex.check_content_of)));

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


    (* output channels *)

    val writeln_buffer = Unsynchronized.ref Buffer.empty;
    fun write s = Unsynchronized.change writeln_buffer (Buffer.add s);
    fun output_writeln () = writeln (trim_line (Buffer.content (! writeln_buffer)));

    val warnings = Unsynchronized.ref ([]: string list);
    fun warn msg = Unsynchronized.change warnings (cons msg);
    fun output_warnings () = List.app warning (rev (! warnings));

    val error_buffer = Unsynchronized.ref Buffer.empty;
    fun err msg = Unsynchronized.change error_buffer (Buffer.add msg #> Buffer.add "\n");
    fun flush_error () = writeln (Buffer.content (! error_buffer));
    fun raise_error msg = error (Buffer.content (Buffer.add msg (! error_buffer)));

    fun message {message = msg, hard, location = loc, context = _} =
      let
        val pos = Exn_Properties.position_of loc;
        val txt =
          (if hard then "ML error" else "ML warning") ^ Position.here pos ^ ":\n" ^
          Pretty.string_of (Pretty.from_ML (pretty_ml msg));
      in if hard then err txt else warn txt end;


    (* results *)

    val depth = ML_Options.get_print_depth ();

    fun apply_result {fixes, types, signatures, structures, functors, values} =
      let
        fun display disp x =
          if depth > 0 then
            (disp x |> pretty_ml |> Pretty.from_ML |> Pretty.string_of |> write; write "\n")
          else ();

        fun apply_fix (a, b) =
          (#enterFix space (a, b); display PolyML.NameSpace.displayFix (a, b));
        fun apply_type (a, b) =
          (#enterType space (a, b); display PolyML.NameSpace.displayType (b, depth, space));
        fun apply_sig (a, b) =
          (#enterSig space (a, b); display PolyML.NameSpace.displaySig (b, depth, space));
        fun apply_struct (a, b) =
          (#enterStruct space (a, b); display PolyML.NameSpace.displayStruct (b, depth, space));
        fun apply_funct (a, b) =
          (#enterFunct space (a, b); display PolyML.NameSpace.displayFunct (b, depth, space));
        fun apply_val (a, b) =
          (#enterVal space (a, b); display PolyML.NameSpace.displayVal (b, depth, space));
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
      | SOME parse_tree => report_parse_tree (#redirect flags) depth space parse_tree);
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
      PolyML.Compiler.CPNameSpace space,
      PolyML.Compiler.CPErrorMessageProc message,
      PolyML.Compiler.CPLineNo (the_default 0 o Position.line_of o get_pos),
      PolyML.Compiler.CPLineOffset (the_default 0 o Position.offset_of o get_pos),
      PolyML.Compiler.CPFileName location_props,
      PolyML.Compiler.CPPrintDepth ML_Options.get_print_depth,
      PolyML.Compiler.CPCompilerResultFun result_fun,
      PolyML.Compiler.CPPrintInAlphabeticalOrder false];
    val _ =
      (while not (List.null (! input_buffer)) do
        PolyML.compiler (get, parameters) ())
      handle exn =>
        if Exn.is_interrupt exn then reraise exn
        else
          let
            val exn_msg =
              (case exn of
                STATIC_ERRORS () => ""
              | Runtime.TOPLEVEL_ERROR => ""
              | _ => "Exception- " ^ Pretty.string_of (Exn_Output.pretty exn) ^ " raised");
            val _ = output_warnings ();
            val _ = output_writeln ();
          in raise_error exn_msg end;
  in
    if #verbose flags then (output_warnings (); flush_error (); output_writeln ())
    else ()
  end;

end;

