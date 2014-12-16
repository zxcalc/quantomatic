(*  Title:      Pure/ML/ml_compiler_polyml.ML
    Author:     Makarius

Advanced runtime compilation for Poly/ML.
*)

structure ML_Compiler: ML_COMPILER =
struct

(* source locations *)

fun position_of (loc: PolyML.location) =
  let
    val {startLine = line, startPosition = offset, endPosition = end_offset, ...} = loc;
    val props = Exn_Properties.of_location loc;
  in
    Position.make {line = line, offset = offset, end_offset = end_offset, props = props}
  end;

fun exn_position exn =
  (case PolyML.exceptionLocation exn of
    NONE => Position.none
  | SOME loc => position_of loc);

val exn_messages_ids = Runtime.exn_messages_ids exn_position;
val exn_messages = Runtime.exn_messages exn_position;
val exn_message = Runtime.exn_message exn_position;


(* parse trees *)

fun report_parse_tree depth space =
  let
    val reported_text =
      (case Context.thread_data () of
        SOME (Context.Proof ctxt) => Context_Position.reported_text ctxt
      | _ => Position.reported_text);

    fun reported_entity kind loc decl =
      reported_text (position_of loc)
        (Markup.entityN, (Markup.kindN, kind) :: Position.def_properties_of (position_of decl)) "";

    fun reported loc (PolyML.PTtype types) =
          cons
            (PolyML.NameSpace.displayTypeExpression (types, depth, space)
              |> pretty_ml |> Pretty.from_ML |> Pretty.string_of
              |> reported_text (position_of loc) Markup.ML_typing)
      | reported loc (PolyML.PTdeclaredAt decl) =
          cons (reported_entity Markup.ML_defN loc decl)
      | reported loc (PolyML.PTopenedAt decl) =
          cons (reported_entity Markup.ML_openN loc decl)
      | reported loc (PolyML.PTstructureAt decl) =
          cons (reported_entity Markup.ML_structN loc decl)
      | reported _ (PolyML.PTnextSibling tree) = reported_tree (tree ())
      | reported _ (PolyML.PTfirstChild tree) = reported_tree (tree ())
      | reported _ _ = I
    and reported_tree (loc, props) = fold (reported loc) props;
  in fn tree => Output.report (implode (reported_tree tree [])) end;


(* eval ML source tokens *)

fun eval verbose pos toks =
  let
    val _ = Secure.secure_mltext ();
    val space = ML_Env.name_space;


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
    fun output_writeln () = writeln (Buffer.content (! writeln_buffer));

    val warnings = Unsynchronized.ref ([]: string list);
    fun warn msg = Unsynchronized.change warnings (cons msg);
    fun output_warnings () = List.app warning (rev (! warnings));

    val error_buffer = Unsynchronized.ref Buffer.empty;
    fun err msg = Unsynchronized.change error_buffer (Buffer.add msg #> Buffer.add "\n");
    fun flush_error () = writeln (Buffer.content (! error_buffer));
    fun raise_error msg = error (Buffer.content (Buffer.add msg (! error_buffer)));

    fun message {message = msg, hard, location = loc, context = _} =
      let
        val pos = position_of loc;
        val txt =
          (if hard then "ML error" else "ML warning") ^ Position.here pos ^ ":\n" ^
          Pretty.string_of (Pretty.from_ML (pretty_ml msg));
      in if hard then err txt else warn txt end;


    (* results *)

    val depth = get_print_depth ();

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
      | SOME parse_tree => report_parse_tree depth space parse_tree);
      (case phase2 of
        NONE => raise STATIC_ERRORS ()
      | SOME code =>
          apply_result
            ((code
              |> Runtime.debugging
              |> Runtime.toplevel_error (err o exn_message)) ())));


    (* compiler invocation *)

    val parameters =
     [PolyML.Compiler.CPOutStream write,
      PolyML.Compiler.CPNameSpace space,
      PolyML.Compiler.CPErrorMessageProc message,
      PolyML.Compiler.CPLineNo (the_default 0 o Position.line_of o get_pos),
      PolyML.Compiler.CPLineOffset (the_default 0 o Position.offset_of o get_pos),
      PolyML.Compiler.CPFileName location_props,
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
              | _ => "Exception- " ^ General.exnMessage exn ^ " raised");
            val _ = output_warnings ();
            val _ = output_writeln ();
          in raise_error exn_msg end;
  in
    if verbose then (output_warnings (); flush_error (); output_writeln ())
    else ()
  end;

end;

