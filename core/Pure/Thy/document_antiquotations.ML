(*  Title:      Pure/Thy/document_antiquotations.ML
    Author:     Makarius

Miscellaneous document antiquotations.
*)

structure Document_Antiquotations: sig end =
struct

(* Markdown errors *)

local

fun markdown_error binding =
  Thy_Output.antiquotation binding (Scan.succeed ())
    (fn {source, ...} => fn _ =>
      error ("Bad Markdown structure: illegal " ^ quote (Binding.name_of binding) ^
        Position.here (Position.no_range_position (#1 (Token.range_of source)))))

in

val _ =
  Theory.setup
   (markdown_error @{binding item} #>
    markdown_error @{binding enum} #>
    markdown_error @{binding descr});

end;


(* control spacing *)

val _ =
  Theory.setup
   (Thy_Output.antiquotation @{binding noindent} (Scan.succeed ()) (K (K "\\noindent")) #>
    Thy_Output.antiquotation @{binding smallskip} (Scan.succeed ()) (K (K "\\smallskip")) #>
    Thy_Output.antiquotation @{binding medskip} (Scan.succeed ()) (K (K "\\medskip")) #>
    Thy_Output.antiquotation @{binding bigskip} (Scan.succeed ()) (K (K "\\bigskip")));


(* control style *)

local

fun control_antiquotation name s1 s2 =
  Thy_Output.antiquotation name (Scan.lift Args.cartouche_input)
    (fn {state, ...} => enclose s1 s2 o Thy_Output.output_text state {markdown = false});

in

val _ =
  Theory.setup
   (control_antiquotation @{binding footnote} "\\footnote{" "}" #>
    control_antiquotation @{binding emph} "\\emph{" "}" #>
    control_antiquotation @{binding bold} "\\textbf{" "}");

end;


(* quasi-formal text (unchecked) *)

local

fun text_antiquotation name =
  Thy_Output.antiquotation name (Scan.lift Args.text_input)
    (fn {context = ctxt, ...} => fn source =>
     (Context_Position.report ctxt (Input.pos_of source)
        (Markup.language_text (Input.is_delimited source));
      Thy_Output.output ctxt [Thy_Output.pretty_text ctxt (Input.source_content source)]));

in

val _ =
  Theory.setup
   (text_antiquotation @{binding text} #>
    text_antiquotation @{binding cartouche});

end;


(* theory text with tokens (unchecked) *)

val _ =
  Theory.setup
    (Thy_Output.antiquotation @{binding theory_text} (Scan.lift Args.text_input)
      (fn {context = ctxt, ...} => fn source =>
        let
          val _ =
            Context_Position.report ctxt (Input.pos_of source)
              (Markup.language_Isar (Input.is_delimited source));

          val keywords = Thy_Header.get_keywords' ctxt;
          val toks =
            Input.source_explode source
            |> not (Config.get ctxt Thy_Output.display) ? Symbol_Pos.trim_lines
            |> Source.of_list
            |> Token.source' true keywords
            |> Source.exhaust;
          val _ = Context_Position.reports_text ctxt (maps (Token.reports keywords) toks);
          val indentation =
            Latex.output_symbols (replicate (Config.get ctxt Thy_Output.indent) Symbol.space);
        in
          implode (map Latex.output_token toks) |>
           (if Config.get ctxt Thy_Output.display then
              split_lines #> map (prefix indentation) #> cat_lines #>
              Latex.environment "isabelle"
            else enclose "\\isa{" "}")
        end));


(* goal state *)

local

fun goal_state name main = Thy_Output.antiquotation name (Scan.succeed ())
  (fn {state, context = ctxt, ...} => fn () =>
    Thy_Output.output ctxt
      [Goal_Display.pretty_goal
        (Config.put Goal_Display.show_main_goal main ctxt)
        (#goal (Proof.goal (Toplevel.proof_of state)))]);

in

val _ = Theory.setup
 (goal_state @{binding goals} true #>
  goal_state @{binding subgoals} false);

end;


(* embedded lemma *)

val _ = Theory.setup
  (Thy_Output.antiquotation @{binding lemma}
    (Scan.lift (Scan.ahead Parse.not_eof) -- Args.prop --
      Scan.lift (Parse.position (Parse.reserved "by") -- Method.parse -- Scan.option Method.parse))
    (fn {source, context = ctxt, ...} => fn ((prop_token, prop), (((_, by_pos), m1), m2)) =>
      let
        val reports = (by_pos, Markup.keyword1) :: maps Method.reports_of (m1 :: the_list m2);
        val _ = Context_Position.reports ctxt reports;

        (* FIXME check proof!? *)
        val _ = ctxt
          |> Proof.theorem NONE (K I) [[(prop, [])]]
          |> Proof.global_terminal_proof (m1, m2);
      in
        Thy_Output.output ctxt
          (Thy_Output.maybe_pretty_source
            Thy_Output.pretty_term ctxt [hd source, prop_token] [prop])
      end));


(* verbatim text *)

val _ =
  Theory.setup
    (Thy_Output.antiquotation @{binding verbatim} (Scan.lift Args.text_input)
      (fn {context = ctxt, ...} => fn source =>
       (Context_Position.report ctxt (Input.pos_of source)
          (Markup.language_verbatim (Input.is_delimited source));
        Thy_Output.verbatim_text ctxt (Input.source_content source))));


(* ML text *)

local

fun ml_text name ml = Thy_Output.antiquotation name (Scan.lift Args.text_input)
  (fn {context = ctxt, ...} => fn source =>
   (ML_Context.eval_in (SOME ctxt) ML_Compiler.flags (Input.pos_of source) (ml source);
    Thy_Output.verbatim_text ctxt (Input.source_content source)));

fun ml_enclose bg en source =
  ML_Lex.read bg @ ML_Lex.read_source false source @ ML_Lex.read en;

in

val _ = Theory.setup
 (ml_text @{binding ML} (ml_enclose "fn _ => (" ");") #>
  ml_text @{binding ML_op} (ml_enclose "fn _ => (op " ");") #>
  ml_text @{binding ML_type} (ml_enclose "val _ = NONE : (" ") option;") #>
  ml_text @{binding ML_structure}
    (ml_enclose "functor XXX() = struct structure XX = " " end;") #>

  ml_text @{binding ML_functor}   (* FIXME formal treatment of functor name (!?) *)
    (fn source =>
      ML_Lex.read ("ML_Env.check_functor " ^
        ML_Syntax.print_string (Input.source_content source))) #>

  ml_text @{binding ML_text} (K []));

end;


(* URLs *)

val _ = Theory.setup
  (Thy_Output.antiquotation @{binding url} (Scan.lift (Parse.position Parse.embedded))
    (fn {context = ctxt, ...} => fn (name, pos) =>
      (Context_Position.reports ctxt [(pos, Markup.language_path), (pos, Markup.url name)];
       enclose "\\url{" "}" name)));


(* doc entries *)

val _ = Theory.setup
  (Thy_Output.antiquotation @{binding doc} (Scan.lift (Parse.position Parse.embedded))
    (fn {context = ctxt, ...} => fn (name, pos) =>
      (Context_Position.report ctxt pos (Markup.doc name);
        Thy_Output.output ctxt [Thy_Output.pretty_text ctxt name])));


(* formal entities *)

fun entity_antiquotation name check output =
  Thy_Output.antiquotation name (Scan.lift (Parse.position Args.name))
    (fn {context = ctxt, ...} => fn (name, pos) => (check ctxt (name, pos); output name));

val _ =
  Theory.setup
   (entity_antiquotation @{binding command} Outer_Syntax.check_command
     (enclose "\\isacommand{" "}" o Output.output) #>
    entity_antiquotation @{binding method} Method.check_name
     (enclose "\\isa{" "}" o Output.output) #>
    entity_antiquotation @{binding attribute} Attrib.check_name
     (enclose "\\isa{" "}" o Output.output));

end;
