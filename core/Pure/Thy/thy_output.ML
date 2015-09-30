(*  Title:      Pure/Thy/thy_output.ML
    Author:     Markus Wenzel, TU Muenchen

Theory document output with antiquotations.
*)

signature THY_OUTPUT =
sig
  val display: bool Config.T
  val quotes: bool Config.T
  val margin: int Config.T
  val indent: int Config.T
  val source: bool Config.T
  val break: bool Config.T
  val modes: string Config.T
  val add_wrapper: ((unit -> string) -> unit -> string) -> Proof.context -> Proof.context
  val add_option: binding -> (string -> Proof.context -> Proof.context) -> theory -> theory
  val check_command: Proof.context -> xstring * Position.T -> string
  val check_option: Proof.context -> xstring * Position.T -> string
  val print_antiquotations: bool -> Proof.context -> unit
  val antiquotation: binding -> 'a context_parser ->
    ({source: Token.src, state: Toplevel.state, context: Proof.context} -> 'a -> string) ->
      theory -> theory
  val boolean: string -> bool
  val integer: string -> int
  val eval_antiq: Toplevel.state -> Antiquote.antiq -> string
  val check_text: Input.source -> Toplevel.state -> unit
  val present_thy: theory -> (Toplevel.transition * Toplevel.state) list -> Token.T list -> Buffer.T
  val pretty_text: Proof.context -> string -> Pretty.T
  val pretty_term: Proof.context -> term -> Pretty.T
  val pretty_thm: Proof.context -> thm -> Pretty.T
  val str_of_source: Token.src -> string
  val maybe_pretty_source: (Proof.context -> 'a -> Pretty.T) -> Proof.context ->
    Token.src -> 'a list -> Pretty.T list
  val string_of_margin: Proof.context -> Pretty.T -> string
  val output: Proof.context -> Pretty.T list -> string
  val verbatim_text: Proof.context -> string -> string
  val old_header_command: Input.source -> Toplevel.transition -> Toplevel.transition
  val document_command: (xstring * Position.T) option * Input.source ->
    Toplevel.transition -> Toplevel.transition
end;

structure Thy_Output: THY_OUTPUT =
struct

(** options **)

val display = Attrib.setup_option_bool ("thy_output_display", @{here});
val break = Attrib.setup_option_bool ("thy_output_break", @{here});
val quotes = Attrib.setup_option_bool ("thy_output_quotes", @{here});
val margin = Attrib.setup_option_int ("thy_output_margin", @{here});
val indent = Attrib.setup_option_int ("thy_output_indent", @{here});
val source = Attrib.setup_option_bool ("thy_output_source", @{here});
val modes = Attrib.setup_option_string ("thy_output_modes", @{here});


structure Wrappers = Proof_Data
(
  type T = ((unit -> string) -> unit -> string) list;
  fun init _ = [];
);

fun add_wrapper wrapper = Wrappers.map (cons wrapper);

val wrap = Wrappers.get #> fold (fn wrapper => fn f => wrapper f);



(** maintain global antiquotations **)

structure Antiquotations = Theory_Data
(
  type T =
    (Token.src -> Toplevel.state -> Proof.context -> string) Name_Space.table *
      (string -> Proof.context -> Proof.context) Name_Space.table;
  val empty : T =
    (Name_Space.empty_table Markup.document_antiquotationN,
      Name_Space.empty_table Markup.document_antiquotation_optionN);
  val extend = I;
  fun merge ((commands1, options1), (commands2, options2)) : T =
    (Name_Space.merge_tables (commands1, commands2),
      Name_Space.merge_tables (options1, options2));
);

val get_antiquotations = Antiquotations.get o Proof_Context.theory_of;

fun add_command name cmd thy = thy
  |> Antiquotations.map (apfst (Name_Space.define (Context.Theory thy) true (name, cmd) #> snd));

fun add_option name opt thy = thy
  |> Antiquotations.map (apsnd (Name_Space.define (Context.Theory thy) true (name, opt) #> snd));

fun check_command ctxt = #1 o Name_Space.check (Context.Proof ctxt) (#1 (get_antiquotations ctxt));

fun check_option ctxt = #1 o Name_Space.check (Context.Proof ctxt) (#2 (get_antiquotations ctxt));

fun command src state ctxt =
  let val (src', f) = Token.check_src ctxt (#1 (get_antiquotations ctxt)) src
  in f src' state ctxt end;

fun option ((xname, pos), s) ctxt =
  let
    val (_, opt) =
      Name_Space.check (Context.Proof ctxt) (#2 (get_antiquotations ctxt)) (xname, pos);
  in opt s ctxt end;

fun print_antiquotations verbose ctxt =
  let
    val (commands, options) = get_antiquotations ctxt;
    val command_names = map #1 (Name_Space.markup_table verbose ctxt commands);
    val option_names = map #1 (Name_Space.markup_table verbose ctxt options);
  in
    [Pretty.big_list "document antiquotations:" (map Pretty.mark_str command_names),
     Pretty.big_list "document antiquotation options:" (map Pretty.mark_str option_names)]
  end |> Pretty.writeln_chunks;

fun antiquotation name scan body =
  add_command name
    (fn src => fn state => fn ctxt =>
      let val (x, ctxt') = Token.syntax scan src ctxt
      in body {source = src, state = state, context = ctxt'} x end);



(** syntax of antiquotations **)

(* option values *)

fun boolean "" = true
  | boolean "true" = true
  | boolean "false" = false
  | boolean s = error ("Bad boolean value: " ^ quote s);

fun integer s =
  let
    fun int ss =
      (case Library.read_int ss of (i, []) => i
      | _ => error ("Bad integer value: " ^ quote s));
  in (case Symbol.explode s of "-" :: ss => ~ (int ss) | ss => int ss) end;


(* outer syntax *)

local

val property =
  Parse.position Parse.xname -- Scan.optional (Parse.$$$ "=" |-- Parse.!!! Parse.xname) "";

val properties =
  Scan.optional (Parse.$$$ "[" |-- Parse.!!! (Parse.enum "," property --| Parse.$$$ "]")) [];

in

val antiq =
  Parse.!!!
    (Parse.position Parse.liberal_name -- properties -- Parse.args --| Scan.ahead Parse.eof)
  >> (fn ((name, props), args) => (props, Token.src name args));

end;


(* eval_antiq *)

fun eval_antiq state ((ss, {range = (pos, _), ...}): Antiquote.antiq) =
  let
    val keywords =
      (case try Toplevel.presentation_context_of state of
        SOME ctxt => Thy_Header.get_keywords' ctxt
      | NONE =>
          error ("Unknown context -- cannot expand document antiquotations" ^
            Position.here pos));

    val (opts, src) = Token.read_antiq keywords antiq (ss, pos);
    fun cmd ctxt = wrap ctxt (fn () => command src state ctxt) ();

    val preview_ctxt = fold option opts (Toplevel.presentation_context_of state);
    val print_ctxt = Context_Position.set_visible false preview_ctxt;
    val _ = cmd preview_ctxt;
    val print_modes = space_explode "," (Config.get print_ctxt modes) @ Latex.modes;
  in Print_Mode.with_modes print_modes (fn () => cmd print_ctxt) () end;


(* check_text *)

fun eval_antiquote state source =
  let
    fun words (Antiquote.Text ss) = [(#1 (Symbol_Pos.range ss), Markup.words)]
      | words (Antiquote.Antiq _) = [];

    fun expand (Antiquote.Text ss) = Symbol_Pos.content ss
      | expand (Antiquote.Antiq antiq) = eval_antiq state antiq;

    val ants = Antiquote.read source;
    val _ = Position.reports (maps words ants);
  in implode (map expand ants) end;

fun check_text source state =
 (Position.report (Input.pos_of source) (Markup.language_document (Input.is_delimited source));
  if Toplevel.is_skipped_proof state then ()
  else ignore (eval_antiquote state source));



(** present theory source **)

(*NB: arranging white space around command spans is a black art*)

(* presentation tokens *)

datatype token =
    No_Token
  | Basic_Token of Token.T
  | Markup_Token of string * Input.source
  | Markup_Env_Token of string * Input.source
  | Verbatim_Token of Input.source;

fun output_token state =
  let val eval = eval_antiquote state in
    fn No_Token => ""
     | Basic_Token tok => Latex.output_basic tok
     | Markup_Token (cmd, source) => Latex.output_markup cmd (eval source)
     | Markup_Env_Token (cmd, source) => Latex.output_markup_env cmd (eval source)
     | Verbatim_Token source => Latex.output_verbatim (eval source)
  end;

fun basic_token pred (Basic_Token tok) = pred tok
  | basic_token _ _ = false;

val improper_token = basic_token Token.is_improper;
val comment_token = basic_token Token.is_comment;
val blank_token = basic_token Token.is_blank;
val newline_token = basic_token Token.is_newline;


(* command spans *)

type command = string * Position.T * string list;   (*name, position, tags*)
type source = (token * (string * int)) list;        (*token, markup flag, meta-comment depth*)

datatype span = Span of command * (source * source * source * source) * bool;

fun make_span cmd src =
  let
    fun take_newline (tok :: toks) =
          if newline_token (fst tok) then ([tok], toks, true)
          else ([], tok :: toks, false)
      | take_newline [] = ([], [], false);
    val (((src_prefix, src_main), src_suffix1), (src_suffix2, src_appendix, newline)) =
      src
      |> take_prefix (improper_token o fst)
      ||>> take_suffix (improper_token o fst)
      ||>> take_prefix (comment_token o fst)
      ||> take_newline;
  in Span (cmd, (src_prefix, src_main, src_suffix1 @ src_suffix2, src_appendix), newline) end;


(* present spans *)

local

fun err_bad_nesting pos =
  error ("Bad nesting of commands in presentation" ^ pos);

fun edge which f (x: string option, y) =
  if x = y then I
  else (case which (x, y) of NONE => I | SOME txt => Buffer.add (f txt));

val begin_tag = edge #2 Latex.begin_tag;
val end_tag = edge #1 Latex.end_tag;
fun open_delim delim e = edge #2 Latex.begin_delim e #> delim #> edge #2 Latex.end_delim e;
fun close_delim delim e = edge #1 Latex.begin_delim e #> delim #> edge #1 Latex.end_delim e;

in

fun present_span keywords span state state' (tag_stack, active_tag, newline, buffer, present_cont) =
  let
    val present = fold (fn (tok, (flag, 0)) =>
        Buffer.add (output_token state' tok)
        #> Buffer.add flag
      | _ => I);

    val Span ((cmd_name, cmd_pos, cmd_tags), srcs, span_newline) = span;

    val (tag, tags) = tag_stack;
    val tag' = try hd (fold (update (op =)) cmd_tags (the_list tag));

    val nesting = Toplevel.level state' - Toplevel.level state;

    val active_tag' =
      if is_some tag' then tag'
      else if cmd_name = "end" andalso not (Toplevel.is_toplevel state') then NONE
      else
        (case Keyword.command_tags keywords cmd_name of
          default_tag :: _ => SOME default_tag
        | [] =>
            if Keyword.is_vacuous keywords cmd_name andalso Toplevel.is_proof state
            then active_tag
            else NONE);

    val edge = (active_tag, active_tag');

    val newline' =
      if is_none active_tag' then span_newline else newline;

    val tag_stack' =
      if nesting = 0 andalso not (Toplevel.is_proof state) then tag_stack
      else if nesting >= 0 then (tag', replicate nesting tag @ tags)
      else
        (case drop (~ nesting - 1) tags of
          tg :: tgs => (tg, tgs)
        | [] => err_bad_nesting (Position.here cmd_pos));

    val buffer' =
      buffer
      |> end_tag edge
      |> close_delim (fst present_cont) edge
      |> snd present_cont
      |> open_delim (present (#1 srcs)) edge
      |> begin_tag edge
      |> present (#2 srcs);
    val present_cont' =
      if newline then (present (#3 srcs), present (#4 srcs))
      else (I, present (#3 srcs) #> present (#4 srcs));
  in (tag_stack', active_tag', newline', buffer', present_cont') end;

fun present_trailer ((_, tags), active_tag, _, buffer, present_cont) =
  if not (null tags) then err_bad_nesting " at end of theory"
  else
    buffer
    |> end_tag (active_tag, NONE)
    |> close_delim (fst present_cont) (active_tag, NONE)
    |> snd present_cont;

end;


(* present_thy *)

local

val space_proper =
  Scan.one Token.is_blank -- Scan.many Token.is_comment -- Scan.one Token.is_proper;

val is_improper = not o (Token.is_proper orf Token.is_begin_ignore orf Token.is_end_ignore);
val improper = Scan.many is_improper;
val improper_end = Scan.repeat (Scan.unless space_proper (Scan.one is_improper));
val blank_end = Scan.repeat (Scan.unless space_proper (Scan.one Token.is_blank));

val opt_newline = Scan.option (Scan.one Token.is_newline);

val ignore =
  Scan.depend (fn d => opt_newline |-- Scan.one Token.is_begin_ignore
    >> pair (d + 1)) ||
  Scan.depend (fn d => Scan.one Token.is_end_ignore --|
    (if d = 0 then Scan.fail_with (K (fn () => "Bad nesting of meta-comments")) else opt_newline)
    >> pair (d - 1));

val tag = (improper -- Parse.$$$ "%" -- improper) |-- Parse.!!! (Parse.tag_name --| blank_end);

val locale =
  Scan.option ((Parse.$$$ "(" -- improper -- Parse.$$$ "in") |--
    Parse.!!! (improper |-- Parse.xname --| (improper -- Parse.$$$ ")")));

in

fun present_thy thy command_results toks =
  let
    val keywords = Thy_Header.get_keywords thy;


    (* tokens *)

    val ignored = Scan.state --| ignore
      >> (fn d => (NONE, (No_Token, ("", d))));

    fun markup pred mk flag = Scan.peek (fn d =>
      improper |--
        Parse.position (Scan.one (fn tok =>
          Token.is_command tok andalso pred keywords (Token.content_of tok))) --
      Scan.repeat tag --
      Parse.!!!! ((improper -- locale -- improper) |-- Parse.document_source --| improper_end)
      >> (fn (((tok, pos'), tags), source) =>
        let val name = Token.content_of tok
        in (SOME (name, pos', tags), (mk (name, source), (flag, d))) end));

    val command = Scan.peek (fn d =>
      Scan.optional (Scan.one Token.is_command_modifier -- improper >> op ::) [] --
      Scan.one Token.is_command -- Scan.repeat tag
      >> (fn ((cmd_mod, cmd), tags) =>
        map (fn tok => (NONE, (Basic_Token tok, ("", d)))) cmd_mod @
          [(SOME (Token.content_of cmd, Token.pos_of cmd, tags),
            (Basic_Token cmd, (Latex.markup_false, d)))]));

    val cmt = Scan.peek (fn d =>
      Parse.$$$ "--" |-- Parse.!!!! (improper |-- Parse.document_source) >>
        (fn source => (NONE, (Markup_Token ("cmt", source), ("", d)))));

    val other = Scan.peek (fn d =>
       Parse.not_eof >> (fn tok => (NONE, (Basic_Token tok, ("", d)))));

    val tokens =
      (ignored ||
        markup Keyword.is_document_heading Markup_Token Latex.markup_true ||
        markup Keyword.is_document_body Markup_Env_Token Latex.markup_true ||
        markup Keyword.is_document_raw (Verbatim_Token o #2) "") >> single ||
      command ||
      (cmt || other) >> single;


    (* spans *)

    val is_eof = fn (_, (Basic_Token x, _)) => Token.is_eof x | _ => false;
    val stopper = Scan.stopper (K (NONE, (Basic_Token Token.eof, ("", 0)))) is_eof;

    val cmd = Scan.one (is_some o fst);
    val non_cmd = Scan.one (is_none o fst andf not o is_eof) >> #2;

    val comments = Scan.many (comment_token o fst o snd);
    val blank = Scan.one (blank_token o fst o snd);
    val newline = Scan.one (newline_token o fst o snd);
    val before_cmd =
      Scan.option (newline -- comments) --
      Scan.option (newline -- comments) --
      Scan.option (blank -- comments) -- cmd;

    val span =
      Scan.repeat non_cmd -- cmd --
        Scan.repeat (Scan.unless before_cmd non_cmd) --
        Scan.option (newline >> (single o snd))
      >> (fn (((toks1, (cmd, tok2)), toks3), tok4) =>
          make_span (the cmd) (toks1 @ (tok2 :: (toks3 @ the_default [] tok4))));

    val spans = toks
      |> take_suffix Token.is_space |> #1
      |> Source.of_list
      |> Source.source' 0 Token.stopper (Scan.error (Scan.bulk tokens >> flat))
      |> Source.source stopper (Scan.error (Scan.bulk span))
      |> Source.exhaust;


    (* present commands *)

    fun present_command tr span st st' =
      Toplevel.setmp_thread_position tr (present_span keywords span st st');

    fun present _ [] = I
      | present st (((tr, st'), span) :: rest) = present_command tr span st st' #> present st' rest;
  in
    if length command_results = length spans then
      ((NONE, []), NONE, true, Buffer.empty, (I, I))
      |> present Toplevel.toplevel (command_results ~~ spans)
      |> present_trailer
    else error "Messed-up outer syntax for presentation"
  end;

end;



(** setup default output **)

(* options *)

val _ = Theory.setup
 (add_option @{binding show_types} (Config.put show_types o boolean) #>
  add_option @{binding show_sorts} (Config.put show_sorts o boolean) #>
  add_option @{binding show_structs} (Config.put show_structs o boolean) #>
  add_option @{binding show_question_marks} (Config.put show_question_marks o boolean) #>
  add_option @{binding show_abbrevs} (Config.put show_abbrevs o boolean) #>
  add_option @{binding names_long} (Config.put Name_Space.names_long o boolean) #>
  add_option @{binding names_short} (Config.put Name_Space.names_short o boolean) #>
  add_option @{binding names_unique} (Config.put Name_Space.names_unique o boolean) #>
  add_option @{binding eta_contract} (Config.put Syntax_Trans.eta_contract o boolean) #>
  add_option @{binding display} (Config.put display o boolean) #>
  add_option @{binding break} (Config.put break o boolean) #>
  add_option @{binding quotes} (Config.put quotes o boolean) #>
  add_option @{binding mode} (add_wrapper o Print_Mode.with_modes o single) #>
  add_option @{binding margin} (Config.put margin o integer) #>
  add_option @{binding indent} (Config.put indent o integer) #>
  add_option @{binding source} (Config.put source o boolean) #>
  add_option @{binding goals_limit} (Config.put Goal_Display.goals_limit o integer));


(* basic pretty printing *)

fun tweak_line ctxt s =
  if Config.get ctxt display then s else Symbol.strip_blanks s;

fun pretty_text ctxt =
  Pretty.chunks o map Pretty.str o map (tweak_line ctxt) o split_lines;

fun pretty_text_report ctxt source =
 (Context_Position.report ctxt (Input.pos_of source)
    (Markup.language_text (Input.is_delimited source));
  pretty_text ctxt (Input.source_content source));

fun pretty_term ctxt t = Syntax.pretty_term (Variable.auto_fixes t ctxt) t;

fun pretty_thm ctxt = pretty_term ctxt o Thm.full_prop_of;

fun pretty_term_style ctxt (style, t) =
  pretty_term ctxt (style t);

fun pretty_thm_style ctxt (style, th) =
  pretty_term ctxt (style (Thm.full_prop_of th));

fun pretty_term_typ ctxt (style, t) =
  let val t' = style t
  in pretty_term ctxt (Type.constraint (Term.fastype_of t') t') end;

fun pretty_term_typeof ctxt (style, t) =
  Syntax.pretty_typ ctxt (Term.fastype_of (style t));

fun pretty_const ctxt c =
  let
    val t = Const (c, Consts.type_scheme (Proof_Context.consts_of ctxt) c)
      handle TYPE (msg, _, _) => error msg;
    val ([t'], _) = Variable.import_terms true [t] ctxt;
  in pretty_term ctxt t' end;

fun pretty_abbrev ctxt s =
  let
    val t = Syntax.read_term (Proof_Context.set_mode Proof_Context.mode_abbrev ctxt) s;
    fun err () = error ("Abbreviated constant expected: " ^ Syntax.string_of_term ctxt t);
    val (head, args) = Term.strip_comb t;
    val (c, T) = Term.dest_Const head handle TERM _ => err ();
    val (U, u) = Consts.the_abbreviation (Proof_Context.consts_of ctxt) c
      handle TYPE _ => err ();
    val t' = Term.betapplys (Envir.expand_atom T (U, u), args);
    val eq = Logic.mk_equals (t, t');
    val ctxt' = Variable.auto_fixes eq ctxt;
  in Proof_Context.pretty_term_abbrev ctxt' eq end;

fun pretty_class ctxt =
  Pretty.str o Proof_Context.extern_class ctxt o Proof_Context.read_class ctxt;

fun pretty_type ctxt s =
  let val Type (name, _) = Proof_Context.read_type_name {proper = true, strict = false} ctxt s
  in Pretty.str (Proof_Context.extern_type ctxt name) end;

fun pretty_prf full ctxt = Proof_Syntax.pretty_proof_of ctxt full;

fun pretty_theory ctxt (name, pos) = (Theory.check ctxt (name, pos); Pretty.str name);


(* default output *)

val str_of_source = space_implode " " o Token.unparse_src;

fun maybe_pretty_source pretty ctxt src xs =
  map (pretty ctxt) xs  (*always pretty in order to exhibit errors!*)
  |> (if Config.get ctxt source then K [pretty_text ctxt (str_of_source src)] else I);

fun string_of_margin ctxt = Pretty.string_of_margin (Config.get ctxt margin);

fun output ctxt prts =
  prts
  |> Config.get ctxt quotes ? map Pretty.quote
  |> (if Config.get ctxt display then
        map (Pretty.indent (Config.get ctxt indent) #> string_of_margin ctxt #> Output.output)
        #> space_implode "\\isasep\\isanewline%\n"
        #> enclose "\\begin{isabelle}%\n" "%\n\\end{isabelle}"
      else
        map ((if Config.get ctxt break then string_of_margin ctxt else Pretty.str_of) #>
          Output.output)
        #> space_implode "\\isasep\\isanewline%\n"
        #> enclose "\\isa{" "}");



(** concrete antiquotations **)

(* basic entities *)

local

fun basic_entities name scan pretty =
  antiquotation name scan (fn {source, context, ...} =>
    output context o maybe_pretty_source pretty context source);

fun basic_entities_style name scan pretty =
  antiquotation name scan (fn {source, context, ...} => fn (style, xs) =>
    output context
      (maybe_pretty_source (fn ctxt => fn x => pretty ctxt (style, x)) context source xs));

fun basic_entity name scan = basic_entities name (scan >> single);

in

val _ = Theory.setup
 (basic_entities_style @{binding thm} (Term_Style.parse -- Attrib.thms) pretty_thm_style #>
  basic_entity @{binding prop} (Term_Style.parse -- Args.prop) pretty_term_style #>
  basic_entity @{binding term} (Term_Style.parse -- Args.term) pretty_term_style #>
  basic_entity @{binding term_type} (Term_Style.parse -- Args.term) pretty_term_typ #>
  basic_entity @{binding typeof} (Term_Style.parse -- Args.term) pretty_term_typeof #>
  basic_entity @{binding const} (Args.const {proper = true, strict = false}) pretty_const #>
  basic_entity @{binding abbrev} (Scan.lift Args.name_inner_syntax) pretty_abbrev #>
  basic_entity @{binding typ} Args.typ_abbrev Syntax.pretty_typ #>
  basic_entity @{binding class} (Scan.lift Args.name_inner_syntax) pretty_class #>
  basic_entity @{binding type} (Scan.lift Args.name) pretty_type #>
  basic_entity @{binding text} (Scan.lift Args.text_input) pretty_text_report #>
  basic_entities @{binding prf} Attrib.thms (pretty_prf false) #>
  basic_entities @{binding full_prf} Attrib.thms (pretty_prf true) #>
  basic_entity @{binding theory} (Scan.lift (Parse.position Args.name)) pretty_theory);

end;


(* goal state *)

local

fun goal_state name main = antiquotation name (Scan.succeed ())
  (fn {state, context = ctxt, ...} => fn () =>
    output ctxt
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
  (antiquotation @{binding lemma}
    (Scan.lift (Scan.ahead Parse.not_eof) -- Args.prop --
      Scan.lift (Parse.position (Parse.reserved "by") -- Method.parse -- Scan.option Method.parse))
    (fn {source, context = ctxt, ...} => fn ((prop_token, prop), (((_, by_pos), m1), m2)) =>
      let
        val prop_src = Token.src (Token.name_of_src source) [prop_token];

        val reports = (by_pos, Markup.keyword1) :: maps Method.reports_of (m1 :: the_list m2);
        val _ = Context_Position.reports ctxt reports;

        (* FIXME check proof!? *)
        val _ = ctxt
          |> Proof.theorem NONE (K I) [[(prop, [])]]
          |> Proof.global_terminal_proof (m1, m2);
      in output ctxt (maybe_pretty_source pretty_term ctxt prop_src [prop]) end));


(* verbatim text *)

fun verbatim_text ctxt =
  if Config.get ctxt display then
    Latex.output_ascii #>
    enclose "\\begin{isabellett}%\n" "%\n\\end{isabellett}"
  else
    split_lines #>
    map (Latex.output_ascii #> enclose "\\isatt{" "}") #>
    space_implode "\\isasep\\isanewline%\n";

val _ =
  Theory.setup
    (antiquotation @{binding verbatim} (Scan.lift Args.text) (verbatim_text o #context));


(* ML text *)

local

fun ml_text name ml = antiquotation name (Scan.lift Args.text_input)
  (fn {context = ctxt, ...} => fn source =>
   (ML_Context.eval_in (SOME ctxt) ML_Compiler.flags (Input.pos_of source) (ml source);
    verbatim_text ctxt (Input.source_content source)));

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
  (antiquotation @{binding url} (Scan.lift (Parse.position Parse.name))
    (fn {context = ctxt, ...} => fn (name, pos) =>
      (Context_Position.reports ctxt [(pos, Markup.language_path), (pos, Markup.url name)];
       enclose "\\url{" "}" name)));



(** document commands **)

fun old_header_command txt =
  Toplevel.keep (fn state =>
    if Toplevel.is_toplevel state then
     (legacy_feature "Obsolete 'header' command -- use 'chapter', 'section' etc. instead";
      check_text txt state)
    else raise Toplevel.UNDEF);

fun document_command (loc, txt) =
  Toplevel.keep (fn state =>
    (case loc of
      NONE => check_text txt state
    | SOME (_, pos) =>
        error ("Illegal target specification -- not a theory context" ^ Position.here pos))) o
  Toplevel.present_local_theory loc (check_text txt);

end;
