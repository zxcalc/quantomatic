(*  Title:      Pure/Syntax/syntax_phases.ML
    Author:     Makarius

Main phases of inner syntax processing, with standard implementations
of parse/unparse operations.
*)

signature SYNTAX_PHASES =
sig
  val decode_sort: term -> sort
  val decode_typ: term -> typ
  val decode_term: Proof.context ->
    Position.report_text list * term Exn.result -> Position.report_text list * term Exn.result
  val parse_ast_pattern: Proof.context -> string * string -> Ast.ast
  val term_of_typ: Proof.context -> typ -> term
  val print_checks: Proof.context -> unit
  val typ_check: int -> string -> (Proof.context -> typ list -> typ list) ->
    Context.generic -> Context.generic
  val term_check: int -> string -> (Proof.context -> term list -> term list) ->
    Context.generic -> Context.generic
  val typ_uncheck: int -> string -> (Proof.context -> typ list -> typ list) ->
    Context.generic -> Context.generic
  val term_uncheck: int -> string -> (Proof.context -> term list -> term list) ->
    Context.generic -> Context.generic
  val typ_check': int -> string ->
    (typ list -> Proof.context -> (typ list * Proof.context) option) ->
    Context.generic -> Context.generic
  val term_check': int -> string ->
    (term list -> Proof.context -> (term list * Proof.context) option) ->
    Context.generic -> Context.generic
  val typ_uncheck': int -> string ->
    (typ list -> Proof.context -> (typ list * Proof.context) option) ->
    Context.generic -> Context.generic
  val term_uncheck': int -> string ->
    (term list -> Proof.context -> (term list * Proof.context) option) ->
    Context.generic -> Context.generic
end

structure Syntax_Phases: SYNTAX_PHASES =
struct

(** markup logical entities **)

fun markup_class ctxt c =
  [Name_Space.markup (Type.class_space (Proof_Context.tsig_of ctxt)) c];

fun markup_type ctxt c =
  [Name_Space.markup (Type.type_space (Proof_Context.tsig_of ctxt)) c];

fun markup_const ctxt c =
  [Name_Space.markup (Consts.space_of (Proof_Context.consts_of ctxt)) c];

fun markup_free ctxt x =
  [if Name.is_skolem x then Markup.skolem else Markup.free] @
  (if Variable.is_body ctxt orelse Variable.is_fixed ctxt x
   then [Variable.markup_fixed ctxt x]
   else []);

fun markup_var xi = [Markup.name (Term.string_of_vname xi) Markup.var];

fun markup_bound def ps (name, id) =
  let val entity = Markup.entity Markup.boundN name in
    Markup.bound ::
      map (fn pos => Markup.properties (Position.entity_properties_of def id pos) entity) ps
  end;

fun markup_entity ctxt c =
  (case Syntax.lookup_const (Proof_Context.syn_of ctxt) c of
    SOME "" => []
  | SOME b => markup_entity ctxt b
  | NONE => c |> Lexicon.unmark
     {case_class = markup_class ctxt,
      case_type = markup_type ctxt,
      case_const = markup_const ctxt,
      case_fixed = markup_free ctxt,
      case_default = K []});



(** decode parse trees **)

(* decode_sort *)

fun decode_sort tm =
  let
    fun err () = raise TERM ("decode_sort: bad encoding of classes", [tm]);

    fun class s = Lexicon.unmark_class s handle Fail _ => err ();

    fun classes (Const (s, _)) = [class s]
      | classes (Const ("_classes", _) $ Const (s, _) $ cs) = class s :: classes cs
      | classes _ = err ();

    fun sort (Const ("_topsort", _)) = []
      | sort (Const ("_sort", _) $ cs) = classes cs
      | sort (Const (s, _)) = [class s]
      | sort _ = err ();
  in sort tm end;


(* decode_typ *)

fun decode_pos (Free (s, _)) =
      if is_some (Term_Position.decode s) then SOME s else NONE
  | decode_pos _ = NONE;

fun decode_typ tm =
  let
    fun err () = raise TERM ("decode_typ: bad encoding of type", [tm]);

    fun typ ps sort tm =
      (case tm of
        Const ("_tfree", _) $ t => typ ps sort t
      | Const ("_tvar", _) $ t => typ ps sort t
      | Const ("_ofsort", _) $ t $ s =>
          (case decode_pos s of
            SOME p => typ (p :: ps) sort t
          | NONE =>
              if is_none sort then typ ps (SOME (decode_sort s)) t
              else err ())
      | Const ("_dummy_ofsort", _) $ s => TFree ("'_dummy_", decode_sort s)
      | Free (x, _) => TFree (x, ps @ the_default dummyS sort)
      | Var (xi, _) => TVar (xi, ps @ the_default dummyS sort)
      | _ =>
          if null ps andalso is_none sort then
            let
              val (head, args) = Term.strip_comb tm;
              val a =
                (case head of
                  Const (c, _) => (Lexicon.unmark_type c handle Fail _ => err ())
                | _ => err ());
            in Type (a, map (typ [] NONE) args) end
          else err ());
  in typ [] NONE tm end;


(* parsetree_to_ast *)

fun parsetree_to_ast ctxt trf parsetree =
  let
    val reports = Unsynchronized.ref ([]: Position.report_text list);
    fun report pos = Position.store_reports reports [pos];
    val append_reports = Position.append_reports reports;

    fun trans a args =
      (case trf a of
        NONE => Ast.mk_appl (Ast.Constant a) args
      | SOME f => f ctxt args);

    fun asts_of_token tok =
      if Lexicon.valued_token tok
      then [Ast.Variable (Lexicon.str_of_token tok)]
      else [];

    fun ast_of_position tok =
      Ast.Variable (Term_Position.encode (Lexicon.pos_of_token tok));

    fun ast_of_dummy a tok =
      Ast.Appl [Ast.Constant "_constrain", Ast.Constant a, ast_of_position tok];

    fun asts_of_position c tok =
      [Ast.Appl [Ast.Constant c, ast_of (Parser.Tip tok), ast_of_position tok]]

    and asts_of (Parser.Node ("_class_name", [Parser.Tip tok])) =
          let
            val pos = Lexicon.pos_of_token tok;
            val (c, rs) = Proof_Context.check_class ctxt (Lexicon.str_of_token tok, pos);
            val _ = append_reports rs;
          in [Ast.Constant (Lexicon.mark_class c)] end
      | asts_of (Parser.Node ("_type_name", [Parser.Tip tok])) =
          let
            val pos = Lexicon.pos_of_token tok;
            val (Type (c, _), rs) =
              Proof_Context.check_type_name {proper = true, strict = false} ctxt
                (Lexicon.str_of_token tok, pos);
            val _ = append_reports rs;
          in [Ast.Constant (Lexicon.mark_type c)] end
      | asts_of (Parser.Node ("_position", [Parser.Tip tok])) = asts_of_position "_constrain" tok
      | asts_of (Parser.Node ("_position_sort", [Parser.Tip tok])) = asts_of_position "_ofsort" tok
      | asts_of (Parser.Node (a as "\\<^const>Pure.dummy_pattern", [Parser.Tip tok])) =
          [ast_of_dummy a tok]
      | asts_of (Parser.Node (a as "_idtdummy", [Parser.Tip tok])) =
          [ast_of_dummy a tok]
      | asts_of (Parser.Node ("_idtypdummy", pts as [Parser.Tip tok, _, _])) =
          [Ast.Appl (Ast.Constant "_constrain" :: ast_of_dummy "_idtdummy" tok :: maps asts_of pts)]
      | asts_of (Parser.Node (a, pts)) =
          let
            val _ = pts |> List.app
              (fn Parser.Node _ => () | Parser.Tip tok =>
                if Lexicon.valued_token tok then ()
                else report (Lexicon.pos_of_token tok) (markup_entity ctxt) a);
          in [trans a (maps asts_of pts)] end
      | asts_of (Parser.Tip tok) = asts_of_token tok

    and ast_of pt =
      (case asts_of pt of
        [ast] => ast
      | asts => raise Ast.AST ("parsetree_to_ast: malformed parsetree", asts));

    val ast = Exn.interruptible_capture ast_of parsetree;
  in (! reports, ast) end;


(* ast_to_term *)

fun ast_to_term ctxt trf =
  let
    fun trans a args =
      (case trf a of
        NONE => Term.list_comb (Syntax.const a, args)
      | SOME f => f ctxt args);

    fun term_of (Ast.Constant a) = trans a []
      | term_of (Ast.Variable x) = Lexicon.read_var x
      | term_of (Ast.Appl (Ast.Constant a :: (asts as _ :: _))) =
          trans a (map term_of asts)
      | term_of (Ast.Appl (ast :: (asts as _ :: _))) =
          Term.list_comb (term_of ast, map term_of asts)
      | term_of (ast as Ast.Appl _) = raise Ast.AST ("ast_to_term: malformed ast", [ast]);
  in term_of end;


(* decode_term -- transform parse tree into raw term *)

fun decode_const ctxt (c, ps) =
  let
    val (Const (c', _), reports) =
      Proof_Context.check_const {proper = true, strict = false} ctxt (c, ps);
  in (c', reports) end;

local

fun get_free ctxt x =
  let
    val fixed = Variable.lookup_fixed ctxt x;
    val is_const = can (decode_const ctxt) (x, []) orelse Long_Name.is_qualified x;
    val is_declared = is_some (Variable.def_type ctxt false (x, ~1));
  in
    if Variable.is_const ctxt x then NONE
    else if is_some fixed then fixed
    else if not is_const orelse is_declared then SOME x
    else NONE
  end;

in

fun decode_term _ (result as (_: Position.report_text list, Exn.Exn _)) = result
  | decode_term ctxt (reports0, Exn.Res tm) =
      let
        val reports = Unsynchronized.ref reports0;
        fun report ps = Position.store_reports reports ps;
        val append_reports = Position.append_reports reports;

        fun decode ps qs bs (Const ("_constrain", _) $ t $ typ) =
              (case Term_Position.decode_position typ of
                SOME (p, T) => Type.constraint T (decode (p :: ps) qs bs t)
              | NONE => Type.constraint (decode_typ typ) (decode ps qs bs t))
          | decode ps qs bs (Const ("_constrainAbs", _) $ t $ typ) =
              (case Term_Position.decode_position typ of
                SOME (q, T) => Type.constraint (T --> dummyT) (decode ps (q :: qs) bs t)
              | NONE => Type.constraint (decode_typ typ --> dummyT) (decode ps qs bs t))
          | decode _ qs bs (Abs (x, T, t)) =
              let
                val id = serial ();
                val _ = report qs (markup_bound true qs) (x, id);
              in Abs (x, T, decode [] [] ((qs, (x, id)) :: bs) t) end
          | decode _ _ bs (t $ u) = decode [] [] bs t $ decode [] [] bs u
          | decode ps _ _ (Const (a, T)) =
              (case try Lexicon.unmark_fixed a of
                SOME x => (report ps (markup_free ctxt) x; Free (x, T))
              | NONE =>
                  let
                    val c =
                      (case try Lexicon.unmark_const a of
                        SOME c => c
                      | NONE => #1 (decode_const ctxt (a, [])));
                    val _ = report ps (markup_const ctxt) c;
                  in Const (c, T) end)
          | decode ps _ _ (Free (a, T)) =
              ((Name.reject_internal (a, ps) handle ERROR msg =>
                  error (msg ^ Proof_Context.consts_completion_message ctxt (a, ps)));
                (case get_free ctxt a of
                  SOME x => (report ps (markup_free ctxt) x; Free (x, T))
                | NONE =>
                    let
                      val (c, rs) = decode_const ctxt (a, ps);
                      val _ = append_reports rs;
                    in Const (c, T) end))
          | decode ps _ _ (Var (xi, T)) = (report ps markup_var xi; Var (xi, T))
          | decode ps _ bs (t as Bound i) =
              (case try (nth bs) i of
                SOME (qs, (x, id)) => (report ps (markup_bound false qs) (x, id); t)
              | NONE => t);

        val tm' = Exn.interruptible_capture (fn () => decode [] [] [] tm) ();
      in (! reports, tm') end;

end;



(** parse **)

(* results *)

fun proper_results results = map_filter (fn (y, Exn.Res x) => SOME (y, x) | _ => NONE) results;
fun failed_results results = map_filter (fn (y, Exn.Exn e) => SOME (y, e) | _ => NONE) results;

fun report_result ctxt pos ambig_msgs results =
  (case (proper_results results, failed_results results) of
    ([], (reports, exn) :: _) => (Context_Position.reports_text ctxt reports; reraise exn)
  | ([(reports, x)], _) => (Context_Position.reports_text ctxt reports; x)
  | _ =>
      if null ambig_msgs then
        error ("Parse error: ambiguous syntax" ^ Position.here pos)
      else error (cat_lines ambig_msgs));


(* parse raw asts *)

fun parse_asts ctxt raw root (syms, pos) =
  let
    val syn = Proof_Context.syn_of ctxt;
    val ast_tr = Syntax.parse_ast_translation syn;

    val toks = Syntax.tokenize syn raw syms;
    val _ = Context_Position.reports ctxt (map Lexicon.report_of_token toks);

    val pts = Syntax.parse syn root (filter Lexicon.is_proper toks)
      handle ERROR msg =>
        error (msg ^ Markup.markup_report (implode (map (Lexicon.reported_token_range ctxt) toks)));
    val len = length pts;

    val limit = Config.get ctxt Syntax.ambiguity_limit;
    val ambig_msgs =
      if len <= 1 then []
      else
        [cat_lines
          (("Ambiguous input" ^ Position.here (Position.reset_range pos) ^
            " produces " ^ string_of_int len ^ " parse trees" ^
            (if len <= limit then "" else " (" ^ string_of_int limit ^ " displayed)") ^ ":") ::
            map (Pretty.string_of o Pretty.item o single o Parser.pretty_parsetree)
              (take limit pts))];

  in (ambig_msgs, map (parsetree_to_ast ctxt ast_tr) pts) end;

fun parse_tree ctxt root input =
  let
    val syn = Proof_Context.syn_of ctxt;
    val tr = Syntax.parse_translation syn;
    val parse_rules = Syntax.parse_rules syn;
    val (ambig_msgs, asts) = parse_asts ctxt false root input;
    val results =
      (map o apsnd o Exn.maps_result)
        (Ast.normalize ctxt parse_rules #> Exn.interruptible_capture (ast_to_term ctxt tr)) asts;
  in (ambig_msgs, results) end;


(* parse logical entities *)

fun parse_failed ctxt pos msg kind =
  cat_error msg ("Failed to parse " ^ kind ^
    Markup.markup_report (Context_Position.reported_text ctxt pos Markup.bad ""));

fun parse_sort ctxt =
  Syntax.parse_token ctxt Term_XML.Decode.sort Markup.language_sort
    (fn (syms, pos) =>
      parse_tree ctxt "sort" (syms, pos)
      |> uncurry (report_result ctxt pos)
      |> decode_sort
      |> Type.minimize_sort (Proof_Context.tsig_of ctxt)
      handle ERROR msg => parse_failed ctxt pos msg "sort");

fun parse_typ ctxt =
  Syntax.parse_token ctxt Term_XML.Decode.typ Markup.language_type
    (fn (syms, pos) =>
      parse_tree ctxt "type" (syms, pos)
      |> uncurry (report_result ctxt pos)
      |> decode_typ
      handle ERROR msg => parse_failed ctxt pos msg "type");

fun parse_term is_prop ctxt =
  let
    val (markup, kind, root, constrain) =
      if is_prop
      then (Markup.language_prop, "prop", "prop", Type.constraint propT)
      else (Markup.language_term, "term", Config.get ctxt Syntax.root, I);
    val decode = constrain o Term_XML.Decode.term;
  in
    Syntax.parse_token ctxt decode markup
      (fn (syms, pos) =>
        let
          val (ambig_msgs, results) = parse_tree ctxt root (syms, pos) ||> map (decode_term ctxt);
          val parsed_len = length (proper_results results);

          val ambiguity_warning = Config.get ctxt Syntax.ambiguity_warning;
          val limit = Config.get ctxt Syntax.ambiguity_limit;

          (*brute-force disambiguation via type-inference*)
          fun check t = (Syntax.check_term ctxt (constrain t); Exn.Res t)
            handle exn as ERROR _ => Exn.Exn exn;

          val results' =
            if parsed_len > 1 then
              (grouped 10 (Par_List.map_name "Syntax_Phases.parse_term") o apsnd o Exn.maps_result)
                check results
            else results;
          val reports' = fst (hd results');

          val errs = map snd (failed_results results');
          val checked = map snd (proper_results results');
          val checked_len = length checked;

          val pretty_term = Syntax.pretty_term (Config.put Printer.show_brackets true ctxt);
        in
          if checked_len = 0 then
            report_result ctxt pos []
              [(reports', Exn.Exn (Exn.EXCEPTIONS (map ERROR ambig_msgs @ errs)))]
          else if checked_len = 1 then
            (if not (null ambig_msgs) andalso ambiguity_warning andalso
                Context_Position.is_visible ctxt then
              warning
                (cat_lines (ambig_msgs @
                  ["Fortunately, only one parse tree is well-formed and type-correct,\n\
                   \but you may still want to disambiguate your grammar or your input."]))
             else (); report_result ctxt pos [] results')
          else
            report_result ctxt pos []
              [(reports', Exn.Exn (ERROR (cat_lines (ambig_msgs @
                (("Ambiguous input\n" ^ string_of_int checked_len ^ " terms are type correct" ^
                  (if checked_len <= limit then ""
                   else " (" ^ string_of_int limit ^ " displayed)") ^ ":") ::
                  map (Pretty.string_of o Pretty.item o single o pretty_term)
                    (take limit checked))))))]
        end handle ERROR msg => parse_failed ctxt pos msg kind)
  end;


(* parse_ast_pattern *)

fun parse_ast_pattern ctxt (root, str) =
  let
    val syn = Proof_Context.syn_of ctxt;

    val reports = Unsynchronized.ref ([]: Position.report_text list);
    fun report ps = Position.store_reports reports ps;

    fun decode_const ps c = (report ps (markup_entity ctxt) c; Ast.Constant c);
    fun decode_var ps x = (report ps (fn () => [Markup.name x Markup.free]) (); Ast.Variable x);
    fun decode_appl ps asts = Ast.Appl (map (decode ps) asts)
    and decode ps (Ast.Constant c) = decode_const ps c
      | decode ps (Ast.Variable x) =
          if is_some (Syntax.lookup_const syn x) orelse Long_Name.is_qualified x
          then decode_const ps x
          else decode_var ps x
      | decode ps (Ast.Appl (asts as (Ast.Constant c :: ast :: Ast.Variable x :: args))) =
          if member (op =) Term_Position.markers c then
            (case Term_Position.decode x of
              SOME p => Ast.mk_appl (decode (p :: ps) ast) (map (decode ps) args)
            | NONE => decode_appl ps asts)
          else decode_appl ps asts
      | decode ps (Ast.Appl asts) = decode_appl ps asts;

    val {text, pos, ...} = Syntax.read_token str;
    val syms = Symbol_Pos.explode (text, pos);
    val ast =
      parse_asts ctxt true root (syms, pos)
      |> uncurry (report_result ctxt pos)
      |> decode [];
    val _ = Context_Position.reports_text ctxt (! reports);
  in ast end;



(** encode parse trees **)

(* term_of_sort *)

fun term_of_sort S =
  let
    val class = Syntax.const o Lexicon.mark_class;

    fun classes [c] = class c
      | classes (c :: cs) = Syntax.const "_classes" $ class c $ classes cs;
  in
    (case S of
      [] => Syntax.const "_topsort"
    | [c] => class c
    | cs => Syntax.const "_sort" $ classes cs)
  end;


(* term_of_typ *)

fun term_of_typ ctxt ty =
  let
    val show_sorts = Config.get ctxt show_sorts orelse Config.get ctxt show_markup;

    fun ofsort t raw_S =
      if show_sorts then
        let val S = #2 (Term_Position.decode_positionS raw_S)
        in if S = dummyS then t else Syntax.const "_ofsort" $ t $ term_of_sort S end
      else t;

    fun term_of (Type (a, Ts)) =
          Term.list_comb (Syntax.const (Lexicon.mark_type a), map term_of Ts)
      | term_of (TFree (x, S)) =
          if is_some (Term_Position.decode x) then Syntax.free x
          else ofsort (Syntax.const "_tfree" $ Syntax.free x) S
      | term_of (TVar (xi, S)) = ofsort (Syntax.const "_tvar" $ Syntax.var xi) S;
  in term_of ty end;


(* simple_ast_of *)

fun simple_ast_of ctxt =
  let
    val tune_var = if Config.get ctxt show_question_marks then I else unprefix "?";
    fun ast_of (Const (c, _)) = Ast.Constant c
      | ast_of (Free (x, _)) = Ast.Variable x
      | ast_of (Var (xi, _)) = Ast.Variable (tune_var (Term.string_of_vname xi))
      | ast_of (t as _ $ _) =
          let val (f, args) = strip_comb t
          in Ast.mk_appl (ast_of f) (map ast_of args) end
      | ast_of (Bound i) = Ast.Appl [Ast.Constant "_loose", Ast.Variable ("B." ^ string_of_int i)]
      | ast_of (Abs _) = raise Fail "simple_ast_of: Abs";
  in ast_of end;


(* sort_to_ast and typ_to_ast *)

fun ast_of_termT ctxt trf tm =
  let
    val ctxt' = Config.put show_sorts false ctxt;
    fun ast_of (t as Const ("_tfree", _) $ Free _) = simple_ast_of ctxt t
      | ast_of (t as Const ("_tvar", _) $ Var _) = simple_ast_of ctxt t
      | ast_of (Const (a, _)) = trans a []
      | ast_of (t as _ $ _) =
          (case strip_comb t of
            (Const (a, _), args) => trans a args
          | (f, args) => Ast.Appl (map ast_of (f :: args)))
      | ast_of t = simple_ast_of ctxt t
    and trans a args = ast_of (trf a ctxt' dummyT args)
      handle Match => Ast.mk_appl (Ast.Constant a) (map ast_of args);
  in ast_of tm end;

fun sort_to_ast ctxt trf S = ast_of_termT ctxt trf (term_of_sort S);
fun typ_to_ast ctxt trf T = ast_of_termT ctxt trf (term_of_typ ctxt T);


(* term_to_ast *)

local

fun mark_aprop tm =
  let
    fun aprop t = Syntax.const "_aprop" $ t;

    fun is_prop Ts t =
      Type_Annotation.clean (Type_Annotation.fastype_of Ts t) = propT
        handle TERM _ => false;

    fun is_term (Const ("Pure.term", _) $ _) = true
      | is_term _ = false;

    fun mark _ (t as Const _) = t
      | mark Ts (t as Const ("_bound", _) $ u) = if is_prop Ts u then aprop t else t
      | mark Ts (t as Free _) = if is_prop Ts t then aprop t else t
      | mark Ts (t as Var _) = if is_prop Ts t then aprop t else t
      | mark Ts (t as Bound _) = if is_prop Ts t then aprop t else t
      | mark Ts (Abs (x, T, t)) = Abs (x, T, mark (T :: Ts) t)
      | mark Ts (t as t1 $ (t2 as Const ("Pure.type", Type ("itself", [T])))) =
          if is_prop Ts t andalso not (is_term t) then Const ("_type_prop", T) $ mark Ts t1
          else mark Ts t1 $ mark Ts t2
      | mark Ts (t as t1 $ t2) =
          (if is_Const (Term.head_of t) orelse not (is_prop Ts t) then I else aprop)
            (mark Ts t1 $ mark Ts t2);
  in mark [] tm end;

fun prune_types ctxt tm =
  let
    fun regard t t' seen =
      if Type_Annotation.is_omitted (Type_Annotation.fastype_of [] t) then (t, seen)
      else if member (op aconv) seen t then (t', seen)
      else (t, t :: seen);

    fun prune (t as Const _, seen) = (t, seen)
      | prune (t as Free (x, T), seen) = regard t (Free (x, Type_Annotation.ignore_type T)) seen
      | prune (t as Var (xi, T), seen) = regard t (Var (xi, Type_Annotation.ignore_type T)) seen
      | prune (t as Bound _, seen) = (t, seen)
      | prune (Abs (x, T, t), seen) =
          let val (t', seen') = prune (t, seen);
          in (Abs (x, T, t'), seen') end
      | prune (t1 $ t2, seen) =
          let
            val (t1', seen') = prune (t1, seen);
            val (t2', seen'') = prune (t2, seen');
          in (t1' $ t2', seen'') end;
  in #1 (prune (tm, [])) end;

fun mark_atoms {structs, fixes} is_syntax_const ctxt tm =
  let
    val show_structs = Config.get ctxt show_structs;

    fun mark ((t as Const (c, _)) $ u) =
          if member (op =) Pure_Thy.token_markers c
          then t $ u else mark t $ mark u
      | mark (t $ u) = mark t $ mark u
      | mark (Abs (x, T, t)) = Abs (x, T, mark t)
      | mark (t as Const (c, T)) =
          if is_syntax_const c then t
          else Const (Lexicon.mark_const c, T)
      | mark (t as Free (x, T)) =
          let val i = find_index (fn s => s = x) structs + 1 in
            if i = 0 andalso member (op =) fixes x then
              Const (Lexicon.mark_fixed x, T)
            else if i = 1 andalso not show_structs then
              Syntax.const "_struct" $ Syntax.const "_indexdefault"
            else Syntax.const "_free" $ t
          end
      | mark (t as Var (xi, T)) =
          if xi = Syntax_Ext.dddot_indexname then Const ("_DDDOT", T)
          else Syntax.const "_var" $ t
      | mark a = a;
  in mark tm end;

in

fun term_to_ast idents is_syntax_const ctxt trf tm =
  let
    val show_types = Config.get ctxt show_types orelse Config.get ctxt show_sorts;
    val show_markup = Config.get ctxt show_markup;

    fun ast_of tm =
      (case strip_comb tm of
        (t as Abs _, ts) => Ast.mk_appl (ast_of (Syntax_Trans.abs_tr' ctxt t)) (map ast_of ts)
      | ((c as Const ("_free", _)), Free (x, T) :: ts) =>
          Ast.mk_appl (constrain (c $ Syntax.free x) T) (map ast_of ts)
      | ((c as Const ("_var", _)), Var (xi, T) :: ts) =>
          Ast.mk_appl (constrain (c $ Syntax.var xi) T) (map ast_of ts)
      | ((c as Const ("_bound", B)), Free (x, T) :: ts) =>
          let
            val X =
              if show_markup andalso not show_types orelse B <> dummyT then T
              else dummyT;
          in Ast.mk_appl (constrain (c $ Syntax.free x) X) (map ast_of ts) end
      | (Const ("_idtdummy", T), ts) =>
          Ast.mk_appl (constrain (Syntax.const "_idtdummy") T) (map ast_of ts)
      | (const as Const (c, T), ts) => trans c (Type_Annotation.smash T) ts
      | (t, ts) => Ast.mk_appl (simple_ast_of ctxt t) (map ast_of ts))

    and trans a T args = ast_of (trf a ctxt T args)
      handle Match => Ast.mk_appl (Ast.Constant a) (map ast_of args)

    and constrain t T0 =
      let
        val T =
          if show_markup andalso not show_types
          then Type_Annotation.clean T0
          else Type_Annotation.smash T0;
      in
        if (show_types orelse show_markup) andalso T <> dummyT then
          Ast.Appl [Ast.Constant "_constrain", simple_ast_of ctxt t,
            ast_of_termT ctxt trf (term_of_typ ctxt T)]
        else simple_ast_of ctxt t
      end;
  in
    tm
    |> mark_aprop
    |> show_types ? prune_types ctxt
    |> Variable.revert_bounds ctxt
    |> mark_atoms idents is_syntax_const ctxt
    |> ast_of
  end;

end;



(** unparse **)

local

fun free_or_skolem ctxt x =
  let
    val m =
      if Variable.is_fixed ctxt x orelse Syntax.is_pretty_global ctxt
      then Markup.fixed x else Markup.intensify;
  in
    if Name.is_skolem x
    then ([m, Markup.skolem], Variable.revert_fixed ctxt x)
    else ([m, Markup.free], x)
  end;

fun var_or_skolem s =
  (case Lexicon.read_variable s of
    SOME (x, i) =>
      (case try Name.dest_skolem x of
        NONE => (Markup.var, s)
      | SOME x' => (Markup.skolem, Term.string_of_vname (x', i)))
  | NONE => (Markup.var, s));

val typing_elem = YXML.output_markup_elem Markup.typing;
val sorting_elem = YXML.output_markup_elem Markup.sorting;

fun unparse_t t_to_ast prt_t markup ctxt t =
  let
    val show_markup = Config.get ctxt show_markup;
    val show_sorts = Config.get ctxt show_sorts;
    val show_types = Config.get ctxt show_types orelse show_sorts;

    val syn = Proof_Context.syn_of ctxt;
    val prtabs = Syntax.prtabs syn;
    val trf = Syntax.print_ast_translation syn;

    fun markup_extern c =
      (case Syntax.lookup_const syn c of
        SOME "" => ([], c)
      | SOME b => markup_extern b
      | NONE => c |> Lexicon.unmark
         {case_class = fn x => (markup_class ctxt x, Proof_Context.extern_class ctxt x),
          case_type = fn x => (markup_type ctxt x, Proof_Context.extern_type ctxt x),
          case_const = fn x => (markup_const ctxt x, Proof_Context.extern_const ctxt x),
          case_fixed = fn x => free_or_skolem ctxt x,
          case_default = fn x => ([], x)});

    fun token_trans "_tfree" x = SOME (Pretty.mark_str (Markup.tfree, x))
      | token_trans "_tvar" x = SOME (Pretty.mark_str (Markup.tvar, x))
      | token_trans "_free" x = SOME (Pretty.marks_str (free_or_skolem ctxt x))
      | token_trans "_bound" x = SOME (Pretty.mark_str (Markup.bound, x))
      | token_trans "_loose" x = SOME (Pretty.mark_str (Markup.bad, x))
      | token_trans "_var" x = SOME (Pretty.mark_str (var_or_skolem x))
      | token_trans "_numeral" x = SOME (Pretty.mark_str (Markup.numeral, x))
      | token_trans "_inner_string" x = SOME (Pretty.mark_str (Markup.inner_string, x))
      | token_trans _ _ = NONE;

    fun markup_trans a [Ast.Variable x] = token_trans a x
      | markup_trans "_constrain" [t, ty] = constrain_trans t ty
      | markup_trans "_idtyp" [t, ty] = constrain_trans t ty
      | markup_trans "_ofsort" [ty, s] = ofsort_trans ty s
      | markup_trans _ _ = NONE

    and constrain_trans t ty =
      if show_markup andalso not show_types then
        let
          val ((bg1, bg2), en) = typing_elem;
          val bg = bg1 ^ Pretty.symbolic_output (pretty_typ_ast Markup.empty ty) ^ bg2;
        in SOME (Pretty.raw_markup (bg, en) (0, [pretty_ast Markup.empty t])) end
      else NONE

    and ofsort_trans ty s =
      if show_markup andalso not show_sorts then
        let
          val ((bg1, bg2), en) = sorting_elem;
          val bg = bg1 ^ Pretty.symbolic_output (pretty_typ_ast Markup.empty s) ^ bg2;
        in SOME (Pretty.raw_markup (bg, en) (0, [pretty_typ_ast Markup.empty ty])) end
      else NONE

    and pretty_typ_ast m ast = ast
      |> Printer.pretty_typ_ast ctxt prtabs trf markup_trans markup_extern
      |> Pretty.markup m

    and pretty_ast m ast = ast
      |> prt_t ctxt prtabs trf markup_trans markup_extern
      |> Pretty.markup m;
  in
    t_to_ast ctxt (Syntax.print_translation syn) t
    |> Ast.normalize ctxt (Syntax.print_rules syn)
    |> pretty_ast markup
  end;

in

val unparse_sort = unparse_t sort_to_ast Printer.pretty_typ_ast (Markup.language_sort false);
val unparse_typ = unparse_t typ_to_ast Printer.pretty_typ_ast (Markup.language_type false);

fun unparse_term ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val syn = Proof_Context.syn_of ctxt;
    val idents = Local_Syntax.idents_of (Proof_Context.syntax_of ctxt);
  in
    unparse_t (term_to_ast idents (is_some o Syntax.lookup_const syn))
      (Printer.pretty_term_ast (not (Pure_Thy.old_appl_syntax thy)))
      (Markup.language_term false) ctxt
  end;

end;



(** translations **)

(* type propositions *)

fun type_prop_tr' ctxt T [Const ("\\<^const>Pure.sort_constraint", _)] =
      Syntax.const "_sort_constraint" $ term_of_typ (Config.put show_sorts true ctxt) T
  | type_prop_tr' ctxt T [t] =
      Syntax.const "_ofclass" $ term_of_typ ctxt T $ t
  | type_prop_tr' _ T ts = raise TYPE ("type_prop_tr'", [T], ts);


(* type reflection *)

fun type_tr' ctxt (Type ("itself", [T])) ts =
      Term.list_comb (Syntax.const "_TYPE" $ term_of_typ ctxt T, ts)
  | type_tr' _ _ _ = raise Match;


(* type constraints *)

fun type_constraint_tr' ctxt (Type ("fun", [T, _])) (t :: ts) =
      Term.list_comb (Syntax.const "_constrain" $ t $ term_of_typ ctxt T, ts)
  | type_constraint_tr' _ _ _ = raise Match;


(* authentic syntax *)

fun const_ast_tr intern ctxt asts =
  (case asts of
    [Ast.Appl [Ast.Constant "_constrain", Ast.Variable c, T as Ast.Variable p]] =>
      let
        val pos = the_default Position.none (Term_Position.decode p);
        val (c', _) = decode_const ctxt (c, [pos]);
        val d = if intern then Lexicon.mark_const c' else c;
      in Ast.Appl [Ast.Constant "_constrain", Ast.Constant d, T] end
  | _ => raise Ast.AST ("const_ast_tr", asts));


(* setup translations *)

val _ = Theory.setup
 (Sign.parse_ast_translation
   [("_context_const", const_ast_tr true),
    ("_context_xconst", const_ast_tr false)] #>
  Sign.typed_print_translation
   [("_type_prop", type_prop_tr'),
    ("\\<^const>Pure.type", type_tr'),
    ("_type_constraint_", type_constraint_tr')]);



(** check/uncheck **)

(* context-sensitive (un)checking *)

type key = int * bool;

structure Checks = Generic_Data
(
  type 'a check = 'a list -> Proof.context -> ('a list * Proof.context) option;
  type T =
    ((key * ((string * typ check) * stamp) list) list *
     (key * ((string * term check) * stamp) list) list);
  val empty = ([], []);
  val extend = I;
  fun merge ((typ_checks1, term_checks1), (typ_checks2, term_checks2)) : T =
    (AList.join (op =) (K (Library.merge (eq_snd (op =)))) (typ_checks1, typ_checks2),
     AList.join (op =) (K (Library.merge (eq_snd (op =)))) (term_checks1, term_checks2));
);

fun print_checks ctxt =
  let
    fun split_checks checks =
      List.partition (fn ((_, un), _) => not un) checks
      |> pairself (map (fn ((i, _), fs) => (i, map (fst o fst) fs))
          #> sort (int_ord o pairself fst));
    fun pretty_checks kind checks =
      checks |> map (fn (i, names) => Pretty.block
        [Pretty.str (kind ^ " (stage " ^ signed_string_of_int i ^ "):"),
          Pretty.brk 1, Pretty.strs names]);

    val (typs, terms) = Checks.get (Context.Proof ctxt);
    val (typ_checks, typ_unchecks) = split_checks typs;
    val (term_checks, term_unchecks) = split_checks terms;
  in
    pretty_checks "typ_checks" typ_checks @
    pretty_checks "term_checks" term_checks @
    pretty_checks "typ_unchecks" typ_unchecks @
    pretty_checks "term_unchecks" term_unchecks
  end |> Pretty.writeln_chunks;


local

fun context_check which (key: key) name f =
  Checks.map (which (AList.map_default op = (key, []) (cons ((name, f), stamp ()))));

fun simple_check eq f xs ctxt =
  let val xs' = f ctxt xs
  in if eq_list eq (xs, xs') then NONE else SOME (xs', ctxt) end;

in

fun typ_check' stage = context_check apfst (stage, false);
fun term_check' stage = context_check apsnd (stage, false);
fun typ_uncheck' stage = context_check apfst (stage, true);
fun term_uncheck' stage = context_check apsnd (stage, true);

fun typ_check key name f = typ_check' key name (simple_check (op =) f);
fun term_check key name f = term_check' key name (simple_check (op aconv) f);
fun typ_uncheck key name f = typ_uncheck' key name (simple_check (op =) f);
fun term_uncheck key name f = term_uncheck' key name (simple_check (op aconv) f);

end;


local

fun check_stage fs = perhaps_loop (perhaps_apply (map uncurry fs));
fun check_all fs = perhaps_apply (map check_stage fs);

fun check which uncheck ctxt0 xs0 =
  let
    val funs = which (Checks.get (Context.Proof ctxt0))
      |> map_filter (fn ((i, u), fs) => if uncheck = u then SOME (i, map (snd o fst) fs) else NONE)
      |> Library.sort (int_ord o pairself fst) |> map snd
      |> not uncheck ? map rev;
  in #1 (perhaps (check_all funs) (xs0, ctxt0)) end;

val apply_typ_check = check fst false;
val apply_term_check = check snd false;
val apply_typ_uncheck = check fst true;
val apply_term_uncheck = check snd true;

in

fun check_typs ctxt raw_tys =
  let
    val (sorting_report, tys) = Proof_Context.prepare_sortsT ctxt raw_tys;
    val _ = if Context_Position.is_visible ctxt then Output.report sorting_report else ();
  in
    tys
    |> apply_typ_check ctxt
    |> Term_Sharing.typs (Proof_Context.theory_of ctxt)
  end;

fun check_terms ctxt raw_ts =
  let
    val (sorting_report, raw_ts') = Proof_Context.prepare_sorts ctxt raw_ts;
    val (ts, ps) = Type_Infer_Context.prepare_positions ctxt raw_ts';

    val tys = map (Logic.mk_type o snd) ps;
    val (ts', tys') = ts @ tys
      |> apply_term_check ctxt
      |> chop (length ts);
    val typing_report =
      fold2 (fn (pos, _) => fn ty =>
        if Position.is_reported pos then
          cons (Position.reported_text pos Markup.typing
            (Syntax.string_of_typ ctxt (Logic.dest_type ty)))
        else I) ps tys' [];

    val _ =
      if Context_Position.is_visible ctxt then Output.report (sorting_report @ typing_report)
      else ();
  in Term_Sharing.terms (Proof_Context.theory_of ctxt) ts' end;

fun check_props ctxt = map (Type.constraint propT) #> check_terms ctxt;

val uncheck_typs = apply_typ_uncheck;
val uncheck_terms = apply_term_uncheck;

end;


(* standard phases *)

val _ = Context.>>
 (typ_check 0 "standard" Proof_Context.standard_typ_check #>
  term_check 0 "standard"
    (fn ctxt => Type_Infer_Context.infer_types ctxt #> map (Proof_Context.expand_abbrevs ctxt)) #>
  term_check 100 "standard_finish" Proof_Context.standard_term_check_finish #>
  term_uncheck 0 "standard" Proof_Context.standard_term_uncheck);



(** install operations **)

val _ = Syntax.install_operations
  {parse_sort = parse_sort,
   parse_typ = parse_typ,
   parse_term = parse_term false,
   parse_prop = parse_term true,
   unparse_sort = unparse_sort,
   unparse_typ = unparse_typ,
   unparse_term = unparse_term,
   check_typs = check_typs,
   check_terms = check_terms,
   check_props = check_props,
   uncheck_typs = uncheck_typs,
   uncheck_terms = uncheck_terms};

end;
