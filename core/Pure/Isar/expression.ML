(*  Title:      Pure/Isar/expression.ML
    Author:     Clemens Ballarin, TU Muenchen

Locale expressions and user interface layer of locales.
*)

signature EXPRESSION =
sig
  (* Locale expressions *)
  datatype 'term map = Positional of 'term option list | Named of (string * 'term) list
  type ('name, 'term) expr = ('name * ((string * bool) * 'term map)) list
  type expression_i = (string, term) expr * (binding * typ option * mixfix) list
  type expression = (xstring * Position.T, string) expr * (binding * string option * mixfix) list

  (* Processing of context statements *)
  val cert_statement: Element.context_i list -> (term * term list) list list ->
    Proof.context -> (term * term list) list list * Proof.context
  val read_statement: Element.context list -> (string * string list) list list ->
    Proof.context -> (term * term list) list list * Proof.context

  (* Declaring locales *)
  val cert_declaration: expression_i -> (Proof.context -> Proof.context) ->
    Element.context_i list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
  val cert_read_declaration: expression_i -> (Proof.context -> Proof.context) ->
    Element.context list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
      (*FIXME*)
  val read_declaration: expression -> (Proof.context -> Proof.context) -> Element.context list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
  val add_locale: binding -> binding ->
    expression_i -> Element.context_i list -> theory -> string * local_theory
  val add_locale_cmd: binding -> binding ->
    expression -> Element.context list -> theory -> string * local_theory

  (* Interpretation *)
  val cert_goal_expression: expression_i -> Proof.context ->
    (term list list * (string * morphism) list * morphism) * Proof.context
  val read_goal_expression: expression -> Proof.context ->
    (term list list * (string * morphism) list * morphism) * Proof.context
  val permanent_interpretation: expression_i -> (Attrib.binding * term) list ->
    local_theory -> Proof.state
  val ephemeral_interpretation: expression_i -> (Attrib.binding * term) list ->
    local_theory -> Proof.state
  val interpret: expression_i -> (Attrib.binding * term) list -> bool -> Proof.state -> Proof.state
  val interpret_cmd: expression -> (Attrib.binding * string) list ->
    bool -> Proof.state -> Proof.state
  val interpretation: expression_i -> (Attrib.binding * term) list -> local_theory -> Proof.state
  val interpretation_cmd: expression -> (Attrib.binding * string) list ->
    local_theory -> Proof.state
  val sublocale: expression_i -> (Attrib.binding * term) list -> local_theory -> Proof.state
  val sublocale_cmd: expression -> (Attrib.binding * string) list -> local_theory -> Proof.state
  val sublocale_global: string -> expression_i ->
    (Attrib.binding * term) list -> theory -> Proof.state
  val sublocale_global_cmd: xstring * Position.T -> expression ->
    (Attrib.binding * string) list -> theory -> Proof.state

  (* Diagnostic *)
  val print_dependencies: Proof.context -> bool -> expression -> unit
end;

structure Expression : EXPRESSION =
struct

datatype ctxt = datatype Element.ctxt;


(*** Expressions ***)

datatype 'term map =
  Positional of 'term option list |
  Named of (string * 'term) list;

type ('name, 'term) expr = ('name * ((string * bool) * 'term map)) list;

type expression_i = (string, term) expr * (binding * typ option * mixfix) list;
type expression = (xstring * Position.T, string) expr * (binding * string option * mixfix) list;


(** Internalise locale names in expr **)

fun check_expr thy instances = map (apfst (Locale.check thy)) instances;


(** Parameters of expression **)

(*Sanity check of instantiations and extraction of implicit parameters.
  The latter only occurs iff strict = false.
  Positional instantiations are extended to match full length of parameter list
  of instantiated locale.*)

fun parameters_of thy strict (expr, fixed) =
  let
    val ctxt = Proof_Context.init_global thy;

    fun reject_dups message xs =
      (case duplicates (op =) xs of
        [] => ()
      | dups => error (message ^ commas dups));

    fun parm_eq ((p1: string, mx1: mixfix), (p2, mx2)) = p1 = p2 andalso
      (mx1 = mx2 orelse error ("Conflicting syntax for parameter " ^ quote p1 ^ " in expression"));

    fun params_loc loc = Locale.params_of thy loc |> map (apfst #1);
    fun params_inst (loc, (prfx, Positional insts)) =
          let
            val ps = params_loc loc;
            val d = length ps - length insts;
            val insts' =
              if d < 0 then
                error ("More arguments than parameters in instantiation of locale " ^
                  quote (Locale.markup_name ctxt loc))
              else insts @ replicate d NONE;
            val ps' = (ps ~~ insts') |>
              map_filter (fn (p, NONE) => SOME p | (_, SOME _) => NONE);
          in (ps', (loc, (prfx, Positional insts'))) end
      | params_inst (loc, (prfx, Named insts)) =
          let
            val _ =
              reject_dups "Duplicate instantiation of the following parameter(s): "
                (map fst insts);
            val ps' = (insts, params_loc loc) |-> fold (fn (p, _) => fn ps =>
              if AList.defined (op =) ps p then AList.delete (op =) p ps
              else error (quote p ^ " not a parameter of instantiated expression"));
          in (ps', (loc, (prfx, Named insts))) end;
    fun params_expr is =
      let
        val (is', ps') = fold_map (fn i => fn ps =>
          let
            val (ps', i') = params_inst i;
            val ps'' = distinct parm_eq (ps @ ps');
          in (i', ps'') end) is []
      in (ps', is') end;

    val (implicit, expr') = params_expr expr;

    val implicit' = map #1 implicit;
    val fixed' = map (Variable.check_name o #1) fixed;
    val _ = reject_dups "Duplicate fixed parameter(s): " fixed';
    val implicit'' =
      if strict then []
      else
        let
          val _ =
            reject_dups
              "Parameter(s) declared simultaneously in expression and for clause: "
              (implicit' @ fixed');
        in map (fn (x, mx) => (Binding.name x, NONE, mx)) implicit end;

  in (expr', implicit'' @ fixed) end;


(** Read instantiation **)

(* Parse positional or named instantiation *)

local

fun prep_inst prep_term ctxt parms (Positional insts) =
      (insts ~~ parms) |> map
        (fn (NONE, p) => Free (p, dummyT)
          | (SOME t, _) => prep_term ctxt t)
  | prep_inst prep_term ctxt parms (Named insts) =
      parms |> map (fn p =>
        (case AList.lookup (op =) insts p of
          SOME t => prep_term ctxt t |
          NONE => Free (p, dummyT)));

in

fun parse_inst x = prep_inst Syntax.parse_term x;
fun make_inst x = prep_inst (K I) x;

end;


(* Instantiation morphism *)

fun inst_morphism (parm_names, parm_types) ((prfx, mandatory), insts') ctxt =
  let
    (* parameters *)
    val type_parm_names = fold Term.add_tfreesT parm_types [] |> map fst;

    (* type inference and contexts *)
    val parm_types' = map (Type_Infer.paramify_vars o Logic.varifyT_global) parm_types;
    val type_parms = fold Term.add_tvarsT parm_types' [] |> map (Logic.mk_type o TVar);
    val arg = type_parms @ map2 Type.constraint parm_types' insts';
    val res = Syntax.check_terms ctxt arg;
    val ctxt' = ctxt |> fold Variable.auto_fixes res;

    (* instantiation *)
    val (type_parms'', res') = chop (length type_parms) res;
    val insts'' = (parm_names ~~ res') |> map_filter
      (fn inst as (x, Free (y, _)) => if x = y then NONE else SOME inst
        | inst => SOME inst);
    val instT = Symtab.make (type_parm_names ~~ map Logic.dest_type type_parms'');
    val inst = Symtab.make insts'';
  in
    (Element.inst_morphism (Proof_Context.theory_of ctxt) (instT, inst) $>
      Morphism.binding_morphism "Expression.inst" (Binding.prefix mandatory prfx), ctxt')
  end;


(*** Locale processing ***)

(** Parsing **)

fun parse_elem prep_typ prep_term ctxt =
  Element.map_ctxt
   {binding = I,
    typ = prep_typ ctxt,
    term = prep_term (Proof_Context.set_mode Proof_Context.mode_schematic ctxt),
    pattern = prep_term (Proof_Context.set_mode Proof_Context.mode_pattern ctxt),
    fact = I,
    attrib = I};

fun parse_concl prep_term ctxt concl =
  (map o map) (fn (t, ps) =>
    (prep_term (Proof_Context.set_mode Proof_Context.mode_schematic ctxt) t,
      map (prep_term (Proof_Context.set_mode Proof_Context.mode_pattern ctxt)) ps)) concl;


(** Simultaneous type inference: instantiations + elements + conclusion **)

local

fun mk_type T = (Logic.mk_type T, []);
fun mk_term t = (t, []);
fun mk_propp (p, pats) = (Type.constraint propT p, pats);

fun dest_type (T, []) = Logic.dest_type T;
fun dest_term (t, []) = t;
fun dest_propp (p, pats) = (p, pats);

fun extract_inst (_, (_, ts)) = map mk_term ts;
fun restore_inst ((l, (p, _)), cs) = (l, (p, map dest_term cs));

fun extract_elem (Fixes fixes) = map (#2 #> the_list #> map mk_type) fixes
  | extract_elem (Constrains csts) = map (#2 #> single #> map mk_type) csts
  | extract_elem (Assumes asms) = map (#2 #> map mk_propp) asms
  | extract_elem (Defines defs) = map (fn (_, (t, ps)) => [mk_propp (t, ps)]) defs
  | extract_elem (Notes _) = [];

fun restore_elem (Fixes fixes, css) =
      (fixes ~~ css) |> map (fn ((x, _, mx), cs) =>
        (x, cs |> map dest_type |> try hd, mx)) |> Fixes
  | restore_elem (Constrains csts, css) =
      (csts ~~ css) |> map (fn ((x, _), cs) =>
        (x, cs |> map dest_type |> hd)) |> Constrains
  | restore_elem (Assumes asms, css) =
      (asms ~~ css) |> map (fn ((b, _), cs) => (b, map dest_propp cs)) |> Assumes
  | restore_elem (Defines defs, css) =
      (defs ~~ css) |> map (fn ((b, _), [c]) => (b, dest_propp c)) |> Defines
  | restore_elem (Notes notes, _) = Notes notes;

fun check cs context =
  let
    fun prep (_, pats) (ctxt, t :: ts) =
      let val ctxt' = Variable.auto_fixes t ctxt
      in
        ((t, Syntax.check_props (Proof_Context.set_mode Proof_Context.mode_pattern ctxt') pats),
          (ctxt', ts))
      end;
    val (cs', (context', _)) = fold_map prep cs
      (context, Syntax.check_terms
        (Proof_Context.set_mode Proof_Context.mode_schematic context) (map fst cs));
  in (cs', context') end;

in

fun check_autofix insts elems concl ctxt =
  let
    val inst_cs = map extract_inst insts;
    val elem_css = map extract_elem elems;
    val concl_cs = (map o map) mk_propp concl;
    (* Type inference *)
    val (inst_cs' :: css', ctxt') =
      (fold_burrow o fold_burrow) check (inst_cs :: elem_css @ [concl_cs]) ctxt;
    val (elem_css', [concl_cs']) = chop (length elem_css) css';
  in
    (map restore_inst (insts ~~ inst_cs'),
      map restore_elem (elems ~~ elem_css'),
      concl_cs', ctxt')
  end;

end;


(** Prepare locale elements **)

fun declare_elem prep_vars (Fixes fixes) ctxt =
      let val (vars, _) = prep_vars fixes ctxt
      in ctxt |> Proof_Context.add_fixes vars |> snd end
  | declare_elem prep_vars (Constrains csts) ctxt =
      ctxt |> prep_vars (map (fn (x, T) => (Binding.name x, SOME T, NoSyn)) csts) |> snd
  | declare_elem _ (Assumes _) ctxt = ctxt
  | declare_elem _ (Defines _) ctxt = ctxt
  | declare_elem _ (Notes _) ctxt = ctxt;


(** Finish locale elements **)

fun finish_inst ctxt (loc, (prfx, inst)) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (parm_names, parm_types) = Locale.params_of thy loc |> map #1 |> split_list;
    val (morph, _) = inst_morphism (parm_names, parm_types) (prfx, inst) ctxt;
  in (loc, morph) end;

fun finish_fixes (parms: (string * typ) list) = map (fn (binding, _, mx) =>
  let val x = Binding.name_of binding
  in (binding, AList.lookup (op =) parms x, mx) end);

local

fun closeup _ _ false elem = elem
  | closeup (outer_ctxt, ctxt) parms true elem =
      let
        (* FIXME consider closing in syntactic phase -- before type checking *)
        fun close_frees t =
          let
            val rev_frees =
              Term.fold_aterms (fn Free (x, T) =>
                if Variable.is_fixed outer_ctxt x orelse AList.defined (op =) parms x then I
                else insert (op =) (x, T) | _ => I) t [];
          in fold (Logic.all o Free) rev_frees t end;

        fun no_binds [] = []
          | no_binds _ = error "Illegal term bindings in context element";
      in
        (case elem of
          Assumes asms => Assumes (asms |> map (fn (a, propps) =>
            (a, map (fn (t, ps) => (close_frees t, no_binds ps)) propps)))
        | Defines defs => Defines (defs |> map (fn ((name, atts), (t, ps)) =>
            let val ((c, _), t') = Local_Defs.cert_def ctxt (close_frees t)
            in ((Thm.def_binding_optional (Binding.name c) name, atts), (t', no_binds ps)) end))
        | e => e)
      end;

in

fun finish_elem _ parms _ (Fixes fixes) = Fixes (finish_fixes parms fixes)
  | finish_elem _ _ _ (Constrains _) = Constrains []
  | finish_elem ctxts parms do_close (Assumes asms) = closeup ctxts parms do_close (Assumes asms)
  | finish_elem ctxts parms do_close (Defines defs) = closeup ctxts parms do_close (Defines defs)
  | finish_elem _ _ _ (Notes facts) = Notes facts;

end;


(** Process full context statement: instantiations + elements + conclusion **)

(* Interleave incremental parsing and type inference over entire parsed stretch. *)

local

fun prep_full_context_statement
    parse_typ parse_prop prep_vars_elem prep_inst prep_vars_inst prep_expr
    {strict, do_close, fixed_frees} raw_import init_body raw_elems raw_concl ctxt1 =
  let
    val thy = Proof_Context.theory_of ctxt1;

    val (raw_insts, fixed) = parameters_of thy strict (apfst (prep_expr thy) raw_import);

    fun prep_insts_cumulative (loc, (prfx, inst)) (i, insts, ctxt) =
      let
        val (parm_names, parm_types) = Locale.params_of thy loc |> map #1 |> split_list;
        val inst' = prep_inst ctxt parm_names inst;
        val parm_types' = parm_types
          |> map (Type_Infer.paramify_vars o
              Term.map_type_tvar (fn ((x, _), S) => TVar ((x, i), S)) o Logic.varifyT_global);
        val inst'' = map2 Type.constraint parm_types' inst';
        val insts' = insts @ [(loc, (prfx, inst''))];
        val (insts'', _, _, _) = check_autofix insts' [] [] ctxt;
        val inst''' = insts'' |> List.last |> snd |> snd;
        val (morph, _) = inst_morphism (parm_names, parm_types) (prfx, inst''') ctxt;
        val ctxt'' = Locale.activate_declarations (loc, morph) ctxt;
      in (i + 1, insts', ctxt'') end;

    fun prep_elem raw_elem ctxt =
      let
        val ctxt' = ctxt
          |> Context_Position.set_visible false
          |> declare_elem prep_vars_elem raw_elem
          |> Context_Position.restore_visible ctxt;
        val elems' = parse_elem parse_typ parse_prop ctxt' raw_elem;
      in (elems', ctxt') end;

    fun prep_concl raw_concl (insts, elems, ctxt) =
      let
        val concl = parse_concl parse_prop ctxt raw_concl;
      in check_autofix insts elems concl ctxt end;

    val fors = prep_vars_inst fixed ctxt1 |> fst;
    val ctxt2 = ctxt1 |> Proof_Context.add_fixes fors |> snd;
    val (_, insts', ctxt3) = fold prep_insts_cumulative raw_insts (0, [], ctxt2);

    val _ =
      if fixed_frees then ()
      else
        (case fold (fold (Variable.add_frees ctxt3) o snd o snd) insts' [] of
          [] => ()
        | frees => error ("Illegal free variables in expression: " ^
            commas_quote (map (Syntax.string_of_term ctxt3 o Free) (rev frees))));

    val ctxt4 = init_body ctxt3;
    val (elems, ctxt5) = fold_map prep_elem raw_elems ctxt4;
    val (insts, elems', concl, ctxt6) = prep_concl raw_concl (insts', elems, ctxt5);

    (* Retrieve parameter types *)
    val xs = maps (fn Fixes fixes => map (Variable.check_name o #1) fixes | _ => [])
      (Fixes fors :: elems');
    val (Ts, ctxt7) = fold_map Proof_Context.inferred_param xs ctxt6;
    val parms = xs ~~ Ts;  (* params from expression and elements *)

    val fors' = finish_fixes parms fors;
    val fixed = map (fn (b, SOME T, mx) => ((Binding.name_of b, T), mx)) fors';
    val deps = map (finish_inst ctxt6) insts;
    val elems'' = map (finish_elem (ctxt1, ctxt6) parms do_close) elems';

  in ((fixed, deps, elems'', concl), (parms, ctxt7)) end;

in

fun cert_full_context_statement x =
  prep_full_context_statement (K I) (K I) Proof_Context.cert_vars
  make_inst Proof_Context.cert_vars (K I) x;

fun cert_read_full_context_statement x =
  prep_full_context_statement Syntax.parse_typ Syntax.parse_prop Proof_Context.read_vars
  make_inst Proof_Context.cert_vars (K I) x;

fun read_full_context_statement x =
  prep_full_context_statement Syntax.parse_typ Syntax.parse_prop Proof_Context.read_vars
  parse_inst Proof_Context.read_vars check_expr x;

end;


(* Context statement: elements + conclusion *)

local

fun prep_statement prep activate raw_elems raw_concl context =
  let
    val ((_, _, elems, concl), _) =
      prep {strict = true, do_close = false, fixed_frees = true}
        ([], []) I raw_elems raw_concl context;
    val (_, context') = context
      |> Proof_Context.set_stmt true
      |> fold_map activate elems;
  in (concl, context') end;

in

fun cert_statement x = prep_statement cert_full_context_statement Element.activate_i x;
fun read_statement x = prep_statement read_full_context_statement Element.activate x;

end;


(* Locale declaration: import + elements *)

fun fix_params params =
  Proof_Context.add_fixes (map (fn ((x, T), mx) => (Binding.name x, SOME T, mx)) params) #> snd;

local

fun prep_declaration prep activate raw_import init_body raw_elems context =
  let
    val ((fixed, deps, elems, _), (parms, ctxt')) =
      prep {strict = false, do_close = true, fixed_frees = false}
        raw_import init_body raw_elems [] context;
    (* Declare parameters and imported facts *)
    val context' = context |>
      fix_params fixed |>
      fold (Context.proof_map o Locale.activate_facts NONE) deps;
    val (elems', context'') = context' |>
      Proof_Context.set_stmt true |>
      fold_map activate elems;
  in ((fixed, deps, elems', context''), (parms, ctxt')) end;

in

fun cert_declaration x = prep_declaration cert_full_context_statement Element.activate_i x;
fun cert_read_declaration x = prep_declaration cert_read_full_context_statement Element.activate x;
fun read_declaration x = prep_declaration read_full_context_statement Element.activate x;

end;


(* Locale expression to set up a goal *)

local

fun props_of thy (name, morph) =
  let
    val (asm, defs) = Locale.specification_of thy name;
  in
    (case asm of NONE => defs | SOME asm => asm :: defs)
    |> map (Morphism.term morph)
  end;

fun prep_goal_expression prep expression context =
  let
    val thy = Proof_Context.theory_of context;

    val ((fixed, deps, _, _), _) =
      prep {strict = true, do_close = true, fixed_frees = true} expression I [] [] context;
    (* proof obligations *)
    val propss = map (props_of thy) deps;

    val goal_ctxt = context |>
      fix_params fixed |>
      (fold o fold) Variable.auto_fixes propss;

    val export = Variable.export_morphism goal_ctxt context;
    val exp_fact = Drule.zero_var_indexes_list o map Thm.strip_shyps o Morphism.fact export;
    val exp_term = Term_Subst.zero_var_indexes o Morphism.term export;
    val exp_typ = Logic.type_map exp_term;
    val export' =
      Morphism.morphism "Expression.prep_goal"
        {binding = [], typ = [exp_typ], term = [exp_term], fact = [exp_fact]};
  in ((propss, deps, export'), goal_ctxt) end;

in

fun cert_goal_expression x = prep_goal_expression cert_full_context_statement x;
fun read_goal_expression x = prep_goal_expression read_full_context_statement x;

end;


(*** Locale declarations ***)

(* extract specification text *)

val norm_term = Envir.beta_norm oo Term.subst_atomic;

fun bind_def ctxt eq (xs, env, eqs) =
  let
    val _ = Local_Defs.cert_def ctxt eq;
    val ((y, T), b) = Local_Defs.abs_def eq;
    val b' = norm_term env b;
    fun err msg = error (msg ^ ": " ^ quote y);
  in
    (case filter (fn (Free (y', _), _) => y = y' | _ => false) env of
      [] => (Term.add_frees b' xs, (Free (y, T), b') :: env, eq :: eqs)
    | dups =>
        if forall (fn (_, b'') => b' aconv b'') dups then (xs, env, eqs)
        else err "Attempt to redefine variable")
  end;

(* text has the following structure:
       (((exts, exts'), (ints, ints')), (xs, env, defs))
   where
     exts: external assumptions (terms in assumes elements)
     exts': dito, normalised wrt. env
     ints: internal assumptions (terms in assumptions from insts)
     ints': dito, normalised wrt. env
     xs: the free variables in exts' and ints' and rhss of definitions,
       this includes parameters except defined parameters
     env: list of term pairs encoding substitutions, where the first term
       is a free variable; substitutions represent defines elements and
       the rhs is normalised wrt. the previous env
     defs: the equations from the defines elements
   *)

fun eval_text _ _ (Fixes _) text = text
  | eval_text _ _ (Constrains _) text = text
  | eval_text _ is_ext (Assumes asms)
        (((exts, exts'), (ints, ints')), (xs, env, defs)) =
      let
        val ts = maps (map #1 o #2) asms;
        val ts' = map (norm_term env) ts;
        val spec' =
          if is_ext then ((exts @ ts, exts' @ ts'), (ints, ints'))
          else ((exts, exts'), (ints @ ts, ints' @ ts'));
      in (spec', (fold Term.add_frees ts' xs, env, defs)) end
  | eval_text ctxt _ (Defines defs) (spec, binds) =
      (spec, fold (bind_def ctxt o #1 o #2) defs binds)
  | eval_text _ _ (Notes _) text = text;

fun eval_inst ctxt (loc, morph) text =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (asm, defs) = Locale.specification_of thy loc;
    val asm' = Option.map (Morphism.term morph) asm;
    val defs' = map (Morphism.term morph) defs;
    val text' =
      text |>
       (if is_some asm then
          eval_text ctxt false (Assumes [(Attrib.empty_binding, [(the asm', [])])])
        else I) |>
       (if not (null defs) then
          eval_text ctxt false (Defines (map (fn def => (Attrib.empty_binding, (def, []))) defs'))
        else I)
(* FIXME clone from locale.ML *)
  in text' end;

fun eval_elem ctxt elem text =
  eval_text ctxt true elem text;

fun eval ctxt deps elems =
  let
    val text' = fold (eval_inst ctxt) deps ((([], []), ([], [])), ([], [], []));
    val ((spec, (_, _, defs))) = fold (eval_elem ctxt) elems text';
  in (spec, defs) end;

(* axiomsN: name of theorem set with destruct rules for locale predicates,
     also name suffix of delta predicates and assumptions. *)

val axiomsN = "axioms";

local

(* introN: name of theorems for introduction rules of locale and
     delta predicates *)

val introN = "intro";

fun atomize_spec thy ts =
  let
    val t = Logic.mk_conjunction_balanced ts;
    val body = Object_Logic.atomize_term thy t;
    val bodyT = Term.fastype_of body;
  in
    if bodyT = propT
    then (t, propT, Thm.reflexive (Thm.cterm_of thy t))
    else (body, bodyT, Object_Logic.atomize (Proof_Context.init_global thy) (Thm.cterm_of thy t))
  end;

(* achieve plain syntax for locale predicates (without "PROP") *)

fun aprop_tr' n c =
  let
    val c' = Lexicon.mark_const c;
    fun tr' (_: Proof.context) T args =
      if T <> dummyT andalso length args = n
      then Syntax.const "_aprop" $ Term.list_comb (Syntax.const c', args)
      else raise Match;
  in (c', tr') end;

(* define one predicate including its intro rule and axioms
   - binding: predicate name
   - parms: locale parameters
   - defs: thms representing substitutions from defines elements
   - ts: terms representing locale assumptions (not normalised wrt. defs)
   - norm_ts: terms representing locale assumptions (normalised wrt. defs)
   - thy: the theory
*)

fun def_pred binding parms defs ts norm_ts thy =
  let
    val name = Sign.full_name thy binding;

    val (body, bodyT, body_eq) = atomize_spec thy norm_ts;
    val env = Term.add_free_names body [];
    val xs = filter (member (op =) env o #1) parms;
    val Ts = map #2 xs;
    val extraTs =
      (subtract (op =) (fold Term.add_tfreesT Ts []) (Term.add_tfrees body []))
      |> Library.sort_wrt #1 |> map TFree;
    val predT = map Term.itselfT extraTs ---> Ts ---> bodyT;

    val args = map Logic.mk_type extraTs @ map Free xs;
    val head = Term.list_comb (Const (name, predT), args);
    val statement = Object_Logic.ensure_propT thy head;

    val ([pred_def], defs_thy) =
      thy
      |> bodyT = propT ? Sign.typed_print_translation [aprop_tr' (length args) name]
      |> Sign.declare_const_global ((Binding.conceal binding, predT), NoSyn) |> snd
      |> Global_Theory.add_defs false
        [((Binding.conceal (Thm.def_binding binding), Logic.mk_equals (head, body)), [])];
    val defs_ctxt = Proof_Context.init_global defs_thy |> Variable.declare_term head;

    val cert = Thm.cterm_of defs_thy;

    val intro = Goal.prove_global defs_thy [] norm_ts statement
      (fn {context = ctxt, ...} =>
        rewrite_goals_tac ctxt [pred_def] THEN
        compose_tac (false, body_eq RS Drule.equal_elim_rule1, 1) 1 THEN
        compose_tac (false, Conjunction.intr_balanced (map (Thm.assume o cert) norm_ts), 0) 1);

    val conjuncts =
      (Drule.equal_elim_rule2 OF
        [body_eq, rewrite_rule defs_ctxt [pred_def] (Thm.assume (cert statement))])
      |> Conjunction.elim_balanced (length ts);

    val (_, axioms_ctxt) = defs_ctxt
      |> Assumption.add_assumes (maps (#hyps o Thm.crep_thm) (defs @ conjuncts));
    val axioms = ts ~~ conjuncts |> map (fn (t, ax) =>
      Element.prove_witness axioms_ctxt t
       (rewrite_goals_tac axioms_ctxt defs THEN compose_tac (false, ax, 0) 1));
  in ((statement, intro, axioms), defs_thy) end;

in

(* main predicate definition function *)

fun define_preds binding parms (((exts, exts'), (ints, ints')), defs) thy =
  let
    val ctxt = Proof_Context.init_global thy;
    val defs' = map (cterm_of thy #> Assumption.assume ctxt #> Drule.abs_def) defs;

    val (a_pred, a_intro, a_axioms, thy'') =
      if null exts then (NONE, NONE, [], thy)
      else
        let
          val abinding =
            if null ints then binding else Binding.suffix_name ("_" ^ axiomsN) binding;
          val ((statement, intro, axioms), thy') =
            thy
            |> def_pred abinding parms defs' exts exts';
          val (_, thy'') =
            thy'
            |> Sign.qualified_path true abinding
            |> Global_Theory.note_thmss ""
              [((Binding.conceal (Binding.name introN), []), [([intro], [Locale.unfold_add])])]
            ||> Sign.restore_naming thy';
          in (SOME statement, SOME intro, axioms, thy'') end;
    val (b_pred, b_intro, b_axioms, thy'''') =
      if null ints then (NONE, NONE, [], thy'')
      else
        let
          val ((statement, intro, axioms), thy''') =
            thy''
            |> def_pred binding parms defs' (ints @ the_list a_pred) (ints' @ the_list a_pred);
          val ctxt''' = Proof_Context.init_global thy''';
          val (_, thy'''') =
            thy'''
            |> Sign.qualified_path true binding
            |> Global_Theory.note_thmss ""
                 [((Binding.conceal (Binding.name introN), []), [([intro], [Locale.intro_add])]),
                  ((Binding.conceal (Binding.name axiomsN), []),
                    [(map (Drule.export_without_context o Element.conclude_witness ctxt''') axioms,
                      [])])]
            ||> Sign.restore_naming thy''';
        in (SOME statement, SOME intro, axioms, thy'''') end;
  in ((a_pred, a_intro, a_axioms), (b_pred, b_intro, b_axioms), thy'''') end;

end;


local

fun assumes_to_notes (Assumes asms) axms =
      fold_map (fn (a, spec) => fn axs =>
          let val (ps, qs) = chop (length spec) axs
          in ((a, [(ps, [])]), qs) end) asms axms
      |> apfst (curry Notes "")
  | assumes_to_notes e axms = (e, axms);

fun defines_to_notes ctxt (Defines defs) =
      Notes ("", map (fn (a, (def, _)) =>
        (a, [([Assumption.assume ctxt (cterm_of (Proof_Context.theory_of ctxt) def)],
          [(Attrib.internal o K) Locale.witness_add])])) defs)
  | defines_to_notes _ e = e;

fun gen_add_locale prep_decl
    binding raw_predicate_binding raw_import raw_body thy =
  let
    val name = Sign.full_name thy binding;
    val _ = Locale.defined thy name andalso
      error ("Duplicate definition of locale " ^ quote name);

    val ((fixed, deps, body_elems, _), (parms, ctxt')) =
      prep_decl raw_import I raw_body (Proof_Context.init_global thy);
    val text as (((_, exts'), _), defs) = eval ctxt' deps body_elems;

    val extraTs =
      subtract (op =)
        (fold Term.add_tfreesT (map snd parms) [])
        (fold Term.add_tfrees exts' []);
    val _ =
      if null extraTs then ()
      else warning ("Additional type variable(s) in locale specification " ^
          Binding.print binding ^ ": " ^
          commas (map (Syntax.string_of_typ ctxt' o TFree) (sort_wrt #1 extraTs)));

    val predicate_binding =
      if Binding.is_empty raw_predicate_binding then binding
      else raw_predicate_binding;
    val ((a_statement, a_intro, a_axioms), (b_statement, b_intro, b_axioms), thy') =
      define_preds predicate_binding parms text thy;
    val pred_ctxt = Proof_Context.init_global thy';

    val a_satisfy = Element.satisfy_morphism a_axioms;
    val b_satisfy = Element.satisfy_morphism b_axioms;

    val params = fixed @
      maps (fn Fixes fixes =>
        map (fn (b, SOME T, mx) => ((Binding.name_of b, T), mx)) fixes | _ => []) body_elems;
    val asm = if is_some b_statement then b_statement else a_statement;

    val notes =
      if is_some asm then
        [("", [((Binding.conceal (Binding.suffix_name ("_" ^ axiomsN) binding), []),
          [([Assumption.assume pred_ctxt (cterm_of thy' (the asm))],
            [(Attrib.internal o K) Locale.witness_add])])])]
      else [];

    val notes' =
      body_elems
      |> map (defines_to_notes pred_ctxt)
      |> map (Element.transform_ctxt a_satisfy)
      |> (fn elems =>
        fold_map assumes_to_notes elems (map (Element.conclude_witness pred_ctxt) a_axioms))
      |> fst
      |> map (Element.transform_ctxt b_satisfy)
      |> map_filter (fn Notes notes => SOME notes | _ => NONE);

    val deps' = map (fn (l, morph) => (l, morph $> b_satisfy)) deps;
    val axioms = map (Element.conclude_witness pred_ctxt) b_axioms;

    val loc_ctxt = thy'
      |> Locale.register_locale binding (extraTs, params)
          (asm, rev defs) (a_intro, b_intro) axioms [] (rev notes) (rev deps')
      |> Named_Target.init name
      |> fold (fn (kind, facts) => Local_Theory.notes_kind kind facts #> snd) notes';

  in (name, loc_ctxt) end;

in

val add_locale = gen_add_locale cert_declaration;
val add_locale_cmd = gen_add_locale read_declaration;

end;


(*** Interpretation ***)

local

(* reading *)

fun prep_with_extended_syntax prep_prop deps ctxt props =
  let
    val deps_ctxt = fold Locale.activate_declarations deps ctxt;
  in
    map (prep_prop deps_ctxt o snd) props |> Syntax.check_terms deps_ctxt
      |> Variable.export_terms deps_ctxt ctxt
  end;

fun prep_interpretation prep_expr prep_prop prep_attr expression raw_eqns initial_ctxt =
  let
    val ((propss, deps, export), expr_ctxt) = prep_expr expression initial_ctxt;
    val eqns = prep_with_extended_syntax prep_prop deps expr_ctxt raw_eqns;
    val attrss = map (apsnd (map (prep_attr initial_ctxt)) o fst) raw_eqns;
    val goal_ctxt = fold Variable.auto_fixes eqns expr_ctxt;
    val export' = Variable.export_morphism goal_ctxt expr_ctxt;
  in (((propss, deps, export, export'), (eqns, attrss)), goal_ctxt) end;

val cert_interpretation =
  prep_interpretation cert_goal_expression (K I) (K I);

val read_interpretation =
  prep_interpretation read_goal_expression Syntax.parse_prop Attrib.check_src;


(* generic interpretation machinery *)

fun meta_rewrite ctxt eqns =
  map (Local_Defs.meta_rewrite_rule ctxt #> Drule.abs_def) (maps snd eqns);

fun note_eqns_register note activate deps witss eqns attrss export export' ctxt =
  let
    val facts = map2 (fn attrs => fn eqn =>
      (attrs, [([Morphism.thm (export' $> export) eqn], [])])) attrss eqns;
    val (eqns', ctxt') = ctxt
      |> note Thm.lemmaK facts
      |> (fn (eqns, ctxt') => (meta_rewrite ctxt' eqns, ctxt'));
    val dep_morphs =
      map2 (fn (dep, morph) => fn wits =>
          (dep, morph $> Element.satisfy_morphism (map (Element.transform_witness export') wits)))
        deps witss;
    fun activate' dep_morph ctxt =
      activate dep_morph
        (Option.map (rpair true) (Element.eq_morphism (Proof_Context.theory_of ctxt) eqns'))
        export ctxt;
  in
    ctxt'
    |> fold activate' dep_morphs
  end;

fun generic_interpretation prep_interpretation setup_proof note activate
    expression raw_eqns initial_ctxt =
  let
    val (((propss, deps, export, export'), (eqns, attrss)), goal_ctxt) =
      prep_interpretation expression raw_eqns initial_ctxt;
    fun after_qed witss eqns =
      note_eqns_register note activate deps witss eqns attrss export export';
  in setup_proof after_qed propss eqns goal_ctxt end;


(* first dimension: proof vs. local theory *)

fun gen_interpret prep_interpretation expression raw_eqns int state =
  let
    val _ = Proof.assert_forward_or_chain state;
    val ctxt = Proof.context_of state;
    fun lift_after_qed after_qed witss eqns =
      Proof.map_context (after_qed witss eqns) #> Proof.reset_facts;
    fun setup_proof after_qed propss eqns goal_ctxt =
      Element.witness_local_proof_eqs (lift_after_qed after_qed) "interpret"
        propss eqns goal_ctxt int state;
  in
    generic_interpretation prep_interpretation setup_proof
      Attrib.local_notes (Context.proof_map ooo Locale.add_registration) expression raw_eqns ctxt
  end;

fun gen_local_theory_interpretation prep_interpretation activate expression raw_eqns lthy =
  generic_interpretation prep_interpretation Element.witness_proof_eqs
    Local_Theory.notes_kind (activate lthy) expression raw_eqns lthy;


(* second dimension: relation to underlying target *)

fun subscribe_or_activate lthy =
  if Named_Target.is_theory lthy
  then Local_Theory.subscription
  else Local_Theory.activate;

fun subscribe_locale_only lthy =
  let
    val _ =
      if Named_Target.is_theory lthy
      then error "Not possible on level of global theory"
      else ();
  in Local_Theory.subscription end;


(* special case: global sublocale command *)

fun gen_sublocale_global prep_loc prep_interpretation
    raw_locale expression raw_eqns thy =
  let
    val lthy = Named_Target.init (prep_loc thy raw_locale) thy;
    fun setup_proof after_qed =
      Element.witness_proof_eqs
        (fn wits => fn eqs => after_qed wits eqs #> Local_Theory.exit);
  in
    lthy |>
      generic_interpretation prep_interpretation setup_proof
        Local_Theory.notes_kind (subscribe_locale_only lthy) expression raw_eqns
  end;

in


(* interfaces *)

fun interpret x = gen_interpret cert_interpretation x;
fun interpret_cmd x = gen_interpret read_interpretation x;

fun permanent_interpretation expression raw_eqns =
  Local_Theory.assert_bottom true
  #> gen_local_theory_interpretation cert_interpretation
    (K Local_Theory.subscription) expression raw_eqns;

fun ephemeral_interpretation x =
  gen_local_theory_interpretation cert_interpretation (K Local_Theory.activate) x;

fun interpretation x =
  gen_local_theory_interpretation cert_interpretation subscribe_or_activate x;
fun interpretation_cmd x =
  gen_local_theory_interpretation read_interpretation subscribe_or_activate x;

fun sublocale x =
  gen_local_theory_interpretation cert_interpretation subscribe_locale_only x;
fun sublocale_cmd x =
  gen_local_theory_interpretation read_interpretation subscribe_locale_only x;

fun sublocale_global x = gen_sublocale_global (K I) cert_interpretation x;
fun sublocale_global_cmd x = gen_sublocale_global Locale.check read_interpretation x;

end;


(** Print the instances that would be activated by an interpretation
  of the expression in the current context (clean = false) or in an
  empty context (clean = true). **)

fun print_dependencies ctxt clean expression =
  let
    val ((_, deps, export), expr_ctxt) = read_goal_expression expression ctxt;
    val export' = if clean then Morphism.identity else export;
  in
    Locale.print_dependencies expr_ctxt clean export' deps
  end;

end;
