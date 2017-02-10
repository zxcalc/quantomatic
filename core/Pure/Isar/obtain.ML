(*  Title:      Pure/Isar/obtain.ML
    Author:     Markus Wenzel, TU Muenchen

Generalized existence and cases rules within Isar proof text.
*)

signature OBTAIN =
sig
  val obtain_thesis: Proof.context -> ((string * typ) * term) * Proof.context
  val obtains_attributes: ('typ, 'term) Element.obtain list -> attribute list
  val obtains_attribs: ('typ, 'term) Element.obtain list -> Token.src list
  val read_obtains: Proof.context -> term -> Element.obtains -> (binding * term) list
  val cert_obtains: Proof.context -> term -> Element.obtains_i -> (binding * term) list
  val parse_obtains: Proof.context -> term -> Element.obtains -> (binding * term) list
  val consider: Element.obtains_i -> bool -> Proof.state -> Proof.state
  val consider_cmd: Element.obtains -> bool -> Proof.state -> Proof.state
  val obtain: binding -> (binding * typ option * mixfix) list ->
    (binding * typ option * mixfix) list -> (term * term list) list list ->
    (Thm.binding * (term * term list) list) list -> bool -> Proof.state -> Proof.state
  val obtain_cmd: binding -> (binding * string option * mixfix) list ->
    (binding * string option * mixfix) list -> (string * string list) list list ->
    (Attrib.binding * (string * string list) list) list -> bool -> Proof.state -> Proof.state
  val result: (Proof.context -> tactic) -> thm list -> Proof.context ->
    ((string * cterm) list * thm list) * Proof.context
  val guess: (binding * typ option * mixfix) list -> bool -> Proof.state -> Proof.state
  val guess_cmd: (binding * string option * mixfix) list -> bool -> Proof.state -> Proof.state
end;

structure Obtain: OBTAIN =
struct

(** specification elements **)

(* obtain_export *)

(*
  [x, A x]
     :
     B
  --------
     B
*)
fun eliminate_term ctxt xs tm =
  let
    val vs = map (dest_Free o Thm.term_of) xs;
    val bads = Term.fold_aterms (fn t as Free v =>
      if member (op =) vs v then insert (op aconv) t else I | _ => I) tm [];
    val _ = null bads orelse
      error ("Result contains obtained parameters: " ^
        space_implode " " (map (Syntax.string_of_term ctxt) bads));
  in tm end;

fun eliminate ctxt rule xs As thm =
  let
    val _ = eliminate_term ctxt xs (Thm.full_prop_of thm);
    val _ = Object_Logic.is_judgment ctxt (Thm.concl_of thm) orelse
      error "Conclusion in obtained context must be object-logic judgment";

    val ((_, [thm']), ctxt') = Variable.import true [thm] ctxt;
    val prems = Drule.strip_imp_prems (Thm.cprop_of thm');
  in
    ((Drule.implies_elim_list thm' (map Thm.assume prems)
        |> Drule.implies_intr_list (map (Drule.norm_hhf_cterm ctxt') As)
        |> Drule.forall_intr_list xs)
      COMP rule)
    |> Drule.implies_intr_list prems
    |> singleton (Variable.export ctxt' ctxt)
  end;

fun obtain_export ctxt rule xs _ As =
  (eliminate ctxt rule xs As, eliminate_term ctxt xs);


(* result declaration *)

fun case_names (obtains: ('typ, 'term) Element.obtain list) =
  obtains |> map_index (fn (i, (b, _)) =>
    if Binding.is_empty b then string_of_int (i + 1) else Name_Space.base_name b);

fun obtains_attributes obtains =
  [Rule_Cases.consumes (~ (length obtains)), Rule_Cases.case_names (case_names obtains)];

fun obtains_attribs obtains =
  [Attrib.consumes (~ (length obtains)), Attrib.case_names (case_names obtains)];


(* obtain thesis *)

fun obtain_thesis ctxt =
  let
    val ([x], ctxt') =
      Proof_Context.add_fixes [(Binding.name Auto_Bind.thesisN, NONE, NoSyn)] ctxt;
    val t = Object_Logic.fixed_judgment ctxt x;
    val v = dest_Free (Object_Logic.drop_judgment ctxt t);
  in ((v, t), ctxt') end;


(* obtain clauses *)

local

val mk_all_external = Logic.all_constraint o Variable.default_type;

fun mk_all_internal ctxt (y, z) t =
  let
    val T =
      (case AList.lookup (op =) (Term.add_frees t []) z of
        SOME T => T
      | NONE => the_default dummyT (Variable.default_type ctxt z));
  in Logic.all_const T $ Term.lambda_name (y, Free (z, T)) t end;

fun prepare_clause prep_var parse_prop mk_all ctxt thesis raw_vars raw_props =
  let
    val ((xs', vars), ctxt') = ctxt
      |> fold_map prep_var raw_vars
      |-> (fn vars => Proof_Context.add_fixes vars ##>> pair vars);
    val xs = map (Variable.check_name o #1) vars;
  in
    Logic.list_implies (map (parse_prop ctxt') raw_props, thesis)
    |> fold_rev (mk_all ctxt') (xs ~~ xs')
  end;

fun prepare_obtains prep_clause check_terms
    ctxt thesis (raw_obtains: ('typ, 'term) Element.obtain list) =
  let
    val clauses = raw_obtains
      |> map (fn (_, (raw_vars, raw_props)) => prep_clause ctxt thesis raw_vars raw_props)
      |> check_terms ctxt;
  in map fst raw_obtains ~~ clauses end;

val parse_clause = prepare_clause Proof_Context.read_var Syntax.parse_prop mk_all_external;
val cert_clause = prepare_clause Proof_Context.cert_var (K I) mk_all_internal;

in

val read_obtains = prepare_obtains parse_clause Syntax.check_terms;
val cert_obtains = prepare_obtains cert_clause (K I);
val parse_obtains = prepare_obtains parse_clause (K I);

end;



(** consider: generalized elimination and cases rule **)

(*
  consider (a) x where "A x" | (b) y where "B y" | ... ==

  have thesis
    if a [intro?]: "!!x. A x ==> thesis"
    and b [intro?]: "!!y. B y ==> thesis"
    and ...
    for thesis
    apply (insert that)
*)

local

fun gen_consider prep_obtains raw_obtains int state =
  let
    val _ = Proof.assert_forward_or_chain state;
    val ctxt = Proof.context_of state;

    val ((_, thesis), thesis_ctxt) = obtain_thesis ctxt;
    val obtains = prep_obtains thesis_ctxt thesis raw_obtains;
    val atts = Rule_Cases.cases_open :: obtains_attributes raw_obtains;
  in
    state
    |> Proof.have true NONE (K I)
      [(Binding.name Auto_Bind.thesisN, NONE, NoSyn)]
      (map (fn (a, A) => ((a, [Context_Rules.intro_query NONE]), [(A, [])])) obtains)
      [((Binding.empty, atts), [(thesis, [])])] int
    |-> Proof.refine_insert
  end;

in

val consider = gen_consider cert_obtains;
val consider_cmd = gen_consider read_obtains;

end;



(** obtain: augmented context based on generalized existence rule **)

(*
  obtain (a) x where "A x" <proof> ==

  have thesis if a [intro?]: "!!x. A x ==> thesis" for thesis
    apply (insert that)
    <proof>
  fix x assm <<obtain_export>> "A x"
*)

local

fun gen_obtain prep_stmt prep_att that_binding raw_decls raw_fixes raw_prems raw_concls int state =
  let
    val _ = Proof.assert_forward_or_chain state;

    val ((_, thesis), thesis_ctxt) = obtain_thesis (Proof.context_of state);

    val ((vars, propss, binds, binds'), params_ctxt) =
      prep_stmt (raw_decls @ raw_fixes) (raw_prems @ map #2 raw_concls) thesis_ctxt;
    val (decls, fixes) = chop (length raw_decls) vars ||> map #2;
    val (premss, conclss) = chop (length raw_prems) propss;
    val propss' = (map o map) (Logic.close_prop fixes (flat premss)) conclss;

    val that_prop =
      Logic.list_rename_params (map (#1 o #2) decls)
        (fold_rev (Logic.all o #2 o #2) decls (Logic.list_implies (flat propss', thesis)));

    val cparams = map (Thm.cterm_of params_ctxt o #2 o #2) decls;
    val asms =
      map (fn ((b, raw_atts), _) => (b, map (prep_att params_ctxt) raw_atts)) raw_concls ~~
      map (map (rpair [])) propss';

    fun after_qed (result_ctxt, results) state' =
      let val [rule] = Proof_Context.export result_ctxt (Proof.context_of state') (flat results) in
        state'
        |> Proof.fix (map #1 decls)
        |> Proof.map_context (fold (Variable.bind_term o apsnd (Logic.close_term fixes)) binds)
        |> Proof.assm (obtain_export params_ctxt rule cparams) [] [] asms
      end;
  in
    state
    |> Proof.have true NONE after_qed
      [(Binding.name Auto_Bind.thesisN, NONE, NoSyn)]
      [((that_binding, [Context_Rules.intro_query NONE]), [(that_prop, [])])]
      [(Binding.empty_atts, [(thesis, [])])] int
    |-> Proof.refine_insert
    |> Proof.map_context (fold Variable.bind_term binds')
  end;

in

val obtain = gen_obtain Proof_Context.cert_stmt (K I);
val obtain_cmd = gen_obtain Proof_Context.read_stmt Attrib.attribute_cmd;

end;



(** tactical result **)

fun check_result ctxt thesis th =
  (case Thm.prems_of th of
    [prem] =>
      if Thm.concl_of th aconv thesis andalso
        Logic.strip_assums_concl prem aconv thesis then th
      else error ("Guessed a different clause:\n" ^ Thm.string_of_thm ctxt th)
  | [] => error "Goal solved -- nothing guessed"
  | _ => error ("Guess split into several cases:\n" ^ Thm.string_of_thm ctxt th));

fun result tac facts ctxt =
  let
    val ((thesis_var, thesis), thesis_ctxt) = obtain_thesis ctxt;
    val st = Goal.init (Thm.cterm_of ctxt thesis);
    val rule =
      (case SINGLE (Method.insert_tac thesis_ctxt facts 1 THEN tac thesis_ctxt) st of
        NONE => raise THM ("Obtain.result: tactic failed", 0, facts)
      | SOME th =>
          check_result thesis_ctxt thesis (Raw_Simplifier.norm_hhf thesis_ctxt (Goal.conclude th)));

    val closed_rule = Thm.forall_intr (Thm.cterm_of ctxt (Free thesis_var)) rule;
    val ((_, [rule']), ctxt') = Variable.import false [closed_rule] ctxt;
    val obtain_rule =
      Thm.forall_elim (Thm.cterm_of ctxt (Logic.varify_global (Free thesis_var))) rule';
    val ((params, stmt), fix_ctxt) = Variable.focus_cterm NONE (Thm.cprem_of obtain_rule 1) ctxt';
    val (prems, ctxt'') =
      Assumption.add_assms (obtain_export fix_ctxt obtain_rule (map #2 params))
        (Drule.strip_imp_prems stmt) fix_ctxt;
  in ((params, prems), ctxt'') end;



(** guess: obtain based on tactical result **)

(*
  <chain_facts>
  guess x <proof body> <proof end> ==

  {
    fix thesis
    <chain_facts> have "PROP ?guess"
      apply magic      -- {* turn goal into "thesis ==> #thesis" *}
      <proof body>
      apply_end magic  -- {* turn final "(!!x. P x ==> thesis) ==> #thesis" into
        "#((!!x. A x ==> thesis) ==> thesis)" which is a finished goal state *}
      <proof end>
  }
  fix x assm <<obtain_export>> "A x"
*)

local

fun unify_params vars thesis_var raw_rule ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val string_of_term = Syntax.string_of_term (Config.put show_types true ctxt);

    fun err msg th = error (msg ^ ":\n" ^ Thm.string_of_thm ctxt th);

    val maxidx = fold (Term.maxidx_typ o snd o fst) vars ~1;
    val rule = Thm.incr_indexes (maxidx + 1) raw_rule;

    val params = Rule_Cases.strip_params (Logic.nth_prem (1, Thm.prop_of rule));
    val m = length vars;
    val n = length params;
    val _ = m <= n orelse err "More variables than parameters in obtained rule" rule;

    fun unify ((x, T), (y, U)) (tyenv, max) = Sign.typ_unify thy (T, U) (tyenv, max)
      handle Type.TUNIFY =>
        err ("Failed to unify variable " ^
          string_of_term (Free (x, Envir.norm_type tyenv T)) ^ " against parameter " ^
          string_of_term (Syntax_Trans.mark_bound_abs (y, Envir.norm_type tyenv U)) ^ " in") rule;
    val (tyenv, _) = fold unify (map #1 vars ~~ take m params)
      (Vartab.empty, Int.max (maxidx, Thm.maxidx_of rule));
    val norm_type = Envir.norm_type tyenv;

    val xs = map (apsnd norm_type o fst) vars;
    val ys = map (apsnd norm_type) (drop m params);
    val ys' = map Name.internal (Name.variant_list (map fst xs) (map fst ys)) ~~ map #2 ys;
    val terms = map (Drule.mk_term o Thm.cterm_of ctxt o Free) (xs @ ys');

    val instT =
      fold (Term.add_tvarsT o #2) params []
      |> map (fn v => (v, Thm.ctyp_of ctxt (norm_type (TVar v))));
    val closed_rule = rule
      |> Thm.forall_intr (Thm.cterm_of ctxt (Free thesis_var))
      |> Thm.instantiate (instT, []);

    val ((_, rule' :: terms'), ctxt') = Variable.import false (closed_rule :: terms) ctxt;
    val vars' =
      map (dest_Free o Thm.term_of o Drule.dest_term) terms' ~~
      (map snd vars @ replicate (length ys) NoSyn);
    val rule'' = Thm.forall_elim (Thm.cterm_of ctxt' (Logic.varify_global (Free thesis_var))) rule';
  in ((vars', rule''), ctxt') end;

fun inferred_type (binding, _, mx) ctxt =
  let
    val x = Variable.check_name binding;
    val ((_, T), ctxt') = Proof_Context.inferred_param x ctxt
  in ((x, T, mx), ctxt') end;

fun polymorphic ctxt vars =
  let val Ts = map Logic.dest_type (Variable.polymorphic ctxt (map (Logic.mk_type o #2) vars))
  in map2 (fn (x, _, mx) => fn T => ((x, T), mx)) vars Ts end;

fun gen_guess prep_var raw_vars int state =
  let
    val _ = Proof.assert_forward_or_chain state;
    val ctxt = Proof.context_of state;
    val chain_facts = if can Proof.assert_chain state then Proof.the_facts state else [];

    val (thesis_var, thesis) = #1 (obtain_thesis ctxt);
    val vars = ctxt
      |> fold_map prep_var raw_vars |-> fold_map inferred_type
      |> fst |> polymorphic ctxt;

    fun guess_context raw_rule state' =
      let
        val ((parms, rule), ctxt') =
          unify_params vars thesis_var raw_rule (Proof.context_of state');
        val (xs, _) = Variable.add_fixes (map (#1 o #1) parms) ctxt';
        val ps = xs ~~ map (#2 o #1) parms;
        val ts = map Free ps;
        val asms =
          Logic.strip_assums_hyp (Logic.nth_prem (1, Thm.prop_of rule))
          |> map (fn asm => (Term.betapplys (fold_rev Term.abs ps asm, ts), []));
        val _ = not (null asms) orelse error "Trivial result -- nothing guessed";
      in
        state'
        |> Proof.map_context (K ctxt')
        |> Proof.fix (map (fn ((x, T), mx) => (Binding.name x, SOME T, mx)) parms)
        |> `Proof.context_of |-> (fn fix_ctxt => Proof.assm
          (obtain_export fix_ctxt rule (map (Thm.cterm_of ctxt) ts))
            [] [] [(Binding.empty_atts, asms)])
        |> Proof.map_context (fold Variable.unbind_term Auto_Bind.no_facts)
      end;

    val goal = Var (("guess", 0), propT);
    val pos = Position.thread_data ();
    fun print_result ctxt' (k, [(s, [_, th])]) =
      Proof_Display.print_results int pos ctxt' (k, [(s, [th])]);
    val before_qed =
      Method.primitive_text (fn ctxt =>
        Goal.conclude #> Raw_Simplifier.norm_hhf ctxt #>
          (fn th => Goal.protect 0 (Conjunction.intr (Drule.mk_term (Thm.cprop_of th)) th)));
    fun after_qed (result_ctxt, results) state' =
      let val [_, res] = Proof_Context.export result_ctxt (Proof.context_of state') (flat results)
      in
        state'
        |> Proof.end_block
        |> guess_context (check_result ctxt thesis res)
      end;
  in
    state
    |> Proof.enter_forward
    |> Proof.begin_block
    |> Proof.fix [(Binding.name Auto_Bind.thesisN, NONE, NoSyn)]
    |> Proof.chain_facts chain_facts
    |> Proof.internal_goal print_result Proof_Context.mode_schematic true "guess"
      (SOME before_qed) after_qed
      [] [] [(Binding.empty_atts, [(Logic.mk_term goal, []), (goal, [])])]
    |> snd
    |> Proof.refine_singleton
        (Method.primitive_text (fn _ => fn _ => Goal.init (Thm.cterm_of ctxt thesis)))
  end;

in

val guess = gen_guess Proof_Context.cert_var;
val guess_cmd = gen_guess Proof_Context.read_var;

end;

end;
