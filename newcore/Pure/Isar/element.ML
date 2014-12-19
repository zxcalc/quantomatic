(*  Title:      Pure/Isar/element.ML
    Author:     Makarius

Explicit data structures for some Isar language elements, with derived
logical operations.
*)

signature ELEMENT =
sig
  datatype ('typ, 'term) stmt =
    Shows of (Attrib.binding * ('term * 'term list) list) list |
    Obtains of (binding * ((binding * 'typ option) list * 'term list)) list
  type statement = (string, string) stmt
  type statement_i = (typ, term) stmt
  datatype ('typ, 'term, 'fact) ctxt =
    Fixes of (binding * 'typ option * mixfix) list |
    Constrains of (string * 'typ) list |
    Assumes of (Attrib.binding * ('term * 'term list) list) list |
    Defines of (Attrib.binding * ('term * 'term list)) list |
    Notes of string * (Attrib.binding * ('fact * Attrib.src list) list) list
  type context = (string, string, Facts.ref) ctxt
  type context_i = (typ, term, thm list) ctxt
  val map_ctxt: {binding: binding -> binding, typ: 'typ -> 'a, term: 'term -> 'b,
    pattern: 'term -> 'b, fact: 'fact -> 'c, attrib: Attrib.src -> Attrib.src} ->
    ('typ, 'term, 'fact) ctxt -> ('a, 'b, 'c) ctxt
  val map_ctxt_attrib: (Attrib.src -> Attrib.src) ->
    ('typ, 'term, 'fact) ctxt -> ('typ, 'term, 'fact) ctxt
  val transform_ctxt: morphism -> context_i -> context_i
  val transform_facts: morphism ->
    (Attrib.binding * (thm list * Args.src list) list) list ->
    (Attrib.binding * (thm list * Args.src list) list) list
  val pretty_stmt: Proof.context -> statement_i -> Pretty.T list
  val pretty_ctxt: Proof.context -> context_i -> Pretty.T list
  val pretty_statement: Proof.context -> string -> thm -> Pretty.T
  type witness
  val prove_witness: Proof.context -> term -> tactic -> witness
  val witness_proof: (witness list list -> Proof.context -> Proof.context) ->
    term list list -> Proof.context -> Proof.state
  val witness_proof_eqs: (witness list list -> thm list -> Proof.context -> Proof.context) ->
    term list list -> term list -> Proof.context -> Proof.state
  val witness_local_proof: (witness list list -> Proof.state -> Proof.state) ->
    string -> term list list -> Proof.context -> bool -> Proof.state -> Proof.state
  val witness_local_proof_eqs: (witness list list -> thm list -> Proof.state -> Proof.state) ->
    string -> term list list -> term list -> Proof.context -> bool -> Proof.state ->
    Proof.state
  val transform_witness: morphism -> witness -> witness
  val conclude_witness: Proof.context -> witness -> thm
  val pretty_witness: Proof.context -> witness -> Pretty.T
  val instT_morphism: theory -> typ Symtab.table -> morphism
  val inst_morphism: theory -> typ Symtab.table * term Symtab.table -> morphism
  val satisfy_morphism: witness list -> morphism
  val eq_morphism: theory -> thm list -> morphism option
  val init: context_i -> Context.generic -> Context.generic
  val init': context_i -> Context.generic -> Context.generic
  val activate_i: context_i -> Proof.context -> context_i * Proof.context
  val activate: (typ, term, Facts.ref) ctxt -> Proof.context -> context_i * Proof.context
end;

structure Element: ELEMENT =
struct

(** language elements **)

(* statement *)

datatype ('typ, 'term) stmt =
  Shows of (Attrib.binding * ('term * 'term list) list) list |
  Obtains of (binding * ((binding * 'typ option) list * 'term list)) list;

type statement = (string, string) stmt;
type statement_i = (typ, term) stmt;


(* context *)

datatype ('typ, 'term, 'fact) ctxt =
  Fixes of (binding * 'typ option * mixfix) list |
  Constrains of (string * 'typ) list |
  Assumes of (Attrib.binding * ('term * 'term list) list) list |
  Defines of (Attrib.binding * ('term * 'term list)) list |
  Notes of string * (Attrib.binding * ('fact * Attrib.src list) list) list;

type context = (string, string, Facts.ref) ctxt;
type context_i = (typ, term, thm list) ctxt;

fun map_ctxt {binding, typ, term, pattern, fact, attrib} =
  fn Fixes fixes => Fixes (fixes |> map (fn (x, T, mx) => (binding x, Option.map typ T, mx)))
   | Constrains xs => Constrains (xs |> map (fn (x, T) =>
      (Variable.check_name (binding (Binding.name x)), typ T)))
   | Assumes asms => Assumes (asms |> map (fn ((a, atts), propps) =>
      ((binding a, map attrib atts), propps |> map (fn (t, ps) => (term t, map pattern ps)))))
   | Defines defs => Defines (defs |> map (fn ((a, atts), (t, ps)) =>
      ((binding a, map attrib atts), (term t, map pattern ps))))
   | Notes (kind, facts) => Notes (kind, facts |> map (fn ((a, atts), bs) =>
      ((binding a, map attrib atts), bs |> map (fn (ths, btts) => (fact ths, map attrib btts)))));

fun map_ctxt_attrib attrib =
  map_ctxt {binding = I, typ = I, term = I, pattern = I, fact = I, attrib = attrib};

fun transform_ctxt phi = map_ctxt
 {binding = Morphism.binding phi,
  typ = Morphism.typ phi,
  term = Morphism.term phi,
  pattern = Morphism.term phi,
  fact = Morphism.fact phi,
  attrib = Args.transform_values phi};

fun transform_facts phi facts =
  Notes ("", facts) |> transform_ctxt phi |> (fn Notes (_, facts') => facts');



(** pretty printing **)

fun pretty_items _ _ [] = []
  | pretty_items keyword sep (x :: ys) =
      Pretty.block [Pretty.keyword2 keyword, Pretty.brk 1, x] ::
        map (fn y => Pretty.block [Pretty.str "  ", Pretty.keyword2 sep, Pretty.brk 1, y]) ys;

fun pretty_name_atts ctxt (b, atts) sep =
  if Attrib.is_empty_binding (b, atts) then []
  else
    [Pretty.block (Pretty.breaks
      (Binding.pretty b :: Attrib.pretty_attribs ctxt atts @ [Pretty.str sep]))];


(* pretty_stmt *)

fun pretty_stmt ctxt =
  let
    val prt_typ = Pretty.quote o Syntax.pretty_typ ctxt;
    val prt_term = Pretty.quote o Syntax.pretty_term ctxt;
    val prt_terms = separate (Pretty.keyword2 "and") o map prt_term;
    val prt_name_atts = pretty_name_atts ctxt;

    fun prt_show (a, ts) =
      Pretty.block (Pretty.breaks (prt_name_atts a ":" @ prt_terms (map fst ts)));

    fun prt_var (x, SOME T) = Pretty.block
          [Pretty.str (Binding.name_of x ^ " ::"), Pretty.brk 1, prt_typ T]
      | prt_var (x, NONE) = Pretty.str (Binding.name_of x);
    val prt_vars = separate (Pretty.keyword2 "and") o map prt_var;

    fun prt_obtain (_, ([], ts)) = Pretty.block (Pretty.breaks (prt_terms ts))
      | prt_obtain (_, (xs, ts)) = Pretty.block (Pretty.breaks
          (prt_vars xs @ [Pretty.keyword2 "where"] @ prt_terms ts));
  in
    fn Shows shows => pretty_items "shows" "and" (map prt_show shows)
     | Obtains obtains => pretty_items "obtains" "|" (map prt_obtain obtains)
  end;


(* pretty_ctxt *)

fun pretty_ctxt ctxt =
  let
    val prt_typ = Pretty.quote o Syntax.pretty_typ ctxt;
    val prt_term = Pretty.quote o Syntax.pretty_term ctxt;
    val prt_thm = Pretty.backquote o Display.pretty_thm ctxt;
    val prt_name_atts = pretty_name_atts ctxt;

    fun prt_mixfix NoSyn = []
      | prt_mixfix mx = [Pretty.brk 2, Mixfix.pretty_mixfix mx];

    fun prt_fix (x, SOME T, mx) = Pretty.block (Pretty.str (Binding.name_of x ^ " ::") ::
          Pretty.brk 1 :: prt_typ T :: Pretty.brk 1 :: prt_mixfix mx)
      | prt_fix (x, NONE, mx) = Pretty.block (Pretty.str (Binding.name_of x) ::
          Pretty.brk 1 :: prt_mixfix mx);
    fun prt_constrain (x, T) = prt_fix (Binding.name x, SOME T, NoSyn);

    fun prt_asm (a, ts) =
      Pretty.block (Pretty.breaks (prt_name_atts a ":" @ map (prt_term o fst) ts));
    fun prt_def (a, (t, _)) =
      Pretty.block (Pretty.breaks (prt_name_atts a ":" @ [prt_term t]));

    fun prt_fact (ths, []) = map prt_thm ths
      | prt_fact (ths, atts) = Pretty.enclose "(" ")"
          (Pretty.breaks (map prt_thm ths)) :: Attrib.pretty_attribs ctxt atts;
    fun prt_note (a, ths) =
      Pretty.block (Pretty.breaks (flat (prt_name_atts a "=" :: map prt_fact ths)));
  in
    fn Fixes fixes => pretty_items "fixes" "and" (map prt_fix fixes)
     | Constrains xs => pretty_items "constrains" "and" (map prt_constrain xs)
     | Assumes asms => pretty_items "assumes" "and" (map prt_asm asms)
     | Defines defs => pretty_items "defines" "and" (map prt_def defs)
     | Notes ("", facts) => pretty_items "notes" "and" (map prt_note facts)
     | Notes (kind, facts) => pretty_items ("notes " ^ kind) "and" (map prt_note facts)
  end;


(* pretty_statement *)

local

fun standard_elim th =
  (case Object_Logic.elim_concl th of
    SOME C =>
      let
        val cert = Thm.cterm_of (Thm.theory_of_thm th);
        val thesis = Var ((Auto_Bind.thesisN, Thm.maxidx_of th + 1), fastype_of C);
        val th' = Thm.instantiate ([], [(cert C, cert thesis)]) th;
      in (th', true) end
  | NONE => (th, false));

fun thm_name kind th prts =
  let val head =
    if Thm.has_name_hint th then
      Pretty.block [Pretty.keyword1 kind,
        Pretty.brk 1, Pretty.str (Long_Name.base_name (Thm.get_name_hint th) ^ ":")]
    else Pretty.keyword1 kind
  in Pretty.block (Pretty.fbreaks (head :: prts)) end;

fun obtain prop ctxt =
  let
    val ((ps, prop'), ctxt') = Variable.focus prop ctxt;
    fun fix (x, T) = (Binding.name (Variable.revert_fixed ctxt' x), SOME T);
    val xs = map (fix o #2) ps;
    val As = Logic.strip_imp_prems prop';
  in ((Binding.empty, (xs, As)), ctxt') end;

in

fun pretty_statement ctxt kind raw_th =
  let
    val thy = Proof_Context.theory_of ctxt;

    val (th, is_elim) = standard_elim (Raw_Simplifier.norm_hhf ctxt raw_th);
    val ((_, [th']), ctxt') = Variable.import true [th] (Variable.set_body true ctxt);
    val prop = Thm.prop_of th';
    val (prems, concl) = Logic.strip_horn prop;
    val concl_term = Object_Logic.drop_judgment thy concl;

    val fixes = fold_aterms (fn v as Free (x, T) =>
        if Variable.newly_fixed ctxt' ctxt x andalso not (v aconv concl_term)
        then insert (op =) (Variable.revert_fixed ctxt' x, T) else I | _ => I) prop [] |> rev;
    val (assumes, cases) = take_suffix (fn prem =>
      is_elim andalso concl aconv Logic.strip_assums_concl prem) prems;
  in
    pretty_ctxt ctxt' (Fixes (map (fn (x, T) => (Binding.name x, SOME T, NoSyn)) fixes)) @
    pretty_ctxt ctxt' (Assumes (map (fn t => (Attrib.empty_binding, [(t, [])])) assumes)) @
     (if null cases then pretty_stmt ctxt' (Shows [(Attrib.empty_binding, [(concl, [])])])
      else
        let val (clauses, ctxt'') = fold_map obtain cases ctxt'
        in pretty_stmt ctxt'' (Obtains clauses) end)
  end |> thm_name kind raw_th;

end;



(** logical operations **)

(* witnesses -- hypotheses as protected facts *)

datatype witness = Witness of term * thm;

val mark_witness = Logic.protect;
fun witness_prop (Witness (t, _)) = t;
fun witness_hyps (Witness (_, th)) = Thm.hyps_of th;
fun map_witness f (Witness witn) = Witness (f witn);

fun transform_witness phi = map_witness (fn (t, th) => (Morphism.term phi t, Morphism.thm phi th));

fun prove_witness ctxt t tac =
  Witness (t,
    Thm.close_derivation
      (Goal.prove ctxt [] [] (mark_witness t) (fn _ => rtac Drule.protectI 1 THEN tac)));


local

val refine_witness =
  Proof.refine (Method.Basic (K (RAW_METHOD
    (K (ALLGOALS (CONJUNCTS (ALLGOALS (CONJUNCTS (TRYALL (rtac Drule.protectI))))))))));

fun gen_witness_proof proof after_qed wit_propss eq_props =
  let
    val propss =
      (map o map) (fn prop => (mark_witness prop, [])) wit_propss @
        [map (rpair []) eq_props];
    fun after_qed' thmss =
      let val (wits, eqs) = split_last ((map o map) Thm.close_derivation thmss);
      in after_qed ((map2 o map2) (curry Witness) wit_propss wits) eqs end;
  in proof after_qed' propss #> refine_witness #> Seq.hd end;

fun proof_local cmd goal_ctxt int after_qed' propss =
  Proof.map_context (K goal_ctxt) #>
  Proof.local_goal (K (K ())) (K I) Proof_Context.bind_propp_i cmd NONE
    after_qed' (map (pair Thm.empty_binding) propss);

in

fun witness_proof after_qed wit_propss =
  gen_witness_proof (Proof.theorem NONE) (fn wits => fn _ => after_qed wits)
    wit_propss [];

val witness_proof_eqs = gen_witness_proof (Proof.theorem NONE);

fun witness_local_proof after_qed cmd wit_propss goal_ctxt int =
  gen_witness_proof (proof_local cmd goal_ctxt int)
    (fn wits => fn _ => after_qed wits) wit_propss [];

fun witness_local_proof_eqs after_qed cmd wit_propss eq_props goal_ctxt int =
  gen_witness_proof (proof_local cmd goal_ctxt int) after_qed wit_propss eq_props;

end;


fun compose_witness (Witness (_, th)) r =
  let
    val th' = Goal.conclude th;
    val A = Thm.cprem_of r 1;
  in
    Thm.implies_elim
      (Conv.gconv_rule Drule.beta_eta_conversion 1 r)
      (Conv.fconv_rule Drule.beta_eta_conversion
        (Thm.instantiate (Thm.match (Thm.cprop_of th', A)) th'))
  end;

fun conclude_witness ctxt (Witness (_, th)) =
  Thm.close_derivation (Raw_Simplifier.norm_hhf_protect ctxt (Goal.conclude th));

fun pretty_witness ctxt witn =
  let val prt_term = Pretty.quote o Syntax.pretty_term ctxt in
    Pretty.block (prt_term (witness_prop witn) ::
      (if Config.get ctxt show_hyps then [Pretty.brk 2, Pretty.list "[" "]"
         (map prt_term (witness_hyps witn))] else []))
  end;


(* derived rules *)

fun instantiate_tfrees thy subst th =
  let
    val certT = Thm.ctyp_of thy;
    val idx = Thm.maxidx_of th + 1;
    fun cert_inst (a, (S, T)) = (certT (TVar ((a, idx), S)), certT T);

    fun add_inst (a, S) insts =
      if AList.defined (op =) insts a then insts
      else (case AList.lookup (op =) subst a of NONE => insts | SOME T => (a, (S, T)) :: insts);
    val insts =
      (Term.fold_types o Term.fold_atyps) (fn TFree v => add_inst v | _ => I)
        (Thm.full_prop_of th) [];
  in
    th
    |> Thm.generalize (map fst insts, []) idx
    |> Thm.instantiate (map cert_inst insts, [])
  end;

fun instantiate_frees thy subst =
  let val cert = Thm.cterm_of thy in
    Drule.forall_intr_list (map (cert o Free o fst) subst) #>
    Drule.forall_elim_list (map (cert o snd) subst)
  end;

fun hyps_rule rule th =
  let val {hyps, ...} = Thm.crep_thm th in
    Drule.implies_elim_list
      (rule (Drule.implies_intr_list hyps th))
      (map (Thm.assume o Drule.cterm_rule rule) hyps)
  end;


(* instantiate types *)

fun instT_type_same env =
  if Symtab.is_empty env then Same.same
  else
    Term_Subst.map_atypsT_same
      (fn TFree (a, _) => (case Symtab.lookup env a of SOME T => T | NONE => raise Same.SAME)
        | _ => raise Same.SAME);

fun instT_term_same env =
  if Symtab.is_empty env then Same.same
  else Term_Subst.map_types_same (instT_type_same env);

val instT_type = Same.commit o instT_type_same;
val instT_term = Same.commit o instT_term_same;

fun instT_subst env th =
  (Thm.fold_terms o Term.fold_types o Term.fold_atyps)
    (fn T as TFree (a, _) =>
      let val T' = the_default T (Symtab.lookup env a)
      in if T = T' then I else insert (eq_fst (op =)) (a, T') end
    | _ => I) th [];

fun instT_thm thy env th =
  if Symtab.is_empty env then th
  else
    let val subst = instT_subst env th
    in if null subst then th else th |> hyps_rule (instantiate_tfrees thy subst) end;

fun instT_morphism thy env =
  Morphism.morphism "Element.instT"
   {binding = [],
    typ = [instT_type env],
    term = [instT_term env],
    fact = [map (instT_thm thy env)]};


(* instantiate types and terms *)

fun inst_term (envT, env) =
  if Symtab.is_empty env then instT_term envT
  else
    instT_term envT #>
    Same.commit (Term_Subst.map_aterms_same
      (fn Free (x, _) => (case Symtab.lookup env x of SOME t => t | NONE => raise Same.SAME)
        | _ => raise Same.SAME)) #>
    Envir.beta_norm;

fun inst_subst (envT, env) th =
  (Thm.fold_terms o Term.fold_aterms)
    (fn Free (x, T) =>
      let
        val T' = instT_type envT T;
        val t = Free (x, T');
        val t' = the_default t (Symtab.lookup env x);
      in if t aconv t' then I else insert (eq_fst (op =)) ((x, T'), t') end
    | _ => I) th [];

fun inst_thm thy (envT, env) th =
  if Symtab.is_empty env then instT_thm thy envT th
  else
    let
      val substT = instT_subst envT th;
      val subst = inst_subst (envT, env) th;
    in
      if null substT andalso null subst then th
      else th |> hyps_rule
       (instantiate_tfrees thy substT #>
        instantiate_frees thy subst #>
        Conv.fconv_rule (Thm.beta_conversion true))
    end;

fun inst_morphism thy (envT, env) =
  Morphism.morphism "Element.inst"
   {binding = [],
    typ = [instT_type envT],
    term = [inst_term (envT, env)],
    fact = [map (inst_thm thy (envT, env))]};


(* satisfy hypotheses *)

fun satisfy_thm witns thm =
  thm |> fold (fn hyp =>
    (case find_first (fn Witness (t, _) => Thm.term_of hyp aconv t) witns of
      NONE => I
    | SOME w => Thm.implies_intr hyp #> compose_witness w)) (#hyps (Thm.crep_thm thm));

val satisfy_morphism = Morphism.thm_morphism "Element.satisfy" o satisfy_thm;


(* rewriting with equalities *)

fun eq_morphism _ [] = NONE
  | eq_morphism thy thms =
      let
        (* FIXME proper context!? *)
        fun rewrite th = rewrite_rule (Proof_Context.init_global (Thm.theory_of_thm th)) thms th;
        val phi =
          Morphism.morphism "Element.eq_morphism"
           {binding = [],
            typ = [],
            term = [Raw_Simplifier.rewrite_term thy thms []],
            fact = [map rewrite]};
      in SOME phi end;



(** activate in context **)

(* init *)

fun init (Fixes fixes) = Context.map_proof (Proof_Context.add_fixes fixes #> #2)
  | init (Constrains _) = I
  | init (Assumes asms) = Context.map_proof (fn ctxt =>
      let
        val asms' = Attrib.map_specs (map (Attrib.attribute ctxt)) asms;
        val (_, ctxt') = ctxt
          |> fold Variable.auto_fixes (maps (map #1 o #2) asms')
          |> Proof_Context.add_assms_i Assumption.assume_export asms';
      in ctxt' end)
  | init (Defines defs) = Context.map_proof (fn ctxt =>
      let
        val defs' = Attrib.map_specs (map (Attrib.attribute ctxt)) defs;
        val asms = defs' |> map (fn (b, (t, ps)) =>
            let val (_, t') = Local_Defs.cert_def ctxt t  (* FIXME adapt ps? *)
            in (t', (b, [(t', ps)])) end);
        val (_, ctxt') = ctxt
          |> fold Variable.auto_fixes (map #1 asms)
          |> Proof_Context.add_assms_i Local_Defs.def_export (map #2 asms);
      in ctxt' end)
  | init (Notes (kind, facts)) = Attrib.generic_notes kind facts #> #2;

fun init' elem context =
  context
  |> Context.mapping I (Thm.unchecked_hyps #> Context_Position.not_really)
  |> init elem
  |> Context.mapping I (fn ctxt =>
      let val ctxt0 = Context.proof_of context
      in ctxt |> Context_Position.restore_visible ctxt0 |> Thm.restore_hyps ctxt0 end);


(* activate *)

fun activate_i elem ctxt =
  let
    val elem' =
      (case map_ctxt_attrib Args.init_assignable elem of
        Defines defs =>
          Defines (defs |> map (fn ((a, atts), (t, ps)) =>
            ((Thm.def_binding_optional (Binding.name (#1 (#1 (Local_Defs.cert_def ctxt t)))) a, atts),
              (t, ps))))
      | e => e);
    val ctxt' = Context.proof_map (init elem') ctxt;
  in (map_ctxt_attrib Args.closure elem', ctxt') end;

fun activate raw_elem ctxt =
  let val elem = raw_elem |> map_ctxt
   {binding = I,
    typ = I,
    term = I,
    pattern = I,
    fact = Proof_Context.get_fact ctxt,
    attrib = Attrib.check_src ctxt}
  in activate_i elem ctxt end;

end;
