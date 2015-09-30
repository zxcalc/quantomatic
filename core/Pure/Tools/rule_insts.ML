(*  Title:      Pure/Tools/rule_insts.ML
    Author:     Makarius

Rule instantiations -- operations within implicit rule / subgoal context.
*)

signature RULE_INSTS =
sig
  val where_rule: Proof.context ->
    ((indexname * Position.T) * string) list ->
    (binding * string option * mixfix) list -> thm -> thm
  val of_rule: Proof.context -> string option list * string option list ->
    (binding * string option * mixfix) list -> thm -> thm
  val read_instantiate: Proof.context ->
    ((indexname * Position.T) * string) list -> string list -> thm -> thm
  val read_term: string -> Proof.context -> term * Proof.context
  val schematic: bool Config.T
  val goal_context: term -> Proof.context -> (string * typ) list * Proof.context
  val res_inst_tac: Proof.context ->
    ((indexname * Position.T) * string) list -> (binding * string option * mixfix) list ->
    thm -> int -> tactic
  val eres_inst_tac: Proof.context ->
    ((indexname * Position.T) * string) list -> (binding * string option * mixfix) list ->
    thm -> int -> tactic
  val cut_inst_tac: Proof.context ->
    ((indexname * Position.T) * string) list -> (binding * string option * mixfix) list ->
    thm -> int -> tactic
  val forw_inst_tac: Proof.context ->
    ((indexname * Position.T) * string) list -> (binding * string option * mixfix) list ->
    thm -> int -> tactic
  val dres_inst_tac: Proof.context ->
    ((indexname * Position.T) * string) list -> (binding * string option * mixfix) list ->
    thm -> int -> tactic
  val thin_tac: Proof.context -> string -> (binding * string option * mixfix) list ->
    int -> tactic
  val subgoal_tac: Proof.context -> string -> (binding * string option * mixfix) list ->
    int -> tactic
  val make_elim_preserve: Proof.context -> thm -> thm
  val method:
    (Proof.context -> ((indexname * Position.T) * string) list ->
      (binding * string option * mixfix) list -> thm -> int -> tactic) ->
    (Proof.context -> thm list -> int -> tactic) ->
    (Proof.context -> Proof.method) context_parser
end;

structure Rule_Insts: RULE_INSTS =
struct

(** read instantiations **)

local

fun error_var msg (xi, pos) =
  error (msg ^ quote (Term.string_of_vname xi) ^ Position.here pos);

fun the_sort tvars (xi, pos) : sort =
  (case AList.lookup (op =) tvars xi of
    SOME S => S
  | NONE => error_var "No such type variable in theorem: " (xi, pos));

fun the_type vars (xi, pos) : typ =
  (case AList.lookup (op =) vars xi of
    SOME T => T
  | NONE => error_var "No such variable in theorem: " (xi, pos));

fun read_type ctxt tvars ((xi, pos), s) =
  let
    val S = the_sort tvars (xi, pos);
    val T = Syntax.read_typ ctxt s;
  in
    if Sign.of_sort (Proof_Context.theory_of ctxt) (T, S) then ((xi, S), T)
    else error_var "Bad sort for instantiation of type variable: " (xi, pos)
  end;

fun make_instT f v =
  let
    val T = TVar v;
    val T' = f T;
  in if T = T' then NONE else SOME (v, T') end;

fun make_inst f v =
  let
    val t = Var v;
    val t' = f t;
  in if t aconv t' then NONE else SOME (v, t') end;

fun read_terms ss Ts ctxt =
  let
    fun parse T = if T = propT then Syntax.parse_prop ctxt else Syntax.parse_term ctxt;
    val (ts, ctxt') = fold_map Variable.fix_dummy_patterns (map2 parse Ts ss) ctxt;
    val ts' =
      map2 (Type.constraint o Type_Infer.paramify_vars) Ts ts
      |> Syntax.check_terms ctxt'
      |> Variable.polymorphic ctxt';
    val Ts' = map Term.fastype_of ts';
    val tyenv = fold (Sign.typ_match (Proof_Context.theory_of ctxt)) (Ts ~~ Ts') Vartab.empty;
    val tyenv' = Vartab.fold (fn (xi, (S, T)) => cons ((xi, S), T)) tyenv [];
  in ((ts', tyenv'), ctxt') end;

in

fun read_term s ctxt =
  let
    val (t, ctxt') = Variable.fix_dummy_patterns (Syntax.parse_term ctxt s) ctxt;
    val t' = Syntax.check_term ctxt' t;
  in (t', ctxt') end;

fun read_insts thm raw_insts raw_fixes ctxt =
  let
    val (type_insts, term_insts) =
      List.partition (fn (((x, _), _), _) => String.isPrefix "'" x) raw_insts;

    val tvars = Thm.fold_terms Term.add_tvars thm [];
    val vars = Thm.fold_terms Term.add_vars thm [];

    (*eigen-context*)
    val ctxt1 = ctxt
      |> Variable.declare_thm thm
      |> Proof_Context.read_vars raw_fixes |-> Proof_Context.add_fixes |> #2;

    (*explicit type instantiations*)
    val instT1 = Term_Subst.instantiateT (map (read_type ctxt1 tvars) type_insts);
    val vars1 = map (apsnd instT1) vars;

    (*term instantiations*)
    val (xs, ss) = split_list term_insts;
    val Ts = map (the_type vars1) xs;
    val ((ts, inferred), ctxt2) = read_terms ss Ts ctxt1;

    (*implicit type instantiations*)
    val instT2 = Term_Subst.instantiateT inferred;
    val vars2 = map (apsnd instT2) vars1;
    val inst2 =
      Term_Subst.instantiate ([], map2 (fn (xi, _) => fn t => ((xi, Term.fastype_of t), t)) xs ts)
      #> Envir.beta_norm;

    val inst_tvars = map_filter (make_instT (instT2 o instT1)) tvars;
    val inst_vars = map_filter (make_inst inst2) vars2;
  in ((inst_tvars, inst_vars), ctxt2) end;

end;



(** forward rules **)

fun where_rule ctxt raw_insts raw_fixes thm =
  let
    val ((inst_tvars, inst_vars), ctxt') = read_insts thm raw_insts raw_fixes ctxt;
  in
    thm
    |> Drule.instantiate_normalize
      (map (apply2 (Thm.ctyp_of ctxt') o apfst TVar) inst_tvars,
       map (apply2 (Thm.cterm_of ctxt') o apfst Var) inst_vars)
    |> singleton (Variable.export ctxt' ctxt)
    |> Rule_Cases.save thm
  end;

fun of_rule ctxt (args, concl_args) fixes thm =
  let
    fun zip_vars _ [] = []
      | zip_vars (_ :: xs) (NONE :: rest) = zip_vars xs rest
      | zip_vars ((x, _) :: xs) (SOME t :: rest) = ((x, Position.none), t) :: zip_vars xs rest
      | zip_vars [] _ = error "More instantiations than variables in theorem";
    val insts =
      zip_vars (rev (Term.add_vars (Thm.full_prop_of thm) [])) args @
      zip_vars (rev (Term.add_vars (Thm.concl_of thm) [])) concl_args;
  in where_rule ctxt insts fixes thm end;

fun read_instantiate ctxt insts xs =
  where_rule ctxt insts (map (fn x => (Binding.name x, NONE, NoSyn)) xs);



(** attributes **)

(* where: named instantiation *)

val named_insts =
  Parse.and_list1 (Parse.position Args.var -- (Args.$$$ "=" |-- Parse.!!! Args.name_inner_syntax))
    -- Parse.for_fixes;

val _ = Theory.setup
  (Attrib.setup @{binding "where"}
    (Scan.lift named_insts >> (fn args =>
      Thm.rule_attribute (fn context => uncurry (where_rule (Context.proof_of context)) args)))
    "named instantiation of theorem");


(* of: positional instantiation (terms only) *)

local

val inst = Args.maybe Args.name_inner_syntax;
val concl = Args.$$$ "concl" -- Args.colon;

val insts =
  Scan.repeat (Scan.unless concl inst) --
  Scan.optional (concl |-- Scan.repeat inst) [];

in

val _ = Theory.setup
  (Attrib.setup @{binding "of"}
    (Scan.lift (insts -- Parse.for_fixes) >> (fn args =>
      Thm.rule_attribute (fn context => uncurry (of_rule (Context.proof_of context)) args)))
    "positional instantiation of theorem");

end;



(** tactics **)

(* goal context *)

(*legacy*)
val schematic = Attrib.setup_config_bool @{binding rule_insts_schematic} (K false);

fun goal_context goal ctxt =
  let
    val ((_, params), ctxt') = ctxt
      |> Variable.declare_constraints goal
      |> Variable.improper_fixes
      |> Variable.focus_params goal
      ||> Variable.restore_proper_fixes ctxt
      ||> Config.get ctxt schematic ? Proof_Context.set_mode Proof_Context.mode_schematic;
  in (params, ctxt') end;


(* resolution after lifting and instantiation; may refer to parameters of the subgoal *)

fun bires_inst_tac bires_flag ctxt raw_insts raw_fixes thm i st = CSUBGOAL (fn (cgoal, _) =>
  let
    (*goal context*)
    val (params, goal_ctxt) = goal_context (Thm.term_of cgoal) ctxt;
    val paramTs = map #2 params;

    (*instantiation context*)
    val ((inst_tvars, inst_vars), inst_ctxt) = read_insts thm raw_insts raw_fixes goal_ctxt;
    val fixed = map #1 (fold (Variable.add_newly_fixed inst_ctxt goal_ctxt o #2) inst_vars []);


    (* lift and instantiate rule *)

    val inc = Thm.maxidx_of st + 1;
    val lift_type = Logic.incr_tvar inc;
    fun lift_var ((a, j), T) = Var ((a, j + inc), paramTs ---> lift_type T);
    fun lift_term t = fold_rev Term.absfree params (Logic.incr_indexes (fixed, paramTs, inc) t);

    val inst_tvars' = inst_tvars
      |> map (apply2 (Thm.ctyp_of inst_ctxt o lift_type) o apfst TVar);
    val inst_vars' = inst_vars
      |> map (fn (v, t) => apply2 (Thm.cterm_of inst_ctxt) (lift_var v, lift_term t));

    val thm' = Thm.lift_rule cgoal thm
      |> Drule.instantiate_normalize (inst_tvars', inst_vars')
      |> singleton (Variable.export inst_ctxt ctxt);
  in compose_tac ctxt (bires_flag, thm', Thm.nprems_of thm) i end) i st;

val res_inst_tac = bires_inst_tac false;
val eres_inst_tac = bires_inst_tac true;


(* forward resolution *)

fun make_elim_preserve ctxt rl =
  let
    val maxidx = Thm.maxidx_of rl;
    fun cvar xi = Thm.cterm_of ctxt (Var (xi, propT));
    val revcut_rl' =
      Drule.instantiate_normalize ([], [(cvar ("V", 0), cvar ("V", maxidx + 1)),
        (cvar ("W", 0), cvar ("W", maxidx + 1))]) Drule.revcut_rl;
  in
    (case Seq.list_of
      (Thm.bicompose (SOME ctxt) {flatten = true, match = false, incremented = false}
        (false, rl, Thm.nprems_of rl) 1 revcut_rl')
     of
      [th] => th
    | _ => raise THM ("make_elim_preserve", 1, [rl]))
  end;

(*instantiate and cut -- for atomic fact*)
fun cut_inst_tac ctxt insts fixes rule =
  res_inst_tac ctxt insts fixes (make_elim_preserve ctxt rule);

(*forward tactic applies a rule to an assumption without deleting it*)
fun forw_inst_tac ctxt insts fixes rule =
  cut_inst_tac ctxt insts fixes rule THEN' assume_tac ctxt;

(*dresolve tactic applies a rule to replace an assumption*)
fun dres_inst_tac ctxt insts fixes rule =
  eres_inst_tac ctxt insts fixes (make_elim_preserve ctxt rule);


(* derived tactics *)

(*deletion of an assumption*)
fun thin_tac ctxt s fixes =
  eres_inst_tac ctxt [((("V", 0), Position.none), s)] fixes Drule.thin_rl;

(*Introduce the given proposition as lemma and subgoal*)
fun subgoal_tac ctxt A fixes =
  DETERM o res_inst_tac ctxt [((("psi", 0), Position.none), A)] fixes cut_rl;


(* method wrapper *)

fun method inst_tac tac =
  Args.goal_spec -- Scan.optional (Scan.lift (named_insts --| Args.$$$ "in")) ([], []) --
  Attrib.thms >> (fn ((quant, (insts, fixes)), thms) => fn ctxt => METHOD (fn facts =>
    if null insts andalso null fixes
    then quant (Method.insert_tac facts THEN' tac ctxt thms)
    else
      (case thms of
        [thm] => quant (Method.insert_tac facts THEN' inst_tac ctxt insts fixes thm)
      | _ => error "Cannot have instantiations with multiple rules")));


(* setup *)

(*warning: rule_tac etc. refer to dynamic subgoal context!*)

val _ = Theory.setup
 (Method.setup @{binding rule_tac} (method res_inst_tac resolve_tac)
    "apply rule (dynamic instantiation)" #>
  Method.setup @{binding erule_tac} (method eres_inst_tac eresolve_tac)
    "apply rule in elimination manner (dynamic instantiation)" #>
  Method.setup @{binding drule_tac} (method dres_inst_tac dresolve_tac)
    "apply rule in destruct manner (dynamic instantiation)" #>
  Method.setup @{binding frule_tac} (method forw_inst_tac forward_tac)
    "apply rule in forward manner (dynamic instantiation)" #>
  Method.setup @{binding cut_tac} (method cut_inst_tac (K cut_rules_tac))
    "cut rule (dynamic instantiation)" #>
  Method.setup @{binding subgoal_tac}
    (Args.goal_spec -- Scan.lift (Scan.repeat1 Args.name_inner_syntax -- Parse.for_fixes) >>
      (fn (quant, (props, fixes)) => fn ctxt =>
        SIMPLE_METHOD'' quant (EVERY' (map (fn prop => subgoal_tac ctxt prop fixes) props))))
    "insert subgoal (dynamic instantiation)" #>
  Method.setup @{binding thin_tac}
    (Args.goal_spec -- Scan.lift (Args.name_inner_syntax -- Parse.for_fixes) >>
      (fn (quant, (prop, fixes)) => fn ctxt => SIMPLE_METHOD'' quant (thin_tac ctxt prop fixes)))
    "remove premise (dynamic instantiation)");

end;
