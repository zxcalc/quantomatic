(*  Title:      Pure/Tools/rule_insts.ML
    Author:     Makarius

Rule instantiations -- operations within implicit rule / subgoal context.
*)

signature BASIC_RULE_INSTS =
sig
  val res_inst_tac: Proof.context -> (indexname * string) list -> thm -> int -> tactic
  val eres_inst_tac: Proof.context -> (indexname * string) list -> thm -> int -> tactic
  val cut_inst_tac: Proof.context -> (indexname * string) list -> thm -> int -> tactic
  val forw_inst_tac: Proof.context -> (indexname * string) list -> thm -> int -> tactic
  val dres_inst_tac: Proof.context -> (indexname * string) list -> thm -> int -> tactic
  val thin_tac: Proof.context -> string -> int -> tactic
  val subgoal_tac: Proof.context -> string -> int -> tactic
end;

signature RULE_INSTS =
sig
  include BASIC_RULE_INSTS
  val where_rule: Proof.context -> (indexname * string) list ->
    (binding * string option * mixfix) list -> thm -> thm
  val of_rule: Proof.context -> string option list * string option list ->
    (binding * string option * mixfix) list -> thm -> thm
  val read_instantiate: Proof.context -> (indexname * string) list -> string list -> thm -> thm
  val instantiate_tac: Proof.context -> (indexname * string) list -> string list -> tactic
  val make_elim_preserve: thm -> thm
  val method: (Proof.context -> (indexname * string) list -> thm -> int -> tactic) ->
    (Proof.context -> thm list -> int -> tactic) -> (Proof.context -> Proof.method) context_parser
end;

structure Rule_Insts: RULE_INSTS =
struct

(** reading instantiations **)

local

fun error_var msg xi = error (msg ^ quote (Term.string_of_vname xi));

fun the_sort tvars (xi: indexname) =
  (case AList.lookup (op =) tvars xi of
    SOME S => S
  | NONE => error_var "No such type variable in theorem: " xi);

fun the_type vars (xi: indexname) =
  (case AList.lookup (op =) vars xi of
    SOME T => T
  | NONE => error_var "No such variable in theorem: " xi);

fun instantiate inst =
  Term_Subst.instantiate ([], map (fn (xi, t) => ((xi, Term.fastype_of t), t)) inst) #>
  Envir.beta_norm;

fun make_instT f v =
  let
    val T = TVar v;
    val T' = f T;
  in if T = T' then NONE else SOME (T, T') end;

fun make_inst f v =
  let
    val t = Var v;
    val t' = f t;
  in if t aconv t' then NONE else SOME (t, t') end;

val add_used =
  (Thm.fold_terms o fold_types o fold_atyps)
    (fn TFree (a, _) => insert (op =) a
      | TVar ((a, _), _) => insert (op =) a
      | _ => I);

in

fun read_termTs ctxt ss Ts =
  let
    fun parse T = if T = propT then Syntax.parse_prop ctxt else Syntax.parse_term ctxt;
    val ts = map2 parse Ts ss;
    val ts' =
      map2 (Type.constraint o Type_Infer.paramify_vars) Ts ts
      |> Syntax.check_terms ctxt
      |> Variable.polymorphic ctxt;
    val Ts' = map Term.fastype_of ts';
    val tyenv = fold Type.raw_match (Ts ~~ Ts') Vartab.empty;
  in (ts', map (apsnd snd) (Vartab.dest tyenv)) end;

fun read_insts ctxt mixed_insts (tvars, vars) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val cert = Thm.cterm_of thy;
    val certT = Thm.ctyp_of thy;

    val (type_insts, term_insts) = List.partition (String.isPrefix "'" o fst o fst) mixed_insts;


    (* type instantiations *)

    fun readT (xi, s) =
      let
        val S = the_sort tvars xi;
        val T = Syntax.read_typ ctxt s;
      in
        if Sign.of_sort thy (T, S) then ((xi, S), T)
        else error_var "Incompatible sort for typ instantiation of " xi
      end;

    val instT1 = Term_Subst.instantiateT (map readT type_insts);
    val vars1 = map (apsnd instT1) vars;


    (* term instantiations *)

    val (xs, ss) = split_list term_insts;
    val Ts = map (the_type vars1) xs;
    val (ts, inferred) = read_termTs ctxt ss Ts;

    val instT2 = Term.typ_subst_TVars inferred;
    val vars2 = map (apsnd instT2) vars1;
    val inst2 = instantiate (xs ~~ ts);


    (* result *)

    val inst_tvars = map_filter (make_instT (instT2 o instT1)) tvars;
    val inst_vars = map_filter (make_inst inst2) vars2;
  in
    (map (pairself certT) inst_tvars, map (pairself cert) inst_vars)
  end;

fun where_rule ctxt mixed_insts fixes thm =
  let
    val ctxt' = ctxt
      |> Proof_Context.read_vars fixes |-> Proof_Context.add_fixes |> #2
      |> Variable.declare_thm thm
      |> fold (fn a => Variable.declare_names (Logic.mk_type (TFree (a, dummyS)))) (add_used thm []);  (* FIXME !? *)
    val tvars = Thm.fold_terms Term.add_tvars thm [];
    val vars = Thm.fold_terms Term.add_vars thm [];
    val insts = read_insts ctxt' mixed_insts (tvars, vars);
  in
    Drule.instantiate_normalize insts thm
    |> singleton (Proof_Context.export ctxt' ctxt)
    |> Rule_Cases.save thm
  end;

fun of_rule ctxt (args, concl_args) fixes thm =
  let
    fun zip_vars _ [] = []
      | zip_vars (_ :: xs) (NONE :: rest) = zip_vars xs rest
      | zip_vars ((x, _) :: xs) (SOME t :: rest) = (x, t) :: zip_vars xs rest
      | zip_vars [] _ = error "More instantiations than variables in theorem";
    val insts =
      zip_vars (rev (Term.add_vars (Thm.full_prop_of thm) [])) args @
      zip_vars (rev (Term.add_vars (Thm.concl_of thm) [])) concl_args;
  in where_rule ctxt insts fixes thm end;

end;


(* instantiation of rule or goal state *)

fun read_instantiate ctxt insts xs =
  where_rule ctxt insts (map (fn x => (Binding.name x, NONE, NoSyn)) xs);

fun instantiate_tac ctxt insts fixes =
  PRIMITIVE (read_instantiate ctxt insts fixes);



(** attributes **)

(* where: named instantiation *)

val _ = Theory.setup
  (Attrib.setup @{binding "where"}
    (Scan.lift
      (Parse.and_list (Args.var -- (Args.$$$ "=" |-- Args.name_inner_syntax)) -- Parse.for_fixes) >>
      (fn (insts, fixes) =>
        Thm.rule_attribute (fn context => where_rule (Context.proof_of context) insts fixes)))
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
    (Scan.lift (insts -- Parse.for_fixes) >> (fn (args, fixes) =>
      Thm.rule_attribute (fn context => of_rule (Context.proof_of context) args fixes)))
    "positional instantiation of theorem");

end;



(** tactics **)

(* resolution after lifting and instantation; may refer to parameters of the subgoal *)

(* FIXME cleanup this mess!!! *)

fun bires_inst_tac bires_flag ctxt insts thm =
  let
    val thy = Proof_Context.theory_of ctxt;
    (* Separate type and term insts *)
    fun has_type_var ((x, _), _) =
      (case Symbol.explode x of "'" :: _ => true | _ => false);
    val Tinsts = filter has_type_var insts;
    val tinsts = filter_out has_type_var insts;

    (* Tactic *)
    fun tac i st = CSUBGOAL (fn (cgoal, _) =>
      let
        val goal = term_of cgoal;
        val params = Logic.strip_params goal;  (*params of subgoal i as string typ pairs*)
        val params = rev (Term.rename_wrt_term goal params)
          (*as they are printed: bound variables with*)
          (*the same name are renamed during printing*)

        val (param_names, ctxt') = ctxt
          |> Variable.declare_thm thm
          |> Thm.fold_terms Variable.declare_constraints st
          |> Proof_Context.add_fixes (map (fn (x, T) => (Binding.name x, SOME T, NoSyn)) params);

        (* Process type insts: Tinsts_env *)
        fun absent xi = error
              ("No such variable in theorem: " ^ Term.string_of_vname xi);
        val (rtypes, rsorts) = Drule.types_sorts thm;
        fun readT (xi, s) =
            let val S = case rsorts xi of SOME S => S | NONE => absent xi;
                val T = Syntax.read_typ ctxt' s;
                val U = TVar (xi, S);
            in if Sign.typ_instance thy (T, U) then (U, T)
               else error ("Instantiation of " ^ Term.string_of_vname xi ^ " fails")
            end;
        val Tinsts_env = map readT Tinsts;
        (* Preprocess rule: extract vars and their types, apply Tinsts *)
        fun get_typ xi =
          (case rtypes xi of
               SOME T => typ_subst_atomic Tinsts_env T
             | NONE => absent xi);
        val (xis, ss) = Library.split_list tinsts;
        val Ts = map get_typ xis;

        val (ts, envT) =
          read_termTs (Proof_Context.set_mode Proof_Context.mode_schematic ctxt') ss Ts;
        val envT' = map (fn (ixn, T) =>
          (TVar (ixn, the (rsorts ixn)), T)) envT @ Tinsts_env;
        val cenv =
          map
            (fn (xi, t) =>
              pairself (Thm.cterm_of thy) (Var (xi, fastype_of t), t))
            (distinct
              (fn ((x1, t1), (x2, t2)) => x1 = x2 andalso t1 aconv t2)
              (xis ~~ ts));
        (* Lift and instantiate rule *)
        val maxidx = Thm.maxidx_of st;
        val paramTs = map #2 params
        and inc = maxidx+1
        fun liftvar (Var ((a,j), T)) =
              Var((a, j+inc), paramTs ---> Logic.incr_tvar inc T)
          | liftvar t = raise TERM("Variable expected", [t]);
        fun liftterm t =
          fold_rev absfree (param_names ~~ paramTs) (Logic.incr_indexes (paramTs, inc) t);
        fun liftpair (cv, ct) = (cterm_fun liftvar cv, cterm_fun liftterm ct);
        val lifttvar = pairself (ctyp_of thy o Logic.incr_tvar inc);
        val rule = Drule.instantiate_normalize
              (map lifttvar envT', map liftpair cenv)
              (Thm.lift_rule cgoal thm)
      in
        compose_tac (bires_flag, rule, nprems_of thm) i
      end) i st;
  in tac end;

val res_inst_tac = bires_inst_tac false;
val eres_inst_tac = bires_inst_tac true;


(* forward resolution *)

fun make_elim_preserve rl =
  let
    val cert = Thm.cterm_of (Thm.theory_of_thm rl);
    val maxidx = Thm.maxidx_of rl;
    fun cvar xi = cert (Var (xi, propT));
    val revcut_rl' =
      Drule.instantiate_normalize ([], [(cvar ("V", 0), cvar ("V", maxidx + 1)),
        (cvar ("W", 0), cvar ("W", maxidx + 1))]) Drule.revcut_rl;
  in
    (case Seq.list_of
      (Thm.bicompose {flatten = true, match = false, incremented = false}
        (false, rl, Thm.nprems_of rl) 1 revcut_rl')
     of
      [th] => th
    | _ => raise THM ("make_elim_preserve", 1, [rl]))
  end;

(*instantiate and cut -- for atomic fact*)
fun cut_inst_tac ctxt insts rule = res_inst_tac ctxt insts (make_elim_preserve rule);

(*forward tactic applies a rule to an assumption without deleting it*)
fun forw_inst_tac ctxt insts rule = cut_inst_tac ctxt insts rule THEN' assume_tac;

(*dresolve tactic applies a rule to replace an assumption*)
fun dres_inst_tac ctxt insts rule = eres_inst_tac ctxt insts (make_elim_preserve rule);


(* derived tactics *)

(*deletion of an assumption*)
fun thin_tac ctxt s = eres_inst_tac ctxt [(("V", 0), s)] Drule.thin_rl;

(*Introduce the given proposition as lemma and subgoal*)
fun subgoal_tac ctxt A = DETERM o res_inst_tac ctxt [(("psi", 0), A)] cut_rl;



(* method wrapper *)

fun method inst_tac tac =
  Args.goal_spec --
  Scan.optional (Scan.lift
    (Parse.and_list1 (Args.var -- (Args.$$$ "=" |-- Parse.!!! Args.name_inner_syntax)) --|
      Args.$$$ "in")) [] --
  Attrib.thms >>
  (fn ((quant, insts), thms) => fn ctxt => METHOD (fn facts =>
    if null insts then quant (Method.insert_tac facts THEN' tac ctxt thms)
    else
      (case thms of
        [thm] => quant (Method.insert_tac facts THEN' inst_tac ctxt insts thm)
      | _ => error "Cannot have instantiations with multiple rules")));


(* setup *)

(*warning: rule_tac etc. refer to dynamic subgoal context!*)

val _ = Theory.setup
 (Method.setup @{binding rule_tac} (method res_inst_tac (K resolve_tac))
    "apply rule (dynamic instantiation)" #>
  Method.setup @{binding erule_tac} (method eres_inst_tac (K eresolve_tac))
    "apply rule in elimination manner (dynamic instantiation)" #>
  Method.setup @{binding drule_tac} (method dres_inst_tac (K dresolve_tac))
    "apply rule in destruct manner (dynamic instantiation)" #>
  Method.setup @{binding frule_tac} (method forw_inst_tac (K forward_tac))
    "apply rule in forward manner (dynamic instantiation)" #>
  Method.setup @{binding cut_tac} (method cut_inst_tac (K cut_rules_tac))
    "cut rule (dynamic instantiation)" #>
  Method.setup @{binding subgoal_tac}
    (Args.goal_spec -- Scan.lift (Scan.repeat1 Args.name_inner_syntax) >>
      (fn (quant, props) => fn ctxt =>
        SIMPLE_METHOD'' quant (EVERY' (map (subgoal_tac ctxt) props))))
    "insert subgoal (dynamic instantiation)" #>
  Method.setup @{binding thin_tac}
    (Args.goal_spec -- Scan.lift Args.name_inner_syntax >>
      (fn (quant, prop) => fn ctxt => SIMPLE_METHOD'' quant (thin_tac ctxt prop)))
      "remove premise (dynamic instantiation)");

end;

structure Basic_Rule_Insts: BASIC_RULE_INSTS = Rule_Insts;
open Basic_Rule_Insts;
