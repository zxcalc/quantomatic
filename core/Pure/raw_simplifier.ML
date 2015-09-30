(*  Title:      Pure/raw_simplifier.ML
    Author:     Tobias Nipkow and Stefan Berghofer, TU Muenchen

Higher-order Simplification.
*)

infix 4
  addsimps delsimps addsimprocs delsimprocs
  setloop addloop delloop
  setSSolver addSSolver setSolver addSolver;

signature BASIC_RAW_SIMPLIFIER =
sig
  val simp_depth_limit: int Config.T
  val simp_trace_depth_limit: int Config.T
  val simp_debug: bool Config.T
  val simp_trace: bool Config.T
  type cong_name = bool * string
  type rrule
  val mk_rrules: Proof.context -> thm list -> rrule list
  val eq_rrule: rrule * rrule -> bool
  type proc
  type solver
  val mk_solver: string -> (Proof.context -> int -> tactic) -> solver
  type simpset
  val empty_ss: simpset
  val merge_ss: simpset * simpset -> simpset
  val dest_ss: simpset ->
   {simps: (string * thm) list,
    procs: (string * cterm list) list,
    congs: (cong_name * thm) list,
    weak_congs: cong_name list,
    loopers: string list,
    unsafe_solvers: string list,
    safe_solvers: string list}
  type simproc
  val eq_simproc: simproc * simproc -> bool
  val transform_simproc: morphism -> simproc -> simproc
  val make_simproc: {name: string, lhss: cterm list,
    proc: morphism -> Proof.context -> cterm -> thm option, identifier: thm list} -> simproc
  val mk_simproc: string -> cterm list -> (Proof.context -> term -> thm option) -> simproc
  val simpset_of: Proof.context -> simpset
  val put_simpset: simpset -> Proof.context -> Proof.context
  val simpset_map: Proof.context -> (Proof.context -> Proof.context) -> simpset -> simpset
  val map_theory_simpset: (Proof.context -> Proof.context) -> theory -> theory
  val empty_simpset: Proof.context -> Proof.context
  val clear_simpset: Proof.context -> Proof.context
  val addsimps: Proof.context * thm list -> Proof.context
  val delsimps: Proof.context * thm list -> Proof.context
  val addsimprocs: Proof.context * simproc list -> Proof.context
  val delsimprocs: Proof.context * simproc list -> Proof.context
  val setloop: Proof.context * (Proof.context -> int -> tactic) -> Proof.context
  val addloop: Proof.context * (string * (Proof.context -> int -> tactic)) -> Proof.context
  val delloop: Proof.context * string -> Proof.context
  val setSSolver: Proof.context * solver -> Proof.context
  val addSSolver: Proof.context * solver -> Proof.context
  val setSolver: Proof.context * solver -> Proof.context
  val addSolver: Proof.context * solver -> Proof.context

  val rewrite_rule: Proof.context -> thm list -> thm -> thm
  val rewrite_goals_rule: Proof.context -> thm list -> thm -> thm
  val rewrite_goals_tac: Proof.context -> thm list -> tactic
  val rewrite_goal_tac: Proof.context -> thm list -> int -> tactic
  val prune_params_tac: Proof.context -> tactic
  val fold_rule: Proof.context -> thm list -> thm -> thm
  val fold_goals_tac: Proof.context -> thm list -> tactic
  val norm_hhf: Proof.context -> thm -> thm
  val norm_hhf_protect: Proof.context -> thm -> thm
end;

signature RAW_SIMPLIFIER =
sig
  include BASIC_RAW_SIMPLIFIER
  exception SIMPLIFIER of string * thm list
  type trace_ops
  val set_trace_ops: trace_ops -> theory -> theory
  val internal_ss: simpset ->
   {congs: (cong_name * thm) list * cong_name list,
    procs: proc Net.net,
    mk_rews:
     {mk: Proof.context -> thm -> thm list,
      mk_cong: Proof.context -> thm -> thm,
      mk_sym: Proof.context -> thm -> thm option,
      mk_eq_True: Proof.context -> thm -> thm option,
      reorient: Proof.context -> term list -> term -> term -> bool},
    termless: term * term -> bool,
    subgoal_tac: Proof.context -> int -> tactic,
    loop_tacs: (string * (Proof.context -> int -> tactic)) list,
    solvers: solver list * solver list}
  val map_ss: (Proof.context -> Proof.context) -> Context.generic -> Context.generic
  val prems_of: Proof.context -> thm list
  val add_simp: thm -> Proof.context -> Proof.context
  val del_simp: thm -> Proof.context -> Proof.context
  val add_eqcong: thm -> Proof.context -> Proof.context
  val del_eqcong: thm -> Proof.context -> Proof.context
  val add_cong: thm -> Proof.context -> Proof.context
  val del_cong: thm -> Proof.context -> Proof.context
  val mksimps: Proof.context -> thm -> thm list
  val set_mksimps: (Proof.context -> thm -> thm list) -> Proof.context -> Proof.context
  val set_mkcong: (Proof.context -> thm -> thm) -> Proof.context -> Proof.context
  val set_mksym: (Proof.context -> thm -> thm option) -> Proof.context -> Proof.context
  val set_mkeqTrue: (Proof.context -> thm -> thm option) -> Proof.context -> Proof.context
  val set_termless: (term * term -> bool) -> Proof.context -> Proof.context
  val set_subgoaler: (Proof.context -> int -> tactic) -> Proof.context -> Proof.context
  val solver: Proof.context -> solver -> int -> tactic
  val simp_depth_limit_raw: Config.raw
  val default_mk_sym: Proof.context -> thm -> thm option
  val simproc_global_i: theory -> string -> term list ->
    (Proof.context -> term -> thm option) -> simproc
  val simproc_global: theory -> string -> string list ->
    (Proof.context -> term -> thm option) -> simproc
  val simp_trace_depth_limit_raw: Config.raw
  val simp_trace_raw: Config.raw
  val simp_debug_raw: Config.raw
  val add_prems: thm list -> Proof.context -> Proof.context
  val set_reorient: (Proof.context -> term list -> term -> term -> bool) ->
    Proof.context -> Proof.context
  val set_solvers: solver list -> Proof.context -> Proof.context
  val rewrite_cterm: bool * bool * bool ->
    (Proof.context -> thm -> thm option) -> Proof.context -> conv
  val rewrite_term: theory -> thm list -> (term -> term option) list -> term -> term
  val rewrite_thm: bool * bool * bool ->
    (Proof.context -> thm -> thm option) -> Proof.context -> thm -> thm
  val generic_rewrite_goal_tac: bool * bool * bool ->
    (Proof.context -> tactic) -> Proof.context -> int -> tactic
  val rewrite: Proof.context -> bool -> thm list -> conv
end;

structure Raw_Simplifier: RAW_SIMPLIFIER =
struct

(** datatype simpset **)

(* congruence rules *)

type cong_name = bool * string;

fun cong_name (Const (a, _)) = SOME (true, a)
  | cong_name (Free (a, _)) = SOME (false, a)
  | cong_name _ = NONE;


(* rewrite rules *)

type rrule =
 {thm: thm,         (*the rewrite rule*)
  name: string,     (*name of theorem from which rewrite rule was extracted*)
  lhs: term,        (*the left-hand side*)
  elhs: cterm,      (*the eta-contracted lhs*)
  extra: bool,      (*extra variables outside of elhs*)
  fo: bool,         (*use first-order matching*)
  perm: bool};      (*the rewrite rule is permutative*)

(*
Remarks:
  - elhs is used for matching,
    lhs only for preservation of bound variable names;
  - fo is set iff
    either elhs is first-order (no Var is applied),
      in which case fo-matching is complete,
    or elhs is not a pattern,
      in which case there is nothing better to do;
*)

fun eq_rrule ({thm = thm1, ...}: rrule, {thm = thm2, ...}: rrule) =
  Thm.eq_thm_prop (thm1, thm2);

(* FIXME: it seems that the conditions on extra variables are too liberal if
prems are nonempty: does solving the prems really guarantee instantiation of
all its Vars? Better: a dynamic check each time a rule is applied.
*)
fun rewrite_rule_extra_vars prems elhs erhs =
  let
    val elhss = elhs :: prems;
    val tvars = fold Term.add_tvars elhss [];
    val vars = fold Term.add_vars elhss [];
  in
    erhs |> Term.exists_type (Term.exists_subtype
      (fn TVar v => not (member (op =) tvars v) | _ => false)) orelse
    erhs |> Term.exists_subterm
      (fn Var v => not (member (op =) vars v) | _ => false)
  end;

fun rrule_extra_vars elhs thm =
  rewrite_rule_extra_vars [] (Thm.term_of elhs) (Thm.full_prop_of thm);

fun mk_rrule2 {thm, name, lhs, elhs, perm} =
  let
    val t = Thm.term_of elhs;
    val fo = Pattern.first_order t orelse not (Pattern.pattern t);
    val extra = rrule_extra_vars elhs thm;
  in {thm = thm, name = name, lhs = lhs, elhs = elhs, extra = extra, fo = fo, perm = perm} end;

(*simple test for looping rewrite rules and stupid orientations*)
fun default_reorient ctxt prems lhs rhs =
  rewrite_rule_extra_vars prems lhs rhs
    orelse
  is_Var (head_of lhs)
    orelse
(* turns t = x around, which causes a headache if x is a local variable -
   usually it is very useful :-(
  is_Free rhs andalso not(is_Free lhs) andalso not(Logic.occs(rhs,lhs))
  andalso not(exists_subterm is_Var lhs)
    orelse
*)
  exists (fn t => Logic.occs (lhs, t)) (rhs :: prems)
    orelse
  null prems andalso Pattern.matches (Proof_Context.theory_of ctxt) (lhs, rhs)
    (*the condition "null prems" is necessary because conditional rewrites
      with extra variables in the conditions may terminate although
      the rhs is an instance of the lhs; example: ?m < ?n ==> f(?n) == f(?m)*)
    orelse
  is_Const lhs andalso not (is_Const rhs);


(* simplification procedures *)

datatype proc =
  Proc of
   {name: string,
    lhs: cterm,
    proc: Proof.context -> cterm -> thm option,
    id: stamp * thm list};

fun eq_procid ((s1: stamp, ths1: thm list), (s2, ths2)) =
  s1 = s2 andalso eq_list Thm.eq_thm (ths1, ths2);

fun eq_proc (Proc {id = id1, ...}, Proc {id = id2, ...}) = eq_procid (id1, id2);


(* solvers *)

datatype solver =
  Solver of
   {name: string,
    solver: Proof.context -> int -> tactic,
    id: stamp};

fun mk_solver name solver = Solver {name = name, solver = solver, id = stamp ()};

fun solver_name (Solver {name, ...}) = name;
fun solver ctxt (Solver {solver = tac, ...}) = tac ctxt;
fun eq_solver (Solver {id = id1, ...}, Solver {id = id2, ...}) = (id1 = id2);


(* simplification sets *)

(*A simpset contains data required during conversion:
    rules: discrimination net of rewrite rules;
    prems: current premises;
    depth: simp_depth and exceeded flag;
    congs: association list of congruence rules and
           a list of `weak' congruence constants.
           A congruence is `weak' if it avoids normalization of some argument.
    procs: discrimination net of simplification procedures
      (functions that prove rewrite rules on the fly);
    mk_rews:
      mk: turn simplification thms into rewrite rules;
      mk_cong: prepare congruence rules;
      mk_sym: turn == around;
      mk_eq_True: turn P into P == True;
    termless: relation for ordered rewriting;*)

datatype simpset =
  Simpset of
   {rules: rrule Net.net,
    prems: thm list,
    depth: int * bool Unsynchronized.ref} *
   {congs: (cong_name * thm) list * cong_name list,
    procs: proc Net.net,
    mk_rews:
     {mk: Proof.context -> thm -> thm list,
      mk_cong: Proof.context -> thm -> thm,
      mk_sym: Proof.context -> thm -> thm option,
      mk_eq_True: Proof.context -> thm -> thm option,
      reorient: Proof.context -> term list -> term -> term -> bool},
    termless: term * term -> bool,
    subgoal_tac: Proof.context -> int -> tactic,
    loop_tacs: (string * (Proof.context -> int -> tactic)) list,
    solvers: solver list * solver list};

fun internal_ss (Simpset (_, ss2)) = ss2;

fun make_ss1 (rules, prems, depth) = {rules = rules, prems = prems, depth = depth};

fun map_ss1 f {rules, prems, depth} = make_ss1 (f (rules, prems, depth));

fun make_ss2 (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =
  {congs = congs, procs = procs, mk_rews = mk_rews, termless = termless,
    subgoal_tac = subgoal_tac, loop_tacs = loop_tacs, solvers = solvers};

fun map_ss2 f {congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers} =
  make_ss2 (f (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers));

fun make_simpset (args1, args2) = Simpset (make_ss1 args1, make_ss2 args2);

fun dest_ss (Simpset ({rules, ...}, {congs, procs, loop_tacs, solvers, ...})) =
 {simps = Net.entries rules
    |> map (fn {name, thm, ...} => (name, thm)),
  procs = Net.entries procs
    |> map (fn Proc {name, lhs, id, ...} => ((name, lhs), id))
    |> partition_eq (eq_snd eq_procid)
    |> map (fn ps => (fst (fst (hd ps)), map (snd o fst) ps)),
  congs = #1 congs,
  weak_congs = #2 congs,
  loopers = map fst loop_tacs,
  unsafe_solvers = map solver_name (#1 solvers),
  safe_solvers = map solver_name (#2 solvers)};


(* empty *)

fun init_ss depth mk_rews termless subgoal_tac solvers =
  make_simpset ((Net.empty, [], depth),
    (([], []), Net.empty, mk_rews, termless, subgoal_tac, [], solvers));

fun default_mk_sym _ th = SOME (th RS Drule.symmetric_thm);

val empty_ss =
  init_ss (0, Unsynchronized.ref false)
    {mk = fn _ => fn th => if can Logic.dest_equals (Thm.concl_of th) then [th] else [],
      mk_cong = K I,
      mk_sym = default_mk_sym,
      mk_eq_True = K (K NONE),
      reorient = default_reorient}
    Term_Ord.termless (K (K no_tac)) ([], []);


(* merge *)  (*NOTE: ignores some fields of 2nd simpset*)

fun merge_ss (ss1, ss2) =
  if pointer_eq (ss1, ss2) then ss1
  else
    let
      val Simpset ({rules = rules1, prems = prems1, depth = depth1},
       {congs = (congs1, weak1), procs = procs1, mk_rews, termless, subgoal_tac,
        loop_tacs = loop_tacs1, solvers = (unsafe_solvers1, solvers1)}) = ss1;
      val Simpset ({rules = rules2, prems = prems2, depth = depth2},
       {congs = (congs2, weak2), procs = procs2, mk_rews = _, termless = _, subgoal_tac = _,
        loop_tacs = loop_tacs2, solvers = (unsafe_solvers2, solvers2)}) = ss2;

      val rules' = Net.merge eq_rrule (rules1, rules2);
      val prems' = Thm.merge_thms (prems1, prems2);
      val depth' = if #1 depth1 < #1 depth2 then depth2 else depth1;
      val congs' = merge (Thm.eq_thm_prop o apply2 #2) (congs1, congs2);
      val weak' = merge (op =) (weak1, weak2);
      val procs' = Net.merge eq_proc (procs1, procs2);
      val loop_tacs' = AList.merge (op =) (K true) (loop_tacs1, loop_tacs2);
      val unsafe_solvers' = merge eq_solver (unsafe_solvers1, unsafe_solvers2);
      val solvers' = merge eq_solver (solvers1, solvers2);
    in
      make_simpset ((rules', prems', depth'), ((congs', weak'), procs',
        mk_rews, termless, subgoal_tac, loop_tacs', (unsafe_solvers', solvers')))
    end;



(** context data **)

structure Simpset = Generic_Data
(
  type T = simpset;
  val empty = empty_ss;
  val extend = I;
  val merge = merge_ss;
);

val simpset_of = Simpset.get o Context.Proof;

fun map_simpset f = Context.proof_map (Simpset.map f);
fun map_simpset1 f = map_simpset (fn Simpset (ss1, ss2) => Simpset (map_ss1 f ss1, ss2));
fun map_simpset2 f = map_simpset (fn Simpset (ss1, ss2) => Simpset (ss1, map_ss2 f ss2));

fun simpset_map ctxt f ss = ctxt |> map_simpset (K ss) |> f |> Context.Proof |> Simpset.get;

fun put_simpset ss = map_simpset (K ss);

val empty_simpset = put_simpset empty_ss;

fun map_theory_simpset f thy =
  let
    val ctxt' = f (Proof_Context.init_global thy);
    val thy' = Proof_Context.theory_of ctxt';
  in Context.theory_map (Simpset.map (K (simpset_of ctxt'))) thy' end;

fun map_ss f = Context.mapping (map_theory_simpset (f o Context_Position.not_really)) f;

val clear_simpset =
  map_simpset (fn Simpset ({depth, ...}, {mk_rews, termless, subgoal_tac, solvers, ...}) =>
    init_ss depth mk_rews termless subgoal_tac solvers);


(* simp depth *)

val simp_depth_limit_raw = Config.declare ("simp_depth_limit", @{here}) (K (Config.Int 100));
val simp_depth_limit = Config.int simp_depth_limit_raw;

val simp_trace_depth_limit_raw =
  Config.declare ("simp_trace_depth_limit", @{here}) (fn _ => Config.Int 1);
val simp_trace_depth_limit = Config.int simp_trace_depth_limit_raw;

fun inc_simp_depth ctxt =
  ctxt |> map_simpset1 (fn (rules, prems, (depth, exceeded)) =>
    (rules, prems,
      (depth + 1,
        if depth = Config.get ctxt simp_trace_depth_limit
        then Unsynchronized.ref false else exceeded)));

fun simp_depth ctxt =
  let val Simpset ({depth = (depth, _), ...}, _) = simpset_of ctxt
  in depth end;


(* diagnostics *)

exception SIMPLIFIER of string * thm list;

val simp_debug_raw = Config.declare ("simp_debug", @{here}) (K (Config.Bool false));
val simp_debug = Config.bool simp_debug_raw;

val simp_trace_raw = Config.declare ("simp_trace", @{here}) (fn _ => Config.Bool false);
val simp_trace = Config.bool simp_trace_raw;

fun cond_warning ctxt msg =
  if Context_Position.is_really_visible ctxt then warning (msg ()) else ();

fun cond_tracing' ctxt flag msg =
  if Config.get ctxt flag then
    let
      val Simpset ({depth = (depth, exceeded), ...}, _) = simpset_of ctxt;
      val depth_limit = Config.get ctxt simp_trace_depth_limit;
    in
      if depth > depth_limit then
        if ! exceeded then () else (tracing "simp_trace_depth_limit exceeded!"; exceeded := true)
      else (tracing (enclose "[" "]" (string_of_int depth) ^ msg ()); exceeded := false)
    end
  else ();

fun cond_tracing ctxt = cond_tracing' ctxt simp_trace;

fun print_term ctxt s t =
  s ^ "\n" ^ Syntax.string_of_term ctxt t;

fun print_thm ctxt s (name, th) =
  print_term ctxt (if name = "" then s else s ^ " " ^ quote name ^ ":") (Thm.full_prop_of th);



(** simpset operations **)

(* prems *)

fun prems_of ctxt =
  let val Simpset ({prems, ...}, _) = simpset_of ctxt in prems end;

fun add_prems ths =
  map_simpset1 (fn (rules, prems, depth) => (rules, ths @ prems, depth));


(* maintain simp rules *)

fun del_rrule (rrule as {thm, elhs, ...}) ctxt =
  ctxt |> map_simpset1 (fn (rules, prems, depth) =>
    (Net.delete_term eq_rrule (Thm.term_of elhs, rrule) rules, prems, depth))
  handle Net.DELETE =>
    (cond_warning ctxt (fn () => print_thm ctxt "Rewrite rule not in simpset:" ("", thm)); ctxt);

fun insert_rrule (rrule as {thm, name, ...}) ctxt =
 (cond_tracing ctxt (fn () => print_thm ctxt "Adding rewrite rule" (name, thm));
  ctxt |> map_simpset1 (fn (rules, prems, depth) =>
    let
      val rrule2 as {elhs, ...} = mk_rrule2 rrule;
      val rules' = Net.insert_term eq_rrule (Thm.term_of elhs, rrule2) rules;
    in (rules', prems, depth) end)
  handle Net.INSERT =>
    (cond_warning ctxt (fn () => print_thm ctxt "Ignoring duplicate rewrite rule:" ("", thm));
      ctxt));

local

fun vperm (Var _, Var _) = true
  | vperm (Abs (_, _, s), Abs (_, _, t)) = vperm (s, t)
  | vperm (t1 $ t2, u1 $ u2) = vperm (t1, u1) andalso vperm (t2, u2)
  | vperm (t, u) = (t = u);

fun var_perm (t, u) =
  vperm (t, u) andalso eq_set (op =) (Term.add_vars t [], Term.add_vars u []);

in

fun decomp_simp thm =
  let
    val prop = Thm.prop_of thm;
    val prems = Logic.strip_imp_prems prop;
    val concl = Drule.strip_imp_concl (Thm.cprop_of thm);
    val (lhs, rhs) = Thm.dest_equals concl handle TERM _ =>
      raise SIMPLIFIER ("Rewrite rule not a meta-equality", [thm]);
    val elhs = Thm.dest_arg (Thm.cprop_of (Thm.eta_conversion lhs));
    val erhs = Envir.eta_contract (Thm.term_of rhs);
    val perm =
      var_perm (Thm.term_of elhs, erhs) andalso
      not (Thm.term_of elhs aconv erhs) andalso
      not (is_Var (Thm.term_of elhs));
  in (prems, Thm.term_of lhs, elhs, Thm.term_of rhs, perm) end;

end;

fun decomp_simp' thm =
  let val (_, lhs, _, rhs, _) = decomp_simp thm in
    if Thm.nprems_of thm > 0 then raise SIMPLIFIER ("Bad conditional rewrite rule", [thm])
    else (lhs, rhs)
  end;

fun mk_eq_True ctxt (thm, name) =
  let val Simpset (_, {mk_rews = {mk_eq_True, ...}, ...}) = simpset_of ctxt in
    (case mk_eq_True ctxt thm of
      NONE => []
    | SOME eq_True =>
        let val (_, lhs, elhs, _, _) = decomp_simp eq_True;
        in [{thm = eq_True, name = name, lhs = lhs, elhs = elhs, perm = false}] end)
  end;

(*create the rewrite rule and possibly also the eq_True variant,
  in case there are extra vars on the rhs*)
fun rrule_eq_True ctxt thm name lhs elhs rhs thm2 =
  let val rrule = {thm = thm, name = name, lhs = lhs, elhs = elhs, perm = false} in
    if rewrite_rule_extra_vars [] lhs rhs then
      mk_eq_True ctxt (thm2, name) @ [rrule]
    else [rrule]
  end;

fun mk_rrule ctxt (thm, name) =
  let val (prems, lhs, elhs, rhs, perm) = decomp_simp thm in
    if perm then [{thm = thm, name = name, lhs = lhs, elhs = elhs, perm = true}]
    else
      (*weak test for loops*)
      if rewrite_rule_extra_vars prems lhs rhs orelse is_Var (Thm.term_of elhs)
      then mk_eq_True ctxt (thm, name)
      else rrule_eq_True ctxt thm name lhs elhs rhs thm
  end;

fun orient_rrule ctxt (thm, name) =
  let
    val (prems, lhs, elhs, rhs, perm) = decomp_simp thm;
    val Simpset (_, {mk_rews = {reorient, mk_sym, ...}, ...}) = simpset_of ctxt;
  in
    if perm then [{thm = thm, name = name, lhs = lhs, elhs = elhs, perm = true}]
    else if reorient ctxt prems lhs rhs then
      if reorient ctxt prems rhs lhs
      then mk_eq_True ctxt (thm, name)
      else
        (case mk_sym ctxt thm of
          NONE => []
        | SOME thm' =>
            let val (_, lhs', elhs', rhs', _) = decomp_simp thm'
            in rrule_eq_True ctxt thm' name lhs' elhs' rhs' thm end)
    else rrule_eq_True ctxt thm name lhs elhs rhs thm
  end;

fun extract_rews ctxt thms =
  let val Simpset (_, {mk_rews = {mk, ...}, ...}) = simpset_of ctxt
  in maps (fn thm => map (rpair (Thm.get_name_hint thm)) (mk ctxt thm)) thms end;

fun extract_safe_rrules ctxt thm =
  maps (orient_rrule ctxt) (extract_rews ctxt [thm]);

fun mk_rrules ctxt thms =
  let
    val rews = extract_rews ctxt thms
    val raw_rrules = flat (map (mk_rrule ctxt) rews)
  in map mk_rrule2 raw_rrules end


(* add/del rules explicitly *)

fun comb_simps ctxt comb mk_rrule thms =
  let
    val rews = extract_rews ctxt thms;
  in fold (fold comb o mk_rrule) rews ctxt end;

fun ctxt addsimps thms =
  comb_simps ctxt insert_rrule (mk_rrule ctxt) thms;

fun ctxt delsimps thms =
  comb_simps ctxt del_rrule (map mk_rrule2 o mk_rrule ctxt) thms;

fun add_simp thm ctxt = ctxt addsimps [thm];
fun del_simp thm ctxt = ctxt delsimps [thm];


(* congs *)

local

fun is_full_cong_prems [] [] = true
  | is_full_cong_prems [] _ = false
  | is_full_cong_prems (p :: prems) varpairs =
      (case Logic.strip_assums_concl p of
        Const ("Pure.eq", _) $ lhs $ rhs =>
          let val (x, xs) = strip_comb lhs and (y, ys) = strip_comb rhs in
            is_Var x andalso forall is_Bound xs andalso
            not (has_duplicates (op =) xs) andalso xs = ys andalso
            member (op =) varpairs (x, y) andalso
            is_full_cong_prems prems (remove (op =) (x, y) varpairs)
          end
      | _ => false);

fun is_full_cong thm =
  let
    val prems = Thm.prems_of thm and concl = Thm.concl_of thm;
    val (lhs, rhs) = Logic.dest_equals concl;
    val (f, xs) = strip_comb lhs and (g, ys) = strip_comb rhs;
  in
    f = g andalso not (has_duplicates (op =) (xs @ ys)) andalso length xs = length ys andalso
    is_full_cong_prems prems (xs ~~ ys)
  end;

fun mk_cong ctxt =
  let val Simpset (_, {mk_rews = {mk_cong = f, ...}, ...}) = simpset_of ctxt
  in f ctxt end;

in

fun add_eqcong thm ctxt = ctxt |> map_simpset2
  (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
    let
      val (lhs, _) = Logic.dest_equals (Thm.concl_of thm)
        handle TERM _ => raise SIMPLIFIER ("Congruence not a meta-equality", [thm]);
    (*val lhs = Envir.eta_contract lhs;*)
      val a = the (cong_name (head_of lhs)) handle Option.Option =>
        raise SIMPLIFIER ("Congruence must start with a constant or free variable", [thm]);
      val (xs, weak) = congs;
      val _ =
        if AList.defined (op =) xs a then
          cond_warning ctxt (fn () => "Overwriting congruence rule for " ^ quote (#2 a))
        else ();
      val xs' = AList.update (op =) (a, thm) xs;
      val weak' = if is_full_cong thm then weak else a :: weak;
    in ((xs', weak'), procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) end);

fun del_eqcong thm ctxt = ctxt |> map_simpset2
  (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
    let
      val (lhs, _) = Logic.dest_equals (Thm.concl_of thm)
        handle TERM _ => raise SIMPLIFIER ("Congruence not a meta-equality", [thm]);
    (*val lhs = Envir.eta_contract lhs;*)
      val a = the (cong_name (head_of lhs)) handle Option.Option =>
        raise SIMPLIFIER ("Congruence must start with a constant", [thm]);
      val (xs, _) = congs;
      val xs' = filter_out (fn (x : cong_name, _) => x = a) xs;
      val weak' = xs' |> map_filter (fn (a, thm) =>
        if is_full_cong thm then NONE else SOME a);
    in ((xs', weak'), procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) end);

fun add_cong thm ctxt = add_eqcong (mk_cong ctxt thm) ctxt;
fun del_cong thm ctxt = del_eqcong (mk_cong ctxt thm) ctxt;

end;


(* simprocs *)

datatype simproc =
  Simproc of
    {name: string,
     lhss: cterm list,
     proc: morphism -> Proof.context -> cterm -> thm option,
     id: stamp * thm list};

fun eq_simproc (Simproc {id = id1, ...}, Simproc {id = id2, ...}) = eq_procid (id1, id2);

fun transform_simproc phi (Simproc {name, lhss, proc, id = (s, ths)}) =
  Simproc
   {name = name,
    lhss = map (Morphism.cterm phi) lhss,
    proc = Morphism.transform phi proc,
    id = (s, Morphism.fact phi ths)};

fun make_simproc {name, lhss, proc, identifier} =
  Simproc {name = name, lhss = lhss, proc = proc, id = (stamp (), identifier)};

fun mk_simproc name lhss proc =
  make_simproc {name = name, lhss = lhss, proc = fn _ => fn ctxt => fn ct =>
    proc ctxt (Thm.term_of ct), identifier = []};

(* FIXME avoid global thy and Logic.varify_global *)
fun simproc_global_i thy name = mk_simproc name o map (Thm.global_cterm_of thy o Logic.varify_global);
fun simproc_global thy name = simproc_global_i thy name o map (Syntax.read_term_global thy);


local

fun add_proc (proc as Proc {name, lhs, ...}) ctxt =
 (cond_tracing ctxt (fn () =>
    print_term ctxt ("Adding simplification procedure " ^ quote name ^ " for") (Thm.term_of lhs));
  ctxt |> map_simpset2
    (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
      (congs, Net.insert_term eq_proc (Thm.term_of lhs, proc) procs,
        mk_rews, termless, subgoal_tac, loop_tacs, solvers))
  handle Net.INSERT =>
    (cond_warning ctxt (fn () => "Ignoring duplicate simplification procedure " ^ quote name);
      ctxt));

fun del_proc (proc as Proc {name, lhs, ...}) ctxt =
  ctxt |> map_simpset2
    (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
      (congs, Net.delete_term eq_proc (Thm.term_of lhs, proc) procs,
        mk_rews, termless, subgoal_tac, loop_tacs, solvers))
  handle Net.DELETE =>
    (cond_warning ctxt (fn () => "Simplification procedure " ^ quote name ^ " not in simpset");
      ctxt);

fun prep_procs (Simproc {name, lhss, proc, id}) =
  lhss |> map (fn lhs => Proc {name = name, lhs = lhs, proc = Morphism.form proc, id = id});

in

fun ctxt addsimprocs ps = fold (fold add_proc o prep_procs) ps ctxt;
fun ctxt delsimprocs ps = fold (fold del_proc o prep_procs) ps ctxt;

end;


(* mk_rews *)

local

fun map_mk_rews f =
  map_simpset2 (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
    let
      val {mk, mk_cong, mk_sym, mk_eq_True, reorient} = mk_rews;
      val (mk', mk_cong', mk_sym', mk_eq_True', reorient') =
        f (mk, mk_cong, mk_sym, mk_eq_True, reorient);
      val mk_rews' = {mk = mk', mk_cong = mk_cong', mk_sym = mk_sym', mk_eq_True = mk_eq_True',
        reorient = reorient'};
    in (congs, procs, mk_rews', termless, subgoal_tac, loop_tacs, solvers) end);

in

fun mksimps ctxt =
  let val Simpset (_, {mk_rews = {mk, ...}, ...}) = simpset_of ctxt
  in mk ctxt end;

fun set_mksimps mk = map_mk_rews (fn (_, mk_cong, mk_sym, mk_eq_True, reorient) =>
  (mk, mk_cong, mk_sym, mk_eq_True, reorient));

fun set_mkcong mk_cong = map_mk_rews (fn (mk, _, mk_sym, mk_eq_True, reorient) =>
  (mk, mk_cong, mk_sym, mk_eq_True, reorient));

fun set_mksym mk_sym = map_mk_rews (fn (mk, mk_cong, _, mk_eq_True, reorient) =>
  (mk, mk_cong, mk_sym, mk_eq_True, reorient));

fun set_mkeqTrue mk_eq_True = map_mk_rews (fn (mk, mk_cong, mk_sym, _, reorient) =>
  (mk, mk_cong, mk_sym, mk_eq_True, reorient));

fun set_reorient reorient = map_mk_rews (fn (mk, mk_cong, mk_sym, mk_eq_True, _) =>
  (mk, mk_cong, mk_sym, mk_eq_True, reorient));

end;


(* termless *)

fun set_termless termless =
  map_simpset2 (fn (congs, procs, mk_rews, _, subgoal_tac, loop_tacs, solvers) =>
   (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers));


(* tactics *)

fun set_subgoaler subgoal_tac =
  map_simpset2 (fn (congs, procs, mk_rews, termless, _, loop_tacs, solvers) =>
   (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers));

fun ctxt setloop tac = ctxt |>
  map_simpset2 (fn (congs, procs, mk_rews, termless, subgoal_tac, _, solvers) =>
   (congs, procs, mk_rews, termless, subgoal_tac, [("", tac)], solvers));

fun ctxt addloop (name, tac) = ctxt |>
  map_simpset2 (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
    (congs, procs, mk_rews, termless, subgoal_tac,
     AList.update (op =) (name, tac) loop_tacs, solvers));

fun ctxt delloop name = ctxt |>
  map_simpset2 (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, solvers) =>
    (congs, procs, mk_rews, termless, subgoal_tac,
     (if AList.defined (op =) loop_tacs name then ()
      else cond_warning ctxt (fn () => "No such looper in simpset: " ^ quote name);
      AList.delete (op =) name loop_tacs), solvers));

fun ctxt setSSolver solver = ctxt |> map_simpset2
  (fn (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, (unsafe_solvers, _)) =>
    (congs, procs, mk_rews, termless, subgoal_tac, loop_tacs, (unsafe_solvers, [solver])));

fun ctxt addSSolver solver = ctxt |> map_simpset2 (fn (congs, procs, mk_rews, termless,
  subgoal_tac, loop_tacs, (unsafe_solvers, solvers)) => (congs, procs, mk_rews, termless,
    subgoal_tac, loop_tacs, (unsafe_solvers, insert eq_solver solver solvers)));

fun ctxt setSolver solver = ctxt |> map_simpset2 (fn (congs, procs, mk_rews, termless,
  subgoal_tac, loop_tacs, (_, solvers)) => (congs, procs, mk_rews, termless,
    subgoal_tac, loop_tacs, ([solver], solvers)));

fun ctxt addSolver solver = ctxt |> map_simpset2 (fn (congs, procs, mk_rews, termless,
  subgoal_tac, loop_tacs, (unsafe_solvers, solvers)) => (congs, procs, mk_rews, termless,
    subgoal_tac, loop_tacs, (insert eq_solver solver unsafe_solvers, solvers)));

fun set_solvers solvers = map_simpset2 (fn (congs, procs, mk_rews, termless,
  subgoal_tac, loop_tacs, _) => (congs, procs, mk_rews, termless,
  subgoal_tac, loop_tacs, (solvers, solvers)));


(* trace operations *)

type trace_ops =
 {trace_invoke: {depth: int, term: term} -> Proof.context -> Proof.context,
  trace_apply: {unconditional: bool, term: term, thm: thm, rrule: rrule} ->
    Proof.context -> (Proof.context -> (thm * term) option) -> (thm * term) option};

structure Trace_Ops = Theory_Data
(
  type T = trace_ops;
  val empty: T =
   {trace_invoke = fn _ => fn ctxt => ctxt,
    trace_apply = fn _ => fn ctxt => fn cont => cont ctxt};
  val extend = I;
  fun merge (trace_ops, _) = trace_ops;
);

val set_trace_ops = Trace_Ops.put;

val trace_ops = Trace_Ops.get o Proof_Context.theory_of;
fun trace_invoke args ctxt = #trace_invoke (trace_ops ctxt) args ctxt;
fun trace_apply args ctxt = #trace_apply (trace_ops ctxt) args ctxt;



(** rewriting **)

(*
  Uses conversions, see:
    L C Paulson, A higher-order implementation of rewriting,
    Science of Computer Programming 3 (1983), pages 119-149.
*)

fun check_conv ctxt msg thm thm' =
  let
    val thm'' = Thm.transitive thm thm' handle THM _ =>
      let
        val nthm' =
          Thm.transitive (Thm.symmetric (Drule.beta_eta_conversion (Thm.lhs_of thm'))) thm'
      in Thm.transitive thm nthm' handle THM _ =>
           let
             val nthm =
               Thm.transitive thm (Drule.beta_eta_conversion (Thm.rhs_of thm))
           in Thm.transitive nthm nthm' end
      end
    val _ =
      if msg then cond_tracing ctxt (fn () => print_thm ctxt "SUCCEEDED" ("", thm'))
      else ();
  in SOME thm'' end
  handle THM _ =>
    let
      val _ $ _ $ prop0 = Thm.prop_of thm;
      val _ =
        cond_tracing ctxt (fn () =>
          print_thm ctxt "Proved wrong theorem (bad subgoaler?)" ("", thm') ^ "\n" ^
          print_term ctxt "Should have proved:" prop0);
    in NONE end;


(* mk_procrule *)

fun mk_procrule ctxt thm =
  let val (prems, lhs, elhs, rhs, _) = decomp_simp thm in
    if rewrite_rule_extra_vars prems lhs rhs
    then (cond_warning ctxt (fn () => print_thm ctxt "Extra vars on rhs:" ("", thm)); [])
    else [mk_rrule2 {thm = thm, name = "", lhs = lhs, elhs = elhs, perm = false}]
  end;


(* rewritec: conversion to apply the meta simpset to a term *)

(*Since the rewriting strategy is bottom-up, we avoid re-normalizing already
  normalized terms by carrying around the rhs of the rewrite rule just
  applied. This is called the `skeleton'. It is decomposed in parallel
  with the term. Once a Var is encountered, the corresponding term is
  already in normal form.
  skel0 is a dummy skeleton that is to enforce complete normalization.*)

val skel0 = Bound 0;

(*Use rhs as skeleton only if the lhs does not contain unnormalized bits.
  The latter may happen iff there are weak congruence rules for constants
  in the lhs.*)

fun uncond_skel ((_, weak), (lhs, rhs)) =
  if null weak then rhs  (*optimization*)
  else if exists_subterm
    (fn Const (a, _) => member (op =) weak (true, a)
      | Free (a, _) => member (op =) weak (false, a)
      | _ => false) lhs then skel0
  else rhs;

(*Behaves like unconditional rule if rhs does not contain vars not in the lhs.
  Otherwise those vars may become instantiated with unnormalized terms
  while the premises are solved.*)

fun cond_skel (args as (_, (lhs, rhs))) =
  if subset (op =) (Term.add_vars rhs [], Term.add_vars lhs []) then uncond_skel args
  else skel0;

(*
  Rewriting -- we try in order:
    (1) beta reduction
    (2) unconditional rewrite rules
    (3) conditional rewrite rules
    (4) simplification procedures

  IMPORTANT: rewrite rules must not introduce new Vars or TVars!
*)

fun rewritec (prover, maxt) ctxt t =
  let
    val Simpset ({rules, ...}, {congs, procs, termless, ...}) = simpset_of ctxt;
    val eta_thm = Thm.eta_conversion t;
    val eta_t' = Thm.rhs_of eta_thm;
    val eta_t = Thm.term_of eta_t';
    fun rew rrule =
      let
        val {thm, name, lhs, elhs, extra, fo, perm} = rrule
        val prop = Thm.prop_of thm;
        val (rthm, elhs') =
          if maxt = ~1 orelse not extra then (thm, elhs)
          else (Thm.incr_indexes (maxt + 1) thm, Thm.incr_indexes_cterm (maxt + 1) elhs);
        val insts =
          if fo then Thm.first_order_match (elhs', eta_t')
          else Thm.match (elhs', eta_t');
        val thm' = Thm.instantiate insts (Thm.rename_boundvars lhs eta_t rthm);
        val prop' = Thm.prop_of thm';
        val unconditional = (Logic.count_prems prop' = 0);
        val (lhs', rhs') = Logic.dest_equals (Logic.strip_imp_concl prop');
        val trace_args = {unconditional = unconditional, term = eta_t, thm = thm', rrule = rrule};
      in
        if perm andalso not (termless (rhs', lhs'))
        then
         (cond_tracing ctxt (fn () =>
            print_thm ctxt "Cannot apply permutative rewrite rule" (name, thm) ^ "\n" ^
            print_thm ctxt "Term does not become smaller:" ("", thm'));
          NONE)
        else
         (cond_tracing ctxt (fn () =>
            print_thm ctxt "Applying instance of rewrite rule" (name, thm));
          if unconditional
          then
           (cond_tracing ctxt (fn () => print_thm ctxt "Rewriting:" ("", thm'));
            trace_apply trace_args ctxt (fn ctxt' =>
              let
                val lr = Logic.dest_equals prop;
                val SOME thm'' = check_conv ctxt' false eta_thm thm';
              in SOME (thm'', uncond_skel (congs, lr)) end))
          else
           (cond_tracing ctxt (fn () => print_thm ctxt "Trying to rewrite:" ("", thm'));
            if simp_depth ctxt > Config.get ctxt simp_depth_limit
            then (cond_tracing ctxt (fn () => "simp_depth_limit exceeded - giving up"); NONE)
            else
              trace_apply trace_args ctxt (fn ctxt' =>
                (case prover ctxt' thm' of
                  NONE => (cond_tracing ctxt' (fn () => print_thm ctxt' "FAILED" ("", thm')); NONE)
                | SOME thm2 =>
                    (case check_conv ctxt' true eta_thm thm2 of
                      NONE => NONE
                    | SOME thm2' =>
                        let
                          val concl = Logic.strip_imp_concl prop;
                          val lr = Logic.dest_equals concl;
                        in SOME (thm2', cond_skel (congs, lr)) end)))))
      end;

    fun rews [] = NONE
      | rews (rrule :: rrules) =
          let val opt = rew rrule handle Pattern.MATCH => NONE
          in (case opt of NONE => rews rrules | some => some) end;

    fun sort_rrules rrs =
      let
        fun is_simple ({thm, ...}: rrule) =
          (case Thm.prop_of thm of
            Const ("Pure.eq", _) $ _ $ _ => true
          | _ => false);
        fun sort [] (re1, re2) = re1 @ re2
          | sort (rr :: rrs) (re1, re2) =
              if is_simple rr
              then sort rrs (rr :: re1, re2)
              else sort rrs (re1, rr :: re2);
      in sort rrs ([], []) end;

    fun proc_rews [] = NONE
      | proc_rews (Proc {name, proc, lhs, ...} :: ps) =
          if Pattern.matches (Proof_Context.theory_of ctxt) (Thm.term_of lhs, Thm.term_of t) then
            (cond_tracing' ctxt simp_debug (fn () =>
              print_term ctxt ("Trying procedure " ^ quote name ^ " on:") eta_t);
             (case proc ctxt eta_t' of
               NONE => (cond_tracing' ctxt simp_debug (fn () => "FAILED"); proc_rews ps)
             | SOME raw_thm =>
                 (cond_tracing ctxt (fn () =>
                    print_thm ctxt ("Procedure " ^ quote name ^ " produced rewrite rule:")
                      ("", raw_thm));
                  (case rews (mk_procrule ctxt raw_thm) of
                    NONE =>
                     (cond_tracing ctxt (fn () =>
                        print_term ctxt ("IGNORED result of simproc " ^ quote name ^
                            " -- does not match") (Thm.term_of t));
                      proc_rews ps)
                  | some => some))))
          else proc_rews ps;
  in
    (case eta_t of
      Abs _ $ _ => SOME (Thm.transitive eta_thm (Thm.beta_conversion false eta_t'), skel0)
    | _ =>
      (case rews (sort_rrules (Net.match_term rules eta_t)) of
        NONE => proc_rews (Net.match_term procs eta_t)
      | some => some))
  end;


(* conversion to apply a congruence rule to a term *)

fun congc prover ctxt maxt cong t =
  let
    val rthm = Thm.incr_indexes (maxt + 1) cong;
    val rlhs = fst (Thm.dest_equals (Drule.strip_imp_concl (Thm.cprop_of rthm)));
    val insts = Thm.match (rlhs, t)
    (* Thm.match can raise Pattern.MATCH;
       is handled when congc is called *)
    val thm' =
      Thm.instantiate insts (Thm.rename_boundvars (Thm.term_of rlhs) (Thm.term_of t) rthm);
    val _ =
      cond_tracing ctxt (fn () => print_thm ctxt "Applying congruence rule:" ("", thm'));
    fun err (msg, thm) = (cond_tracing ctxt (fn () => print_thm ctxt msg ("", thm)); NONE);
  in
    (case prover thm' of
      NONE => err ("Congruence proof failed.  Could not prove", thm')
    | SOME thm2 =>
        (case check_conv ctxt true (Drule.beta_eta_conversion t) thm2 of
          NONE => err ("Congruence proof failed.  Should not have proved", thm2)
        | SOME thm2' =>
            if op aconv (apply2 Thm.term_of (Thm.dest_equals (Thm.cprop_of thm2')))
            then NONE else SOME thm2'))
  end;

val (cA, (cB, cC)) =
  apsnd Thm.dest_equals (Thm.dest_implies (hd (cprems_of Drule.imp_cong)));

fun transitive1 NONE NONE = NONE
  | transitive1 (SOME thm1) NONE = SOME thm1
  | transitive1 NONE (SOME thm2) = SOME thm2
  | transitive1 (SOME thm1) (SOME thm2) = SOME (Thm.transitive thm1 thm2);

fun transitive2 thm = transitive1 (SOME thm);
fun transitive3 thm = transitive1 thm o SOME;

fun bottomc ((simprem, useprem, mutsimp), prover, maxidx) =
  let
    fun botc skel ctxt t =
      if is_Var skel then NONE
      else
        (case subc skel ctxt t of
           some as SOME thm1 =>
             (case rewritec (prover, maxidx) ctxt (Thm.rhs_of thm1) of
                SOME (thm2, skel2) =>
                  transitive2 (Thm.transitive thm1 thm2)
                    (botc skel2 ctxt (Thm.rhs_of thm2))
              | NONE => some)
         | NONE =>
             (case rewritec (prover, maxidx) ctxt t of
                SOME (thm2, skel2) => transitive2 thm2
                  (botc skel2 ctxt (Thm.rhs_of thm2))
              | NONE => NONE))

    and try_botc ctxt t =
      (case botc skel0 ctxt t of
        SOME trec1 => trec1
      | NONE => Thm.reflexive t)

    and subc skel ctxt t0 =
        let val Simpset (_, {congs, ...}) = simpset_of ctxt in
          (case Thm.term_of t0 of
              Abs (a, T, _) =>
                let
                    val (v, ctxt') = Variable.next_bound (a, T) ctxt;
                    val b = #1 (Term.dest_Free v);
                    val (v', t') = Thm.dest_abs (SOME b) t0;
                    val b' = #1 (Term.dest_Free (Thm.term_of v'));
                    val _ =
                      if b <> b' then
                        warning ("Bad Simplifier context: renamed bound variable " ^
                          quote b ^ " to " ^ quote b' ^ Position.here (Position.thread_data ()))
                      else ();
                    val skel' = (case skel of Abs (_, _, sk) => sk | _ => skel0);
                in
                  (case botc skel' ctxt' t' of
                    SOME thm => SOME (Thm.abstract_rule a v' thm)
                  | NONE => NONE)
                end
            | t $ _ =>
              (case t of
                Const ("Pure.imp", _) $ _  => impc t0 ctxt
              | Abs _ =>
                  let val thm = Thm.beta_conversion false t0
                  in
                    (case subc skel0 ctxt (Thm.rhs_of thm) of
                      NONE => SOME thm
                    | SOME thm' => SOME (Thm.transitive thm thm'))
                  end
              | _  =>
                  let
                    fun appc () =
                      let
                        val (tskel, uskel) =
                          (case skel of
                            tskel $ uskel => (tskel, uskel)
                          | _ => (skel0, skel0));
                        val (ct, cu) = Thm.dest_comb t0;
                      in
                        (case botc tskel ctxt ct of
                          SOME thm1 =>
                            (case botc uskel ctxt cu of
                              SOME thm2 => SOME (Thm.combination thm1 thm2)
                            | NONE => SOME (Thm.combination thm1 (Thm.reflexive cu)))
                        | NONE =>
                            (case botc uskel ctxt cu of
                              SOME thm1 => SOME (Thm.combination (Thm.reflexive ct) thm1)
                            | NONE => NONE))
                      end;
                    val (h, ts) = strip_comb t;
                  in
                    (case cong_name h of
                      SOME a =>
                        (case AList.lookup (op =) (fst congs) a of
                           NONE => appc ()
                        | SOME cong =>
     (*post processing: some partial applications h t1 ... tj, j <= length ts,
       may be a redex. Example: map (%x. x) = (%xs. xs) wrt map_cong*)
                           (let
                              val thm = congc (prover ctxt) ctxt maxidx cong t0;
                              val t = the_default t0 (Option.map Thm.rhs_of thm);
                              val (cl, cr) = Thm.dest_comb t
                              val dVar = Var(("", 0), dummyT)
                              val skel =
                                list_comb (h, replicate (length ts) dVar)
                            in
                              (case botc skel ctxt cl of
                                NONE => thm
                              | SOME thm' =>
                                  transitive3 thm (Thm.combination thm' (Thm.reflexive cr)))
                            end handle Pattern.MATCH => appc ()))
                     | _ => appc ())
                  end)
            | _ => NONE)
        end
    and impc ct ctxt =
      if mutsimp then mut_impc0 [] ct [] [] ctxt
      else nonmut_impc ct ctxt

    and rules_of_prem prem ctxt =
      if maxidx_of_term (Thm.term_of prem) <> ~1
      then
       (cond_tracing ctxt (fn () =>
          print_term ctxt "Cannot add premise as rewrite rule because it contains (type) unknowns:"
            (Thm.term_of prem));
        (([], NONE), ctxt))
      else
        let val (asm, ctxt') = Thm.assume_hyps prem ctxt
        in ((extract_safe_rrules ctxt' asm, SOME asm), ctxt') end

    and add_rrules (rrss, asms) ctxt =
      (fold o fold) insert_rrule rrss ctxt |> add_prems (map_filter I asms)

    and disch r prem eq =
      let
        val (lhs, rhs) = Thm.dest_equals (Thm.cprop_of eq);
        val eq' =
          Thm.implies_elim
            (Thm.instantiate ([], [(cA, prem), (cB, lhs), (cC, rhs)]) Drule.imp_cong)
            (Thm.implies_intr prem eq);
      in
        if not r then eq'
        else
          let
            val (prem', concl) = Thm.dest_implies lhs;
            val (prem'', _) = Thm.dest_implies rhs;
          in
            Thm.transitive
              (Thm.transitive
                (Thm.instantiate ([], [(cA, prem'), (cB, prem), (cC, concl)]) Drule.swap_prems_eq)
                eq')
              (Thm.instantiate ([], [(cA, prem), (cB, prem''), (cC, concl)]) Drule.swap_prems_eq)
          end
      end

    and rebuild [] _ _ _ _ eq = eq
      | rebuild (prem :: prems) concl (_ :: rrss) (_ :: asms) ctxt eq =
          let
            val ctxt' = add_rrules (rev rrss, rev asms) ctxt;
            val concl' =
              Drule.mk_implies (prem, the_default concl (Option.map Thm.rhs_of eq));
            val dprem = Option.map (disch false prem);
          in
            (case rewritec (prover, maxidx) ctxt' concl' of
              NONE => rebuild prems concl' rrss asms ctxt (dprem eq)
            | SOME (eq', _) =>
                transitive2 (fold (disch false) prems (the (transitive3 (dprem eq) eq')))
                  (mut_impc0 (rev prems) (Thm.rhs_of eq') (rev rrss) (rev asms) ctxt))
          end

    and mut_impc0 prems concl rrss asms ctxt =
      let
        val prems' = strip_imp_prems concl;
        val ((rrss', asms'), ctxt') = fold_map rules_of_prem prems' ctxt |>> split_list;
      in
        mut_impc (prems @ prems') (strip_imp_concl concl) (rrss @ rrss')
          (asms @ asms') [] [] [] [] ctxt' ~1 ~1
      end

    and mut_impc [] concl [] [] prems' rrss' asms' eqns ctxt changed k =
        transitive1 (fold (fn (eq1, prem) => fn eq2 => transitive1 eq1
            (Option.map (disch false prem) eq2)) (eqns ~~ prems') NONE)
          (if changed > 0 then
             mut_impc (rev prems') concl (rev rrss') (rev asms')
               [] [] [] [] ctxt ~1 changed
           else rebuild prems' concl rrss' asms' ctxt
             (botc skel0 (add_rrules (rev rrss', rev asms') ctxt) concl))

      | mut_impc (prem :: prems) concl (rrs :: rrss) (asm :: asms)
          prems' rrss' asms' eqns ctxt changed k =
        (case (if k = 0 then NONE else botc skel0 (add_rrules
          (rev rrss' @ rrss, rev asms' @ asms) ctxt) prem) of
            NONE => mut_impc prems concl rrss asms (prem :: prems')
              (rrs :: rrss') (asm :: asms') (NONE :: eqns) ctxt changed
              (if k = 0 then 0 else k - 1)
        | SOME eqn =>
            let
              val prem' = Thm.rhs_of eqn;
              val tprems = map Thm.term_of prems;
              val i = 1 + fold Integer.max (map (fn p =>
                find_index (fn q => q aconv p) tprems) (Thm.hyps_of eqn)) ~1;
              val ((rrs', asm'), ctxt') = rules_of_prem prem' ctxt;
            in
              mut_impc prems concl rrss asms (prem' :: prems')
                (rrs' :: rrss') (asm' :: asms')
                (SOME (fold_rev (disch true)
                  (take i prems)
                  (Drule.imp_cong_rule eqn (Thm.reflexive (Drule.list_implies
                    (drop i prems, concl))))) :: eqns)
                ctxt' (length prems') ~1
            end)

    (*legacy code -- only for backwards compatibility*)
    and nonmut_impc ct ctxt =
      let
        val (prem, conc) = Thm.dest_implies ct;
        val thm1 = if simprem then botc skel0 ctxt prem else NONE;
        val prem1 = the_default prem (Option.map Thm.rhs_of thm1);
        val ctxt1 =
          if not useprem then ctxt
          else
            let val ((rrs, asm), ctxt') = rules_of_prem prem1 ctxt
            in add_rrules ([rrs], [asm]) ctxt' end;
      in
        (case botc skel0 ctxt1 conc of
          NONE =>
            (case thm1 of
              NONE => NONE
            | SOME thm1' => SOME (Drule.imp_cong_rule thm1' (Thm.reflexive conc)))
        | SOME thm2 =>
            let val thm2' = disch false prem1 thm2 in
              (case thm1 of
                NONE => SOME thm2'
              | SOME thm1' =>
                 SOME (Thm.transitive (Drule.imp_cong_rule thm1' (Thm.reflexive conc)) thm2'))
            end)
      end;

  in try_botc end;


(* Meta-rewriting: rewrites t to u and returns the theorem t==u *)

(*
  Parameters:
    mode = (simplify A,
            use A in simplifying B,
            use prems of B (if B is again a meta-impl.) to simplify A)
           when simplifying A ==> B
    prover: how to solve premises in conditional rewrites and congruences
*)

fun rewrite_cterm mode prover raw_ctxt raw_ct =
  let
    val thy = Proof_Context.theory_of raw_ctxt;

    val ct = Thm.adjust_maxidx_cterm ~1 raw_ct;
    val maxidx = Thm.maxidx_of_cterm ct;
    val _ =
      Theory.subthy (Thm.theory_of_cterm ct, thy) orelse
        raise CTERM ("rewrite_cterm: bad background theory", [ct]);

    val ctxt =
      raw_ctxt
      |> Context_Position.set_visible false
      |> inc_simp_depth
      |> (fn ctxt => trace_invoke {depth = simp_depth ctxt, term = Thm.term_of ct} ctxt);

    val _ =
      cond_tracing ctxt (fn () =>
        print_term ctxt "SIMPLIFIER INVOKED ON THE FOLLOWING TERM:" (Thm.term_of ct));
  in bottomc (mode, Option.map (Drule.flexflex_unique (SOME ctxt)) oo prover, maxidx) ctxt ct end;

val simple_prover =
  SINGLE o (fn ctxt => ALLGOALS (resolve_tac ctxt (prems_of ctxt)));

fun rewrite _ _ [] = Thm.reflexive
  | rewrite ctxt full thms =
      rewrite_cterm (full, false, false) simple_prover
        (empty_simpset ctxt addsimps thms);

fun rewrite_rule ctxt = Conv.fconv_rule o rewrite ctxt true;

(*simple term rewriting -- no proof*)
fun rewrite_term thy rules procs =
  Pattern.rewrite_term thy (map decomp_simp' rules) procs;

fun rewrite_thm mode prover ctxt = Conv.fconv_rule (rewrite_cterm mode prover ctxt);

(*Rewrite the subgoals of a proof state (represented by a theorem)*)
fun rewrite_goals_rule ctxt thms th =
  Conv.fconv_rule (Conv.prems_conv ~1 (rewrite_cterm (true, true, true) simple_prover
    (empty_simpset ctxt addsimps thms))) th;


(** meta-rewriting tactics **)

(*Rewrite all subgoals*)
fun rewrite_goals_tac ctxt defs = PRIMITIVE (rewrite_goals_rule ctxt defs);

(*Rewrite one subgoal*)
fun generic_rewrite_goal_tac mode prover_tac ctxt i thm =
  if 0 < i andalso i <= Thm.nprems_of thm then
    Seq.single (Conv.gconv_rule (rewrite_cterm mode (SINGLE o prover_tac) ctxt) i thm)
  else Seq.empty;

fun rewrite_goal_tac ctxt rews =
  generic_rewrite_goal_tac (true, false, false) (K no_tac)
    (empty_simpset ctxt addsimps rews);

(*Prunes all redundant parameters from the proof state by rewriting.*)
fun prune_params_tac ctxt = rewrite_goals_tac ctxt [Drule.triv_forall_equality];


(* for folding definitions, handling critical pairs *)

(*The depth of nesting in a term*)
fun term_depth (Abs (_, _, t)) = 1 + term_depth t
  | term_depth (f $ t) = 1 + Int.max (term_depth f, term_depth t)
  | term_depth _ = 0;

val lhs_of_thm = #1 o Logic.dest_equals o Thm.prop_of;

(*folding should handle critical pairs!  E.g. K == Inl(0),  S == Inr(Inl(0))
  Returns longest lhs first to avoid folding its subexpressions.*)
fun sort_lhs_depths defs =
  let val keylist = AList.make (term_depth o lhs_of_thm) defs
      val keys = sort_distinct (rev_order o int_ord) (map #2 keylist)
  in map (AList.find (op =) keylist) keys end;

val rev_defs = sort_lhs_depths o map Thm.symmetric;

fun fold_rule ctxt defs = fold (rewrite_rule ctxt) (rev_defs defs);
fun fold_goals_tac ctxt defs = EVERY (map (rewrite_goals_tac ctxt) (rev_defs defs));


(* HHF normal form: !! before ==>, outermost !! generalized *)

local

fun gen_norm_hhf ss ctxt th =
  (if Drule.is_norm_hhf (Thm.prop_of th) then th
   else
    Conv.fconv_rule
      (rewrite_cterm (true, false, false) (K (K NONE)) (put_simpset ss ctxt)) th)
  |> Thm.adjust_maxidx_thm ~1
  |> Drule.gen_all (Variable.maxidx_of ctxt);

val hhf_ss =
  simpset_of (empty_simpset (Context.proof_of (Context.the_thread_data ()))
    addsimps Drule.norm_hhf_eqs);

val hhf_protect_ss =
  simpset_of (empty_simpset (Context.proof_of (Context.the_thread_data ()))
    addsimps Drule.norm_hhf_eqs |> add_eqcong Drule.protect_cong);

in

val norm_hhf = gen_norm_hhf hhf_ss;
val norm_hhf_protect = gen_norm_hhf hhf_protect_ss;

end;

end;

structure Basic_Meta_Simplifier: BASIC_RAW_SIMPLIFIER = Raw_Simplifier;
open Basic_Meta_Simplifier;
