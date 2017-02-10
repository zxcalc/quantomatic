(*  Title:      Pure/simplifier.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Generic simplifier, suitable for most logics (see also
raw_simplifier.ML for the actual meta-level rewriting engine).
*)

signature BASIC_SIMPLIFIER =
sig
  include BASIC_RAW_SIMPLIFIER
  val simp_tac: Proof.context -> int -> tactic
  val asm_simp_tac: Proof.context -> int -> tactic
  val full_simp_tac: Proof.context -> int -> tactic
  val asm_lr_simp_tac: Proof.context -> int -> tactic
  val asm_full_simp_tac: Proof.context -> int -> tactic
  val safe_simp_tac: Proof.context -> int -> tactic
  val safe_asm_simp_tac: Proof.context -> int -> tactic
  val safe_full_simp_tac: Proof.context -> int -> tactic
  val safe_asm_lr_simp_tac: Proof.context -> int -> tactic
  val safe_asm_full_simp_tac: Proof.context -> int -> tactic
  val simplify: Proof.context -> thm -> thm
  val asm_simplify: Proof.context -> thm -> thm
  val full_simplify: Proof.context -> thm -> thm
  val asm_lr_simplify: Proof.context -> thm -> thm
  val asm_full_simplify: Proof.context -> thm -> thm
end;

signature SIMPLIFIER =
sig
  include BASIC_SIMPLIFIER
  val map_ss: (Proof.context -> Proof.context) -> Context.generic -> Context.generic
  val attrib: (thm -> Proof.context -> Proof.context) -> attribute
  val simp_add: attribute
  val simp_del: attribute
  val cong_add: attribute
  val cong_del: attribute
  val check_simproc: Proof.context -> xstring * Position.T -> string
  val the_simproc: Proof.context -> string -> simproc
  type 'a simproc_spec = {lhss: 'a list, proc: morphism -> Proof.context -> cterm -> thm option}
  val make_simproc: Proof.context -> string -> term simproc_spec -> simproc
  val define_simproc: binding -> term simproc_spec -> local_theory -> local_theory
  val define_simproc_cmd: binding -> string simproc_spec -> local_theory -> local_theory
  val pretty_simpset: bool -> Proof.context -> Pretty.T
  val default_mk_sym: Proof.context -> thm -> thm option
  val prems_of: Proof.context -> thm list
  val add_simp: thm -> Proof.context -> Proof.context
  val del_simp: thm -> Proof.context -> Proof.context
  val init_simpset: thm list -> Proof.context -> Proof.context
  val add_eqcong: thm -> Proof.context -> Proof.context
  val del_eqcong: thm -> Proof.context -> Proof.context
  val add_cong: thm -> Proof.context -> Proof.context
  val del_cong: thm -> Proof.context -> Proof.context
  val add_prems: thm list -> Proof.context -> Proof.context
  val mksimps: Proof.context -> thm -> thm list
  val set_mksimps: (Proof.context -> thm -> thm list) -> Proof.context -> Proof.context
  val set_mkcong: (Proof.context -> thm -> thm) -> Proof.context -> Proof.context
  val set_mksym: (Proof.context -> thm -> thm option) -> Proof.context -> Proof.context
  val set_mkeqTrue: (Proof.context -> thm -> thm option) -> Proof.context -> Proof.context
  val set_termless: (term * term -> bool) -> Proof.context -> Proof.context
  val set_subgoaler: (Proof.context -> int -> tactic) -> Proof.context -> Proof.context
  type trace_ops
  val set_trace_ops: trace_ops -> theory -> theory
  val rewrite: Proof.context -> conv
  val asm_rewrite: Proof.context -> conv
  val full_rewrite: Proof.context -> conv
  val asm_lr_rewrite: Proof.context -> conv
  val asm_full_rewrite: Proof.context -> conv
  val cong_modifiers: Method.modifier parser list
  val simp_modifiers': Method.modifier parser list
  val simp_modifiers: Method.modifier parser list
  val method_setup: Method.modifier parser list -> theory -> theory
  val unsafe_solver_tac: Proof.context -> int -> tactic
  val unsafe_solver: solver
  val safe_solver_tac: Proof.context -> int -> tactic
  val safe_solver: solver
end;

structure Simplifier: SIMPLIFIER =
struct

open Raw_Simplifier;


(** declarations **)

(* attributes *)

fun attrib f = Thm.declaration_attribute (map_ss o f);

val simp_add = attrib add_simp;
val simp_del = attrib del_simp;
val cong_add = attrib add_cong;
val cong_del = attrib del_cong;


(** named simprocs **)

structure Simprocs = Generic_Data
(
  type T = simproc Name_Space.table;
  val empty : T = Name_Space.empty_table "simproc";
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);


(* get simprocs *)

val get_simprocs = Simprocs.get o Context.Proof;

fun check_simproc ctxt = Name_Space.check (Context.Proof ctxt) (get_simprocs ctxt) #> #1;
val the_simproc = Name_Space.get o get_simprocs;

val _ = Theory.setup
  (ML_Antiquotation.value @{binding simproc}
    (Args.context -- Scan.lift (Parse.position Args.name)
      >> (fn (ctxt, name) =>
        "Simplifier.the_simproc ML_context " ^ ML_Syntax.print_string (check_simproc ctxt name))));


(* define simprocs *)

type 'a simproc_spec = {lhss: 'a list, proc: morphism -> Proof.context -> cterm -> thm option};

fun make_simproc ctxt name {lhss, proc} =
  let
    val ctxt' = fold Variable.auto_fixes lhss ctxt;
    val lhss' = Variable.export_terms ctxt' ctxt lhss;
  in
    cert_simproc (Proof_Context.theory_of ctxt) name {lhss = lhss', proc = proc}
  end;

local

fun def_simproc prep b {lhss, proc} lthy =
  let
    val simproc =
      make_simproc lthy (Local_Theory.full_name lthy b) {lhss = prep lthy lhss, proc = proc};
  in
    lthy |> Local_Theory.declaration {syntax = false, pervasive = false} (fn phi => fn context =>
      let
        val b' = Morphism.binding phi b;
        val simproc' = transform_simproc phi simproc;
      in
        context
        |> Simprocs.map (#2 o Name_Space.define context true (b', simproc'))
        |> map_ss (fn ctxt => ctxt addsimprocs [simproc'])
      end)
  end;

in

val define_simproc = def_simproc Syntax.check_terms;
val define_simproc_cmd = def_simproc Syntax.read_terms;

end;



(** pretty_simpset **)

fun pretty_simpset verbose ctxt =
  let
    val pretty_term = Syntax.pretty_term ctxt;
    val pretty_thm = Thm.pretty_thm ctxt;
    val pretty_thm_item = Thm.pretty_thm_item ctxt;

    fun pretty_simproc (name, lhss) =
      Pretty.block
        (Pretty.mark_str name :: Pretty.str ":" :: Pretty.fbrk ::
          Pretty.fbreaks (map (Pretty.item o single o pretty_term) lhss));

    fun pretty_cong_name (const, name) =
      pretty_term ((if const then Const else Free) (name, dummyT));
    fun pretty_cong (name, thm) =
      Pretty.block [pretty_cong_name name, Pretty.str ":", Pretty.brk 1, pretty_thm thm];

    val {simps, procs, congs, loopers, unsafe_solvers, safe_solvers, ...} =
      dest_ss (simpset_of ctxt);
    val simprocs =
      Name_Space.markup_entries verbose ctxt (Name_Space.space_of_table (get_simprocs ctxt)) procs;
  in
    [Pretty.big_list "simplification rules:" (map (pretty_thm_item o #2) simps),
      Pretty.big_list "simplification procedures:" (map pretty_simproc simprocs),
      Pretty.big_list "congruences:" (map pretty_cong congs),
      Pretty.strs ("loopers:" :: map quote loopers),
      Pretty.strs ("unsafe solvers:" :: map quote unsafe_solvers),
      Pretty.strs ("safe solvers:" :: map quote safe_solvers)]
    |> Pretty.chunks
  end;



(** simplification tactics and rules **)

fun solve_all_tac solvers ctxt =
  let
    val {subgoal_tac, ...} = Raw_Simplifier.internal_ss (simpset_of ctxt);
    val solve_tac = subgoal_tac (Raw_Simplifier.set_solvers solvers ctxt) THEN_ALL_NEW (K no_tac);
  in DEPTH_SOLVE (solve_tac 1) end;

(*NOTE: may instantiate unknowns that appear also in other subgoals*)
fun generic_simp_tac safe mode ctxt =
  let
    val {loop_tacs, solvers = (unsafe_solvers, solvers), ...} =
      Raw_Simplifier.internal_ss (simpset_of ctxt);
    val loop_tac = FIRST' (map (fn (_, tac) => tac ctxt) (rev loop_tacs));
    val solve_tac = FIRST' (map (Raw_Simplifier.solver ctxt)
      (rev (if safe then solvers else unsafe_solvers)));

    fun simp_loop_tac i =
      Raw_Simplifier.generic_rewrite_goal_tac mode (solve_all_tac unsafe_solvers) ctxt i THEN
      (solve_tac i ORELSE TRY ((loop_tac THEN_ALL_NEW simp_loop_tac) i));
  in PREFER_GOAL (simp_loop_tac 1) end;

local

fun simp rew mode ctxt thm =
  let
    val {solvers = (unsafe_solvers, _), ...} = Raw_Simplifier.internal_ss (simpset_of ctxt);
    val tacf = solve_all_tac (rev unsafe_solvers);
    fun prover s th = Option.map #1 (Seq.pull (tacf s th));
  in rew mode prover ctxt thm end;

in

val simp_thm = simp Raw_Simplifier.rewrite_thm;
val simp_cterm = simp Raw_Simplifier.rewrite_cterm;

end;


(* tactics *)

val simp_tac = generic_simp_tac false (false, false, false);
val asm_simp_tac = generic_simp_tac false (false, true, false);
val full_simp_tac = generic_simp_tac false (true, false, false);
val asm_lr_simp_tac = generic_simp_tac false (true, true, false);
val asm_full_simp_tac = generic_simp_tac false (true, true, true);

(*not totally safe: may instantiate unknowns that appear also in other subgoals*)
val safe_simp_tac = generic_simp_tac true (false, false, false);
val safe_asm_simp_tac = generic_simp_tac true (false, true, false);
val safe_full_simp_tac = generic_simp_tac true (true, false, false);
val safe_asm_lr_simp_tac = generic_simp_tac true (true, true, false);
val safe_asm_full_simp_tac = generic_simp_tac true (true, true, true);


(* conversions *)

val          simplify = simp_thm (false, false, false);
val      asm_simplify = simp_thm (false, true, false);
val     full_simplify = simp_thm (true, false, false);
val   asm_lr_simplify = simp_thm (true, true, false);
val asm_full_simplify = simp_thm (true, true, true);

val          rewrite = simp_cterm (false, false, false);
val      asm_rewrite = simp_cterm (false, true, false);
val     full_rewrite = simp_cterm (true, false, false);
val   asm_lr_rewrite = simp_cterm (true, true, false);
val asm_full_rewrite = simp_cterm (true, true, true);



(** concrete syntax of attributes **)

(* add / del *)

val simpN = "simp";
val congN = "cong";
val onlyN = "only";
val no_asmN = "no_asm";
val no_asm_useN = "no_asm_use";
val no_asm_simpN = "no_asm_simp";
val asm_lrN = "asm_lr";


(* simprocs *)

local

val add_del =
  (Args.del -- Args.colon >> K (op delsimprocs) ||
    Scan.option (Args.add -- Args.colon) >> K (op addsimprocs))
  >> (fn f => fn simproc => fn phi => Thm.declaration_attribute
      (K (Raw_Simplifier.map_ss (fn ctxt => f (ctxt, [transform_simproc phi simproc])))));

in

val simproc_att =
  (Args.context -- Scan.lift add_del) :|-- (fn (ctxt, decl) =>
    Scan.repeat1 (Scan.lift (Args.named_attribute (decl o the_simproc ctxt o check_simproc ctxt))))
  >> (fn atts => Thm.declaration_attribute (fn th =>
        fold (fn att => Thm.attribute_declaration (Morphism.form att) th) atts));

end;


(* conversions *)

local

fun conv_mode x =
  ((Args.parens (Args.$$$ no_asmN) >> K simplify ||
    Args.parens (Args.$$$ no_asm_simpN) >> K asm_simplify ||
    Args.parens (Args.$$$ no_asm_useN) >> K full_simplify ||
    Scan.succeed asm_full_simplify) |> Scan.lift) x;

in

val simplified = conv_mode -- Attrib.thms >>
  (fn (f, ths) => Thm.rule_attribute ths (fn context =>
    f ((if null ths then I else Raw_Simplifier.clear_simpset)
        (Context.proof_of context) addsimps ths)));

end;


(* setup attributes *)

val _ = Theory.setup
 (Attrib.setup @{binding simp} (Attrib.add_del simp_add simp_del)
    "declaration of Simplifier rewrite rule" #>
  Attrib.setup @{binding cong} (Attrib.add_del cong_add cong_del)
    "declaration of Simplifier congruence rule" #>
  Attrib.setup @{binding simproc} simproc_att
    "declaration of simplification procedures" #>
  Attrib.setup @{binding simplified} simplified "simplified rule");



(** method syntax **)

val cong_modifiers =
 [Args.$$$ congN -- Args.colon >> K (Method.modifier cong_add @{here}),
  Args.$$$ congN -- Args.add -- Args.colon >> K (Method.modifier cong_add @{here}),
  Args.$$$ congN -- Args.del -- Args.colon >> K (Method.modifier cong_del @{here})];

val simp_modifiers =
 [Args.$$$ simpN -- Args.colon >> K (Method.modifier simp_add @{here}),
  Args.$$$ simpN -- Args.add -- Args.colon >> K (Method.modifier simp_add @{here}),
  Args.$$$ simpN -- Args.del -- Args.colon >> K (Method.modifier simp_del @{here}),
  Args.$$$ simpN -- Args.$$$ onlyN -- Args.colon >>
    K {init = Raw_Simplifier.clear_simpset, attribute = simp_add, pos = @{here}}]
   @ cong_modifiers;

val simp_modifiers' =
 [Args.add -- Args.colon >> K (Method.modifier simp_add @{here}),
  Args.del -- Args.colon >> K (Method.modifier simp_del @{here}),
  Args.$$$ onlyN -- Args.colon >>
    K {init = Raw_Simplifier.clear_simpset, attribute = simp_add, pos = @{here}}]
   @ cong_modifiers;

val simp_options =
 (Args.parens (Args.$$$ no_asmN) >> K simp_tac ||
  Args.parens (Args.$$$ no_asm_simpN) >> K asm_simp_tac ||
  Args.parens (Args.$$$ no_asm_useN) >> K full_simp_tac ||
  Args.parens (Args.$$$ asm_lrN) >> K asm_lr_simp_tac ||
  Scan.succeed asm_full_simp_tac);

fun simp_method more_mods meth =
  Scan.lift simp_options --|
    Method.sections (more_mods @ simp_modifiers') >>
    (fn tac => fn ctxt => METHOD (fn facts => meth ctxt tac facts));



(** setup **)

fun method_setup more_mods =
  Method.setup @{binding simp}
    (simp_method more_mods (fn ctxt => fn tac => fn facts =>
      HEADGOAL (Method.insert_tac ctxt facts THEN'
        (CHANGED_PROP oo tac) ctxt)))
    "simplification" #>
  Method.setup @{binding simp_all}
    (simp_method more_mods (fn ctxt => fn tac => fn facts =>
      ALLGOALS (Method.insert_tac ctxt facts) THEN
        (CHANGED_PROP o PARALLEL_ALLGOALS o tac) ctxt))
    "simplification (all goals)";

fun unsafe_solver_tac ctxt =
  FIRST' [resolve_tac ctxt (Drule.reflexive_thm :: Raw_Simplifier.prems_of ctxt), assume_tac ctxt];
val unsafe_solver = mk_solver "Pure unsafe" unsafe_solver_tac;

(*no premature instantiation of variables during simplification*)
fun safe_solver_tac ctxt =
  FIRST' [match_tac ctxt (Drule.reflexive_thm :: Raw_Simplifier.prems_of ctxt), eq_assume_tac];
val safe_solver = mk_solver "Pure safe" safe_solver_tac;

val _ =
  Theory.setup
    (method_setup [] #> Context.theory_map (map_ss (fn ctxt =>
      empty_simpset ctxt
      setSSolver safe_solver
      setSolver unsafe_solver
      |> set_subgoaler asm_simp_tac)));

end;

structure Basic_Simplifier: BASIC_SIMPLIFIER = Simplifier;
open Basic_Simplifier;
