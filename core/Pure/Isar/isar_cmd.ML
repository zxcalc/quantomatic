(*  Title:      Pure/Isar/isar_cmd.ML
    Author:     Markus Wenzel, TU Muenchen

Miscellaneous Isar commands.
*)

signature ISAR_CMD =
sig
  val setup: Input.source -> theory -> theory
  val local_setup: Input.source -> Proof.context -> Proof.context
  val parse_ast_translation: Input.source -> theory -> theory
  val parse_translation: Input.source -> theory -> theory
  val print_translation: Input.source -> theory -> theory
  val typed_print_translation: Input.source -> theory -> theory
  val print_ast_translation: Input.source -> theory -> theory
  val translations: (xstring * string) Syntax.trrule list -> theory -> theory
  val no_translations: (xstring * string) Syntax.trrule list -> theory -> theory
  val oracle: bstring * Position.range -> Input.source -> theory -> theory
  val declaration: {syntax: bool, pervasive: bool} -> Input.source -> local_theory -> local_theory
  val simproc_setup: string * Position.T -> string list -> Input.source ->
    local_theory -> local_theory
  val qed: Method.text_range option -> Toplevel.transition -> Toplevel.transition
  val terminal_proof: Method.text_range * Method.text_range option ->
    Toplevel.transition -> Toplevel.transition
  val default_proof: Toplevel.transition -> Toplevel.transition
  val immediate_proof: Toplevel.transition -> Toplevel.transition
  val done_proof: Toplevel.transition -> Toplevel.transition
  val skip_proof: Toplevel.transition -> Toplevel.transition
  val ml_diag: bool -> Input.source -> Toplevel.transition -> Toplevel.transition
  val diag_state: Proof.context -> Toplevel.state
  val diag_goal: Proof.context -> {context: Proof.context, facts: thm list, goal: thm}
  val pretty_theorems: bool -> Toplevel.state -> Pretty.T list
  val print_stmts: string list * (Facts.ref * Token.src list) list
    -> Toplevel.transition -> Toplevel.transition
  val print_thms: string list * (Facts.ref * Token.src list) list
    -> Toplevel.transition -> Toplevel.transition
  val print_prfs: bool -> string list * (Facts.ref * Token.src list) list option
    -> Toplevel.transition -> Toplevel.transition
  val print_prop: (string list * string) -> Toplevel.transition -> Toplevel.transition
  val print_term: (string list * string) -> Toplevel.transition -> Toplevel.transition
  val print_type: (string list * (string * string option)) ->
    Toplevel.transition -> Toplevel.transition
end;

structure Isar_Cmd: ISAR_CMD =
struct


(** theory declarations **)

(* generic setup *)

fun setup source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "setup" "theory -> theory"
    "Context.map_theory setup"
  |> Context.theory_map;

fun local_setup source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "local_setup" "local_theory -> local_theory"
    "Context.map_proof local_setup"
  |> Context.proof_map;


(* translation functions *)

fun parse_ast_translation source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "parse_ast_translation"
    "(string * (Proof.context -> Ast.ast list -> Ast.ast)) list"
    "Context.map_theory (Sign.parse_ast_translation parse_ast_translation)"
  |> Context.theory_map;

fun parse_translation source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "parse_translation"
    "(string * (Proof.context -> term list -> term)) list"
    "Context.map_theory (Sign.parse_translation parse_translation)"
  |> Context.theory_map;

fun print_translation source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "print_translation"
    "(string * (Proof.context -> term list -> term)) list"
    "Context.map_theory (Sign.print_translation print_translation)"
  |> Context.theory_map;

fun typed_print_translation source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "typed_print_translation"
    "(string * (Proof.context -> typ -> term list -> term)) list"
    "Context.map_theory (Sign.typed_print_translation typed_print_translation)"
  |> Context.theory_map;

fun print_ast_translation source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "print_ast_translation"
    "(string * (Proof.context -> Ast.ast list -> Ast.ast)) list"
    "Context.map_theory (Sign.print_ast_translation print_ast_translation)"
  |> Context.theory_map;


(* translation rules *)

fun read_trrules thy raw_rules =
  let
    val ctxt = Proof_Context.init_global thy;
    val read_root =
      #1 o dest_Type o Proof_Context.read_type_name {proper = true, strict = false} ctxt;
  in
    raw_rules
    |> map (Syntax.map_trrule (fn (r, s) => Syntax_Phases.parse_ast_pattern ctxt (read_root r, s)))
  end;

fun translations args thy = Sign.add_trrules (read_trrules thy args) thy;
fun no_translations args thy = Sign.del_trrules (read_trrules thy args) thy;


(* oracles *)

fun oracle (name, range) source =
  let
    val body_range = Input.range_of source;
    val body = ML_Lex.read_source false source;

    val ants =
      ML_Lex.read
       ("local\n\
        \  val binding = " ^ ML_Syntax.make_binding (name, #1 range) ^ ";\n\
        \  val") @ ML_Lex.read_set_range body_range "oracle" @ ML_Lex.read "=" @ body @
        ML_Lex.read (";\nin\n\
        \  val") @ ML_Lex.read_set_range range name @ ML_Lex.read "=\
        \ snd (Context.>>> (Context.map_theory_result (Thm.add_oracle (binding, oracle))));\n\
        \end;\n";
  in
    Context.theory_map
      (ML_Context.exec (fn () => ML_Context.eval ML_Compiler.flags (#1 body_range) ants))
  end;


(* declarations *)

fun declaration {syntax, pervasive} source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "declaration" "Morphism.declaration"
    ("Context.map_proof (Local_Theory.declaration {syntax = " ^ Bool.toString syntax ^ ", \
      \pervasive = " ^ Bool.toString pervasive ^ "} declaration)")
  |> Context.proof_map;


(* simprocs *)

fun simproc_setup name lhss source =
  ML_Lex.read_source false source
  |> ML_Context.expression (Input.range_of source) "proc"
    "Morphism.morphism -> Proof.context -> cterm -> thm option"
    ("Context.map_proof (Simplifier.define_simproc_cmd " ^
      ML_Syntax.atomic (ML_Syntax.make_binding name) ^
      "{lhss = " ^ ML_Syntax.print_strings lhss ^ ", proc = proc})")
  |> Context.proof_map;


(* local endings *)

fun local_qed m = Toplevel.proof (Proof.local_qed (m, true));
val local_terminal_proof = Toplevel.proof o Proof.local_future_terminal_proof;
val local_default_proof = Toplevel.proof Proof.local_default_proof;
val local_immediate_proof = Toplevel.proof Proof.local_immediate_proof;
val local_done_proof = Toplevel.proof Proof.local_done_proof;
val local_skip_proof = Toplevel.proof' Proof.local_skip_proof;


(* global endings *)

fun global_qed m = Toplevel.end_proof (K (Proof.global_qed (m, true)));
val global_terminal_proof = Toplevel.end_proof o K o Proof.global_future_terminal_proof;
val global_default_proof = Toplevel.end_proof (K Proof.global_default_proof);
val global_immediate_proof = Toplevel.end_proof (K Proof.global_immediate_proof);
val global_skip_proof = Toplevel.end_proof Proof.global_skip_proof;
val global_done_proof = Toplevel.end_proof (K Proof.global_done_proof);


(* common endings *)

fun qed m = local_qed m o global_qed m;
fun terminal_proof m = local_terminal_proof m o global_terminal_proof m;
val default_proof = local_default_proof o global_default_proof;
val immediate_proof = local_immediate_proof o global_immediate_proof;
val done_proof = local_done_proof o global_done_proof;
val skip_proof = local_skip_proof o global_skip_proof;


(* diagnostic ML evaluation *)

structure Diag_State = Proof_Data
(
  type T = Toplevel.state;
  fun init _ = Toplevel.toplevel;
);

fun ml_diag verbose source = Toplevel.keep (fn state =>
  let
    val opt_ctxt =
      try Toplevel.generic_theory_of state
      |> Option.map (Context.proof_of #> Diag_State.put state);
    val flags = ML_Compiler.verbose verbose ML_Compiler.flags;
  in ML_Context.eval_source_in opt_ctxt flags source end);

val diag_state = Diag_State.get;
val diag_goal = Proof.goal o Toplevel.proof_of o diag_state;

val _ = Theory.setup
  (ML_Antiquotation.value (Binding.qualify true "Isar" @{binding state})
    (Scan.succeed "Isar_Cmd.diag_state ML_context") #>
   ML_Antiquotation.value (Binding.qualify true "Isar" @{binding goal})
    (Scan.succeed "Isar_Cmd.diag_goal ML_context"));


(* theorems of theory or proof context *)

fun pretty_theorems verbose st =
  if Toplevel.is_proof st then
    Proof_Context.pretty_local_facts verbose (Toplevel.context_of st)
  else
    let
      val ctxt = Toplevel.context_of st;
      val prev_thys =
        (case Toplevel.previous_context_of st of
          SOME prev => [Proof_Context.theory_of prev]
        | NONE => Theory.parents_of (Proof_Context.theory_of ctxt));
    in Proof_Display.pretty_theorems_diff verbose prev_thys ctxt end;


(* print theorems, terms, types etc. *)

local

fun string_of_stmts ctxt args =
  Attrib.eval_thms ctxt args
  |> map (Element.pretty_statement ctxt Thm.theoremK)
  |> Pretty.chunks2 |> Pretty.string_of;

fun string_of_thms ctxt args =
  Pretty.string_of (Proof_Context.pretty_fact ctxt ("", Attrib.eval_thms ctxt args));

fun string_of_prfs full state arg =
  Pretty.string_of
    (case arg of
      NONE =>
        let
          val {context = ctxt, goal = thm} = Proof.simple_goal (Toplevel.proof_of state);
          val prf = Thm.proof_of thm;
          val prop = Thm.full_prop_of thm;
          val prf' = Proofterm.rewrite_proof_notypes ([], []) prf;
        in
          Proof_Syntax.pretty_proof ctxt
            (if full then Reconstruct.reconstruct_proof ctxt prop prf' else prf')
        end
    | SOME srcs =>
        let
          val ctxt = Toplevel.context_of state;
          val thy = Proof_Context.theory_of ctxt;
          fun pretty_proof thm =
            let
              val (_, prop) =
                Logic.unconstrainT (Thm.shyps_of thm)
                  (Logic.list_implies (Thm.hyps_of thm, Thm.prop_of thm));
            in
              Proofterm.proof_of (Proofterm.strip_thm (Thm.proof_body_of thm))
              |> Reconstruct.reconstruct_proof ctxt prop
              |> Reconstruct.expand_proof ctxt [("", NONE)]
              |> Proofterm.rew_proof thy
              |> Proofterm.no_thm_proofs
              |> not full ? Proofterm.shrink_proof
              |> Proof_Syntax.pretty_proof ctxt
            end;
        in Pretty.chunks (map pretty_proof (Attrib.eval_thms ctxt srcs)) end);

fun string_of_prop ctxt s =
  let
    val prop = Syntax.read_prop ctxt s;
    val ctxt' = Variable.auto_fixes prop ctxt;
  in Pretty.string_of (Pretty.quote (Syntax.pretty_term ctxt' prop)) end;

fun string_of_term ctxt s =
  let
    val t = Syntax.read_term ctxt s;
    val T = Term.type_of t;
    val ctxt' = Variable.auto_fixes t ctxt;
  in
    Pretty.string_of
      (Pretty.block [Pretty.quote (Syntax.pretty_term ctxt' t), Pretty.fbrk,
        Pretty.str "::", Pretty.brk 1, Pretty.quote (Syntax.pretty_typ ctxt' T)])
  end;

fun string_of_type ctxt (s, NONE) =
      let val T = Syntax.read_typ ctxt s
      in Pretty.string_of (Pretty.quote (Syntax.pretty_typ ctxt T)) end
  | string_of_type ctxt (s1, SOME s2) =
      let
        val ctxt' = Config.put show_sorts true ctxt;
        val raw_T = Syntax.parse_typ ctxt' s1;
        val S = Syntax.read_sort ctxt' s2;
        val T =
          Syntax.check_term ctxt'
            (Logic.mk_type raw_T |> Type.constraint (Term.itselfT (Type_Infer.anyT S)))
          |> Logic.dest_type;
      in Pretty.string_of (Pretty.quote (Syntax.pretty_typ ctxt' T)) end;

fun print_item string_of (modes, arg) = Toplevel.keep (fn state =>
  Print_Mode.with_modes modes (fn () => writeln (string_of state arg)) ());

in

val print_stmts = print_item (string_of_stmts o Toplevel.context_of);
val print_thms = print_item (string_of_thms o Toplevel.context_of);
val print_prfs = print_item o string_of_prfs;
val print_prop = print_item (string_of_prop o Toplevel.context_of);
val print_term = print_item (string_of_term o Toplevel.context_of);
val print_type = print_item (string_of_type o Toplevel.context_of);

end;

end;
