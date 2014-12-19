(*  Title:      Pure/Tools/proof_general_pure.ML
    Author:     David Aspinall
    Author:     Makarius

Proof General setup within theory Pure.
*)

(*Proof General legacy*)

structure ProofGeneral_Pure: sig end =
struct

(** preferences **)

(* display *)

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option show_types}
    "show-types"
    "Include types in display of Isabelle terms";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option show_sorts}
    "show-sorts"
    "Include sorts in display of Isabelle types";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option show_consts}
    "show-consts"
    "Show types of consts in Isabelle goal display";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option names_long}
    "long-names"
    "Show fully qualified names in Isabelle terms";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option show_brackets}
    "show-brackets"
    "Show full bracketing in Isabelle terms";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option show_main_goal}
    "show-main-goal"
    "Show main goal in proof state display";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_display
    NONE
    @{system_option eta_contract}
    "eta-contract"
    "Print terms eta-contracted";


(* advanced display *)

val _ =
  ProofGeneral.preference_option ProofGeneral.category_advanced_display
    NONE
    @{system_option goals_limit}
    "goals-limit"
    "Setting for maximum number of subgoals to be printed";

val _ =
  ProofGeneral.preference ProofGeneral.category_advanced_display
    NONE
    (Markup.print_int o get_default_print_depth)
    (default_print_depth o Markup.parse_int)
    ProofGeneral.pgipint
    "print-depth"
    "Setting for the ML print depth";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_advanced_display
    NONE
    @{system_option show_question_marks}
    "show-question-marks"
    "Show leading question mark of variable name";


(* tracing *)

val _ =
  ProofGeneral.preference_bool ProofGeneral.category_tracing
    NONE
    Raw_Simplifier.simp_trace_default
    "trace-simplifier"
    "Trace simplification rules";

val _ =
  ProofGeneral.preference_int ProofGeneral.category_tracing
    NONE
    Raw_Simplifier.simp_trace_depth_limit_default
    "trace-simplifier-depth"
    "Trace simplifier depth limit";

val _ =
  ProofGeneral.preference_bool ProofGeneral.category_tracing
    NONE
    Pattern.unify_trace_failure_default
    "trace-unification"
    "Output error diagnostics during unification";

val _ =
  ProofGeneral.preference_bool ProofGeneral.category_tracing
    NONE
    Toplevel.timing
    "global-timing"
    "Whether to enable timing in Isabelle";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_tracing
    NONE
    @{system_option ML_exception_trace}
    "debugging"
    "Whether to enable exception trace for toplevel command execution";

val _ =
  ProofGeneral.preference_bool ProofGeneral.category_tracing
    NONE
    ProofGeneral.thm_deps
    "theorem-dependencies"
    "Track theorem dependencies within Proof General";


(* proof *)

val _ =
  ProofGeneral.preference_option ProofGeneral.category_proof
    (SOME "true")
    @{system_option quick_and_dirty}
    "quick-and-dirty"
    "Take a few short cuts";

val _ =
  ProofGeneral.preference_option ProofGeneral.category_proof
    NONE
    @{system_option skip_proofs}
    "skip-proofs"
    "Skip over proofs";

val _ =
  ProofGeneral.preference ProofGeneral.category_proof
    NONE
    (Markup.print_bool o Proofterm.proofs_enabled)
    (fn s => Proofterm.proofs := (if Markup.parse_bool s then 2 else 0))
    ProofGeneral.pgipbool
    "full-proofs"
    "Record full proof objects internally";

val _ =
  ProofGeneral.preference ProofGeneral.category_proof
    NONE
    (Markup.print_int o Multithreading.max_threads_value)
    (Multithreading.max_threads_update o Markup.parse_int)
    ProofGeneral.pgipint
    "max-threads"
    "Maximum number of threads";

val _ =
  ProofGeneral.preference ProofGeneral.category_proof
    NONE
    (fn () => Markup.print_bool (! Goal.parallel_proofs >= 1))
    (fn s => Goal.parallel_proofs := (if Markup.parse_bool s then 1 else 0))
    ProofGeneral.pgipint
    "parallel-proofs"
    "Check proofs in parallel";



(** command syntax **)

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.process_pgip"} "(internal)"
    (Parse.text >> (fn str => Toplevel.imperative (fn () => ProofGeneral.process_pgip str)));

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.pr"} "(internal)"
    (Scan.succeed (Toplevel.keep (fn state =>
      if Toplevel.is_toplevel state orelse Toplevel.is_theory state
      then ProofGeneral.tell_clear_goals ()
      else (Toplevel.quiet := false; Toplevel.print_state state))));

val _ = (*undo without output -- historical*)
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.undo"} "(internal)"
    (Scan.succeed (Toplevel.imperative (fn () => Isar.undo 1)));

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.restart"} "(internal)"
    (Parse.opt_unit >> (K (Toplevel.imperative ProofGeneral.restart)));

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.kill_proof"} "(internal)"
    (Scan.succeed (Toplevel.imperative (fn () =>
      (Isar.kill_proof (); ProofGeneral.tell_clear_goals ()))));

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.inform_file_processed"} "(internal)"
    (Parse.name >> (fn file => Toplevel.imperative (fn () =>
      ProofGeneral.inform_file_processed file)));

val _ =
  Outer_Syntax.improper_command
    @{command_spec "ProofGeneral.inform_file_retracted"} "(internal)"
    (Parse.name >> (fn file => Toplevel.imperative (fn () =>
      ProofGeneral.inform_file_retracted file)));

end;

