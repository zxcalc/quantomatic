(*** Isabelle/Pure bootstrap from "RAW" environment ***)

(** bootstrap phase 0: towards secure ML barrier *)

structure Distribution =     (*filled-in by makedist*)
struct
  val version = "Isabelle2014: August 2014";
  val is_identified = true;
  val is_official = true;
end;


(* library of general tools *)

use "General/basics.ML";
use "library.ML";
use "General/print_mode.ML";
use "General/alist.ML";
use "General/table.ML";

use "Concurrent/simple_thread.ML";

use "Concurrent/synchronized.ML";
if Multithreading.available then ()
else use "Concurrent/synchronized_sequential.ML";
use "Concurrent/counter.ML";

use "General/properties.ML";
use "General/output.ML";
use "PIDE/markup.ML";
fun legacy_feature s = warning (Markup.markup Markup.legacy ("Legacy feature! " ^ s));
use "General/scan.ML";
use "General/source.ML";
use "General/symbol.ML";
use "General/position.ML";
use "General/symbol_pos.ML";
use "General/antiquote.ML";
use "ML/ml_lex.ML";
use "ML/ml_parse.ML";
use "General/secure.ML";

val use_text = Secure.use_text;
val use_file = Secure.use_file;

fun use s =
  Position.setmp_thread_data (Position.file_only s)
    (fn () =>
      Secure.use_file ML_Parse.global_context true s
        handle ERROR msg => (writeln msg; error "ML error")) ();

val toplevel_pp = Secure.toplevel_pp;


(** bootstrap phase 1: towards ML within Isar context *)

(* library of general tools *)

use "General/integer.ML";
use "General/stack.ML";
use "General/queue.ML";
use "General/heap.ML";
use "General/same.ML";
use "General/ord_list.ML";
use "General/balanced_tree.ML";
use "General/linear_set.ML";
use "General/buffer.ML";
use "General/pretty.ML";
use "General/path.ML";
use "General/url.ML";
use "General/file.ML";
use "General/long_name.ML";
use "General/binding.ML";
use "General/socket_io.ML";
use "General/seq.ML";
use "General/timing.ML";

use "General/sha1.ML";
if ML_System.is_polyml then use "General/sha1_polyml.ML" else ();
use "General/sha1_samples.ML";

use "PIDE/xml.ML";
use "PIDE/yxml.ML";
use "PIDE/document_id.ML";

use "General/change_table.ML";
use "General/graph.ML";

use "System/options.ML";


(* concurrency within the ML runtime *)

if ML_System.is_polyml
then use "ML/exn_properties_polyml.ML"
else use "ML/exn_properties_dummy.ML";

if ML_System.name = "polyml-5.5.1"
  orelse ML_System.name = "polyml-5.5.2"
then use "ML/exn_trace_polyml-5.5.1.ML"
else ();

if ML_System.name = "polyml-5.5.0"
  orelse ML_System.name = "polyml-5.5.1"
  orelse ML_System.name = "polyml-5.5.2"
then use "ML/ml_statistics_polyml-5.5.0.ML"
else use "ML/ml_statistics_dummy.ML";

use "Concurrent/single_assignment.ML";
if Multithreading.available then ()
else use "Concurrent/single_assignment_sequential.ML";

if Multithreading.available
then use "Concurrent/bash.ML"
else use "Concurrent/bash_sequential.ML";

use "Concurrent/par_exn.ML";
use "Concurrent/task_queue.ML";
use "Concurrent/future.ML";
use "Concurrent/event_timer.ML";

if ML_System.is_polyml then use "Concurrent/time_limit.ML" else ();

use "Concurrent/lazy.ML";
if Multithreading.available then ()
else use "Concurrent/lazy_sequential.ML";

use "Concurrent/par_list.ML";
if Multithreading.available then ()
else use "Concurrent/par_list_sequential.ML";

use "Concurrent/mailbox.ML";
use "Concurrent/cache.ML";

use "PIDE/active.ML";


(* fundamental structures *)

use "name.ML";
use "term.ML";
use "context.ML";
use "context_position.ML";
use "config.ML";


(* inner syntax *)

use "Syntax/type_annotation.ML";
use "Syntax/term_position.ML";
use "Syntax/lexicon.ML";
use "Syntax/ast.ML";
use "Syntax/syntax_ext.ML";
use "Syntax/parser.ML";
use "Syntax/syntax_trans.ML";
use "Syntax/mixfix.ML";
use "Syntax/printer.ML";
use "Syntax/syntax.ML";


(* core of tactical proof system *)

use "term_ord.ML";
use "term_subst.ML";
use "term_xml.ML";
use "General/completion.ML";
use "General/name_space.ML";
use "sorts.ML";
use "type.ML";
use "logic.ML";
use "Syntax/simple_syntax.ML";
use "net.ML";
use "item_net.ML";
use "envir.ML";
use "consts.ML";
use "primitive_defs.ML";
use "defs.ML";
use "sign.ML";
use "term_sharing.ML";
use "pattern.ML";
use "unify.ML";
use "theory.ML";
use "interpretation.ML";
use "proofterm.ML";
use "thm.ML";
use "more_thm.ML";
use "facts.ML";
use "global_theory.ML";
use "pure_thy.ML";
use "drule.ML";
use "morphism.ML";
use "variable.ML";
use "conv.ML";
use "goal_display.ML";
use "tactical.ML";
use "search.ML";
use "tactic.ML";
use "raw_simplifier.ML";
use "conjunction.ML";
use "assumption.ML";
use "display.ML";


(* Isar -- Intelligible Semi-Automated Reasoning *)

(*ML support and global execution*)
use "ML/ml_syntax.ML";
use "ML/ml_env.ML";
use "ML/ml_options.ML";
use "ML/exn_output.ML";
if ML_System.is_polyml then use "ML/exn_output_polyml.ML" else ();
use "ML/ml_options.ML";
use "Isar/runtime.ML";
use "PIDE/execution.ML";
use "ML/ml_compiler.ML";
if ML_System.is_polyml then use "ML/ml_compiler_polyml.ML" else ();

use "skip_proof.ML";
use "goal.ML";

(*proof context*)
use "Isar/object_logic.ML";
use "Isar/rule_cases.ML";
use "Isar/auto_bind.ML";
use "type_infer.ML";
use "Syntax/local_syntax.ML";
use "Isar/proof_context.ML";
use "type_infer_context.ML";
use "Syntax/syntax_phases.ML";
use "Isar/local_defs.ML";

(*outer syntax*)
use "Isar/token.ML";
use "Isar/keyword.ML";
use "Isar/parse.ML";
use "Isar/args.ML";

(*theory sources*)
use "Thy/thy_header.ML";
use "Thy/thy_syntax.ML";
use "Thy/html.ML";
use "Thy/latex.ML";

(*ML with context and antiquotations*)
use "ML/ml_context.ML";
use "ML/ml_antiquotation.ML";

fun use s =
  ML_Context.eval_file (ML_Compiler.verbose true ML_Compiler.flags) (Path.explode s)
    handle ERROR msg => (writeln msg; error "ML error");



(** bootstrap phase 2: towards Pure.thy and final ML toplevel setup *)

(*basic proof engine*)
use "Isar/proof_display.ML";
use "Isar/attrib.ML";
use "Isar/context_rules.ML";
use "Isar/method.ML";
use "Isar/proof.ML";
use "Isar/element.ML";
use "Isar/obtain.ML";

(*local theories and targets*)
use "Isar/locale.ML";
use "Isar/local_theory.ML";
use "Isar/generic_target.ML";
use "Isar/overloading.ML";
use "axclass.ML";
use "Isar/class.ML";
use "Isar/named_target.ML";
use "Isar/expression.ML";
use "Isar/class_declaration.ML";
use "Isar/bundle.ML";

use "simplifier.ML";

(*executable theory content*)
use "Isar/code.ML";

(*specifications*)
use "Isar/parse_spec.ML";
use "Isar/spec_rules.ML";
use "Isar/specification.ML";
use "Isar/typedecl.ML";

(*toplevel transactions*)
use "Isar/proof_node.ML";
use "Isar/toplevel.ML";

(*proof term operations*)
use "Proof/reconstruct.ML";
use "Proof/proof_syntax.ML";
use "Proof/proof_rewrite_rules.ML";
use "Proof/proof_checker.ML";
use "Proof/extraction.ML";

(*theory documents*)
use "System/isabelle_system.ML";
use "Thy/term_style.ML";
use "Thy/thy_output.ML";
use "Isar/outer_syntax.ML";
use "General/graph_display.ML";
use "Thy/present.ML";
use "PIDE/command.ML";
use "PIDE/query_operation.ML";
use "PIDE/resources.ML";
use "Thy/thy_info.ML";
use "PIDE/document.ML";

(*theory and proof operations*)
use "Thy/thm_deps.ML";
use "Isar/isar_cmd.ML";

use "subgoal.ML";


(* Isabelle/Isar system *)

use "PIDE/session.ML";
use "System/command_line.ML";
use "System/system_channel.ML";
use "System/message_channel.ML";
use "System/isabelle_process.ML";
use "System/invoke_scala.ML";
use "PIDE/protocol.ML";
use "System/isar.ML";


(* miscellaneous tools and packages for Pure Isabelle *)

use "Tools/build.ML";
use "Tools/named_thms.ML";
use "Tools/proof_general.ML";

structure Output: OUTPUT = Output;  (*seal system channels!*)


(* ML toplevel pretty printing *)

toplevel_pp ["Pretty", "T"] "(fn _: Pretty.T => Pretty.str \"<pretty>\")";
toplevel_pp ["Task_Queue", "task"] "Pretty.str o Task_Queue.str_of_task";
toplevel_pp ["Task_Queue", "group"] "Pretty.str o Task_Queue.str_of_group";
toplevel_pp ["Position", "T"] "Pretty.position";
toplevel_pp ["Binding", "binding"] "Pretty.str o Binding.print";
toplevel_pp ["Thm", "thm"] "Proof_Display.pp_thm";
toplevel_pp ["Thm", "cterm"] "Proof_Display.pp_cterm";
toplevel_pp ["Thm", "ctyp"] "Proof_Display.pp_ctyp";
toplevel_pp ["Context", "theory"] "Context.pretty_thy";
toplevel_pp ["Context", "Proof", "context"] "Proof_Display.pp_context";
toplevel_pp ["Ast", "ast"] "Ast.pretty_ast";
toplevel_pp ["Path", "T"] "Path.pretty";
toplevel_pp ["SHA1", "digest"] "Pretty.str o quote o SHA1.rep";
toplevel_pp ["Proof", "state"] "(fn _: Proof.state => Pretty.str \"<Proof.state>\")";
toplevel_pp ["Toplevel", "state"] "Toplevel.pretty_abstract";
toplevel_pp ["Morphism", "morphism"] "Morphism.pretty";

if ML_System.is_polyml then use "ML/install_pp_polyml.ML" else ();


(* the Pure theory *)

use "pure_syn.ML";
Runtime.toplevel_program (fn () => Thy_Info.use_thy ("Pure", Position.none));
Context.set_thread_data NONE;
structure Pure = struct val thy = Thy_Info.get_theory "Pure" end;

toplevel_pp ["typ"] "Proof_Display.pp_typ Pure.thy";


(* ML toplevel commands *)

fun use_thys args =
  Runtime.toplevel_program (fn () => Thy_Info.use_thys (map (rpair Position.none) args));
val use_thy = use_thys o single;

val cd = File.cd o Path.explode;

Proofterm.proofs := 0;

