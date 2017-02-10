chapter "Isabelle/Pure bootstrap";

ML_file "ML/ml_name_space.ML";


section "Bootstrap phase 0: Poly/ML setup";

ML_file "ML/ml_pervasive.ML";
ML_file "ML/ml_system.ML";
ML_file "System/distribution.ML";

ML_file "General/basics.ML";
ML_file "General/symbol_explode.ML";

ML_file "Concurrent/multithreading.ML";
ML_file "Concurrent/unsynchronized.ML";
ML_file "Concurrent/synchronized.ML";
ML_file "Concurrent/counter.ML";

ML_file "ML/ml_heap.ML";
ML_file "ML/ml_profiling.ML";
ML_file "ML/ml_print_depth0.ML";
ML_file "ML/ml_pretty.ML";
ML_file "ML/ml_compiler0.ML";


section "Bootstrap phase 1: towards ML within position context";

subsection "Library of general tools";

ML_file "library.ML";
ML_file "General/print_mode.ML";
ML_file "General/alist.ML";
ML_file "General/table.ML";

ML_file "General/random.ML";
ML_file "General/value.ML";
ML_file "General/properties.ML";
ML_file "General/output.ML";
ML_file "PIDE/markup.ML";
ML_file "General/scan.ML";
ML_file "General/source.ML";
ML_file "General/symbol.ML";
ML_file "General/position.ML";
ML_file "General/symbol_pos.ML";
ML_file "General/input.ML";
ML_file "General/antiquote.ML";
ML_file "ML/ml_lex.ML";
ML_file "ML/ml_compiler1.ML";


section "Bootstrap phase 2: towards ML within Isar context";

subsection "Library of general tools";

ML_file "General/integer.ML";
ML_file "General/stack.ML";
ML_file "General/queue.ML";
ML_file "General/heap.ML";
ML_file "General/same.ML";
ML_file "General/ord_list.ML";
ML_file "General/balanced_tree.ML";
ML_file "General/linear_set.ML";
ML_file "General/buffer.ML";
ML_file "General/pretty.ML";
ML_file "General/rat.ML";
ML_file "PIDE/xml.ML";
ML_file "General/path.ML";
ML_file "General/url.ML";
ML_file "System/bash_syntax.ML";
ML_file "General/file.ML";
ML_file "General/long_name.ML";
ML_file "General/binding.ML";
ML_file "General/socket_io.ML";
ML_file "General/seq.ML";
ML_file "General/timing.ML";
ML_file "General/sha1.ML";

ML_file "PIDE/yxml.ML";
ML_file "PIDE/document_id.ML";

ML_file "General/change_table.ML";
ML_file "General/graph.ML";


subsection "Fundamental structures";

ML_file "name.ML";
ML_file "term.ML";
ML_file "context.ML";
ML_file "context_position.ML";
ML_file "System/options.ML";
ML_file "config.ML";


subsection "Concurrency within the ML runtime";

ML_file "ML/exn_properties.ML";

ML_file "ML/ml_statistics.ML";

ML_file "Concurrent/thread_data_virtual.ML";
ML_file "Concurrent/standard_thread.ML";
ML_file "Concurrent/single_assignment.ML";

ML_file "Concurrent/par_exn.ML";
ML_file "Concurrent/task_queue.ML";
ML_file "Concurrent/future.ML";
ML_file "Concurrent/event_timer.ML";
ML_file "Concurrent/timeout.ML";
ML_file "Concurrent/lazy.ML";
ML_file "Concurrent/par_list.ML";

ML_file "Concurrent/mailbox.ML";
ML_file "Concurrent/cache.ML";

ML_file "PIDE/active.ML";


subsection "Inner syntax";

ML_file "Syntax/type_annotation.ML";
ML_file "Syntax/term_position.ML";
ML_file "Syntax/lexicon.ML";
ML_file "Syntax/ast.ML";
ML_file "Syntax/syntax_ext.ML";
ML_file "Syntax/parser.ML";
ML_file "Syntax/syntax_trans.ML";
ML_file "Syntax/mixfix.ML";
ML_file "Syntax/printer.ML";
ML_file "Syntax/syntax.ML";


subsection "Core of tactical proof system";

ML_file "term_ord.ML";
ML_file "term_subst.ML";
ML_file "term_xml.ML";
ML_file "General/completion.ML";
ML_file "General/name_space.ML";
ML_file "sorts.ML";
ML_file "type.ML";
ML_file "logic.ML";
ML_file "Syntax/simple_syntax.ML";
ML_file "net.ML";
ML_file "item_net.ML";
ML_file "envir.ML";
ML_file "consts.ML";
ML_file "primitive_defs.ML";
ML_file "sign.ML";
ML_file "defs.ML";
ML_file "term_sharing.ML";
ML_file "pattern.ML";
ML_file "unify.ML";
ML_file "theory.ML";
ML_file "proofterm.ML";
ML_file "thm.ML";
ML_file "more_pattern.ML";
ML_file "more_unify.ML";
ML_file "more_thm.ML";
ML_file "facts.ML";
ML_file "global_theory.ML";
ML_file "pure_thy.ML";
ML_file "drule.ML";
ML_file "morphism.ML";
ML_file "variable.ML";
ML_file "conv.ML";
ML_file "goal_display.ML";
ML_file "tactical.ML";
ML_file "search.ML";
ML_file "tactic.ML";
ML_file "raw_simplifier.ML";
ML_file "conjunction.ML";
ML_file "assumption.ML";


subsection "Isar -- Intelligible Semi-Automated Reasoning";

(*ML support and global execution*)
ML_file "ML/ml_syntax.ML";
ML_file "ML/ml_env.ML";
ML_file "ML/ml_options.ML";
ML_file "ML/ml_print_depth.ML";
ML_file_no_debug "ML/exn_debugger.ML";
ML_file_no_debug "Isar/runtime.ML";
ML_file "PIDE/execution.ML";
ML_file "ML/ml_compiler.ML";

ML_file "skip_proof.ML";
ML_file "goal.ML";

(*outer syntax*)
ML_file "Isar/keyword.ML";
ML_file "Isar/token.ML";
ML_file "Isar/parse.ML";
ML_file "Thy/thy_header.ML";

(*proof context*)
ML_file "Isar/object_logic.ML";
ML_file "Isar/rule_cases.ML";
ML_file "Isar/auto_bind.ML";
ML_file "type_infer.ML";
ML_file "Syntax/local_syntax.ML";
ML_file "Isar/proof_context.ML";
ML_file "type_infer_context.ML";
ML_file "Syntax/syntax_phases.ML";
ML_file "Isar/args.ML";

(*theory specifications*)
ML_file "Isar/local_defs.ML";
ML_file "Isar/local_theory.ML";
ML_file "Isar/entity.ML";
ML_file "PIDE/command_span.ML";
ML_file "Thy/thy_syntax.ML";
ML_file "Thy/markdown.ML";
ML_file "Thy/html.ML";
ML_file "Thy/latex.ML";

(*ML with context and antiquotations*)
ML_file "ML/ml_context.ML";
ML_file "ML/ml_antiquotation.ML";
ML_file "ML/ml_compiler2.ML";


section "Bootstrap phase 3: towards theory Pure and final ML toplevel setup";

(*basic proof engine*)
ML_file "par_tactical.ML";
ML_file "Isar/proof_display.ML";
ML_file "Isar/attrib.ML";
ML_file "Isar/context_rules.ML";
ML_file "Isar/method.ML";
ML_file "Isar/proof.ML";
ML_file "Isar/element.ML";
ML_file "Isar/obtain.ML";
ML_file "Isar/subgoal.ML";
ML_file "Isar/calculation.ML";

(*local theories and targets*)
ML_file "Isar/locale.ML";
ML_file "Isar/generic_target.ML";
ML_file "Isar/overloading.ML";
ML_file "axclass.ML";
ML_file "Isar/class.ML";
ML_file "Isar/named_target.ML";
ML_file "Isar/expression.ML";
ML_file "Isar/interpretation.ML";
ML_file "Isar/class_declaration.ML";
ML_file "Isar/bundle.ML";
ML_file "Isar/experiment.ML";

ML_file "simplifier.ML";
ML_file "Tools/plugin.ML";

(*executable theory content*)
ML_file "Isar/code.ML";

(*specifications*)
ML_file "Isar/spec_rules.ML";
ML_file "Isar/specification.ML";
ML_file "Isar/parse_spec.ML";
ML_file "Isar/typedecl.ML";

(*toplevel transactions*)
ML_file "Isar/proof_node.ML";
ML_file "Isar/toplevel.ML";

(*proof term operations*)
ML_file "Proof/reconstruct.ML";
ML_file "Proof/proof_syntax.ML";
ML_file "Proof/proof_rewrite_rules.ML";
ML_file "Proof/proof_checker.ML";
ML_file "Proof/extraction.ML";

(*Isabelle system*)
ML_file "System/bash.ML";
ML_file "System/isabelle_system.ML";


(*theory documents*)
ML_file "Thy/term_style.ML";
ML_file "Isar/outer_syntax.ML";
ML_file "Thy/thy_output.ML";
ML_file "Thy/document_antiquotations.ML";
ML_file "General/graph_display.ML";
ML_file "Thy/present.ML";
ML_file "pure_syn.ML";
ML_file "PIDE/command.ML";
ML_file "PIDE/query_operation.ML";
ML_file "PIDE/resources.ML";
ML_file "Thy/thy_info.ML";
ML_file "PIDE/session.ML";
ML_file "PIDE/protocol_message.ML";
ML_file "PIDE/document.ML";

(*theory and proof operations*)
ML_file "Isar/isar_cmd.ML";


subsection "Isabelle/Isar system";

ML_file "System/command_line.ML";
ML_file "System/system_channel.ML";
ML_file "System/message_channel.ML";
ML_file "System/isabelle_process.ML";
ML_file "System/invoke_scala.ML";
ML_file "PIDE/protocol.ML";
ML_file "General/output_primitives_virtual.ML";


subsection "Miscellaneous tools and packages for Pure Isabelle";

ML_file "ML/ml_pp.ML";
ML_file "ML/ml_antiquotations.ML";
ML_file "ML/ml_thms.ML";
ML_file "ML/ml_file.ML";

ML_file "Tools/build.ML";
ML_file "Tools/named_thms.ML";
ML_file "Tools/print_operation.ML";
ML_file "Tools/bibtex.ML";
ML_file "Tools/rail.ML";
ML_file "Tools/rule_insts.ML";
ML_file "Tools/thm_deps.ML";
ML_file "Tools/thy_deps.ML";
ML_file "Tools/class_deps.ML";
ML_file "Tools/find_theorems.ML";
ML_file "Tools/find_consts.ML";
ML_file "Tools/simplifier_trace.ML";
ML_file_no_debug "Tools/debugger.ML";
ML_file "Tools/named_theorems.ML";
ML_file "Tools/jedit.ML";
