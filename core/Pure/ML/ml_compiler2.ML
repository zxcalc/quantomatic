(*  Title:      Pure/ML/ml_compiler2.ML
    Author:     Makarius

Isabelle/ML file operations.
*)

val {ML_file, ML_file_debug, ML_file_no_debug} =
  ML_Compiler0.ML_file_operations (fn opt_debug => fn file =>
    let val flags = ML_Compiler.verbose true (ML_Compiler.debug_flags opt_debug) in
      ML_Context.eval_file flags (Path.explode file)
        handle ERROR msg => (writeln msg; error "ML error")
    end);

val use = ML_file;
