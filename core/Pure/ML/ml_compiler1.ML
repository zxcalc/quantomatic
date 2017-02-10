(*  Title:      Pure/ML/ml_compiler1.ML
    Author:     Makarius

Refined ML file operations for bootstrap.
*)

val {ML_file, ML_file_debug, ML_file_no_debug} =
  let
    val context: ML_Compiler0.context =
     {name_space = ML_Name_Space.global,
      print_depth = NONE,
      here = Position.here oo Position.line_file,
      print = writeln,
      error = error};
  in
    ML_Compiler0.ML_file_operations (fn opt_debug => fn file =>
      Position.setmp_thread_data (Position.file_only file)
        (fn () =>
          ML_Compiler0.ML_file context
            {verbose = true, debug = ML_Compiler0.debug_option opt_debug} file
          handle ERROR msg => (writeln msg; error "ML error")) ())
  end;
