(*  Title:      Pure/ML/ml_file.ML
    Author:     Makarius

The 'ML_file' command.
*)

structure ML_File: sig end =
struct

val _ =
  Outer_Syntax.command ("ML_file", @{here}) "ML text from file"
    (Resources.parse_files "ML_file" >> (fn files => Toplevel.generic_theory (fn gthy =>
        let
          val [{src_path, lines, digest, pos}] = files (Context.theory_of gthy);
          val provide = Resources.provide (src_path, digest);
          val source = Input.source true (cat_lines lines) (pos, pos);
          val flags = {SML = false, exchange = false, redirect = true, verbose = true};
        in
          gthy
          |> ML_Context.exec (fn () => ML_Context.eval_source flags source)
          |> Local_Theory.propagate_ml_env
          |> Context.mapping provide (Local_Theory.background_theory provide)
        end)));

end;
