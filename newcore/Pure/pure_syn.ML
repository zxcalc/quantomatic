(*  Title:      Pure/pure_syn.ML
    Author:     Makarius

Minimal outer syntax for bootstrapping Isabelle/Pure.
*)

structure Pure_Syn: sig end =
struct

val _ =
  Outer_Syntax.command
    (("theory", Keyword.tag_theory Keyword.thy_begin), @{here}) "begin theory"
    (Thy_Header.args >> (fn header =>
      Toplevel.init_theory
        (fn () => Thy_Info.toplevel_begin_theory (! ProofGeneral.master_path) header)));

val _ =
  Outer_Syntax.command
    (("ML_file", Keyword.tag_ml Keyword.thy_load), @{here}) "ML text from file"
    (Resources.parse_files "ML_file" >> (fn files => Toplevel.generic_theory (fn gthy =>
        let
          val [{src_path, lines, digest, pos}] = files (Context.theory_of gthy);
          val provide = Resources.provide (src_path, digest);
          val source = {delimited = true, text = cat_lines lines, pos = pos};
          val flags = {SML = false, exchange = false, redirect = true, verbose = true};
        in
          gthy
          |> ML_Context.exec (fn () => ML_Context.eval_source flags source)
          |> Local_Theory.propagate_ml_env
          |> Context.mapping provide (Local_Theory.background_theory provide)
        end)));

end;

