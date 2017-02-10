(*  Title:      Pure/pure_syn.ML
    Author:     Makarius

Outer syntax for bootstrapping: commands that are accessible outside a
regular theory context.
*)

signature PURE_SYN =
sig
  val bootstrap_thy: theory
end;

structure Pure_Syn: PURE_SYN =
struct

val semi = Scan.option (Parse.$$$ ";");

val _ =
  Outer_Syntax.command ("chapter", @{here}) "chapter heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("section", @{here}) "section heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("subsection", @{here}) "subsection heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("subsubsection", @{here}) "subsubsection heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("paragraph", @{here}) "paragraph heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("subparagraph", @{here}) "subparagraph heading"
    (Parse.opt_target -- Parse.document_source --| semi
      >> Thy_Output.document_command {markdown = false});

val _ =
  Outer_Syntax.command ("text", @{here}) "formal comment (primary style)"
    (Parse.opt_target -- Parse.document_source >> Thy_Output.document_command {markdown = true});

val _ =
  Outer_Syntax.command ("txt", @{here}) "formal comment (secondary style)"
    (Parse.opt_target -- Parse.document_source >> Thy_Output.document_command {markdown = true});

val _ =
  Outer_Syntax.command ("text_raw", @{here}) "LaTeX text (without surrounding environment)"
    (Parse.opt_target -- Parse.document_source >> Thy_Output.document_command {markdown = true});

val _ =
  Outer_Syntax.command ("theory", @{here}) "begin theory"
    (Thy_Header.args >>
      (fn _ => Toplevel.init_theory (fn () => error "Missing theory initialization")));


val bootstrap_thy = Context.the_global_context ();

val _ = Theory.setup (Config.put_global Outer_Syntax.bootstrap false);

end;
