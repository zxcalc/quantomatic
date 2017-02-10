(*  Title:      Pure/Tools/bibtex.ML
    Author:     Makarius

BibTeX support.
*)

signature BIBTEX =
sig
  val cite_macro: string Config.T
end;

structure Bibtex: BIBTEX =
struct

val cite_macro = Attrib.setup_config_string @{binding cite_macro} (K "cite");

val _ =
  Theory.setup
   (Thy_Output.add_option @{binding cite_macro} (Config.put cite_macro) #>
    Thy_Output.antiquotation @{binding cite}
      (Scan.lift
        (Scan.option (Parse.verbatim || Parse.cartouche) --
         Parse.and_list1 (Parse.position Args.name)))
      (fn {context = ctxt, ...} => fn (opt, citations) =>
        let
          val _ =
            Context_Position.reports ctxt
              (map (fn (name, pos) => (pos, Markup.citation name)) citations);
          val opt_arg = (case opt of NONE => "" | SOME s => "[" ^ s ^ "]");
          val arg = "{" ^ space_implode "," (map #1 citations) ^ "}";
        in "\\" ^ Config.get ctxt cite_macro ^ opt_arg ^ arg end));

end;

