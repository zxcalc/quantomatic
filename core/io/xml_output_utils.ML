signature XML_OUTPUT_UTILS =
sig
  val wrap_text_simple : string -> string -> XML.tree;
  val wrap_bool_simple : string -> bool -> XML.tree;
  val wrap_int_simple : string -> int -> XML.tree;
  val wrap_el_simple : string -> XML.tree -> XML.tree;
  val wrap_els_simple : string -> XML.tree list -> XML.tree;
  val output_symtab_entries : string Symtab.table -> XML.tree list;
  val output_symtab : string -> string Symtab.table -> XML.tree;
end;

structure XML_Output_Utils : XML_OUTPUT_UTILS
= struct
  open XML;

  fun wrap_els_simple name elem = (Elem ((name, []), elem));

  fun wrap_el_simple name elem = wrap_els_simple name [elem];

  fun wrap_text_simple name text = wrap_el_simple name (Text text);

  fun wrap_bool_simple name true = wrap_text_simple name "true"
    | wrap_bool_simple name false = wrap_text_simple name "false";

  fun wrap_int_simple name i = wrap_text_simple name (Int.toString i);

  fun output_symtab_entries tab = let
    fun output_entry (k, s) = Elem (("entry", [("name", k)]), [Text s])
  in
    map output_entry (Symtab.dest tab)
  end;

  fun output_symtab tag = (wrap_els_simple tag) o output_symtab_entries;
end;

