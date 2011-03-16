signature OUTPUT =
sig
  type data
  type T
  val output : data -> T
end;
signature PRETTY_XML = OUTPUT where type T = XML.tree;
signature OUTPUT_PRETTY_XML = OUTPUT where type T = Pretty.T;


functor OutputSStrNameXML(N : SSTR_NAMES) : OUTPUT_PRETTY_XML 
= struct 
  type T = Pretty.T;
  type data = N.name;
  val output = N.pretty_name;
end;

(* composition *)
functor ComposeOutputs (
  structure Out1 : OUTPUT
  structure Out2 : OUTPUT
  sharing type Out2.data = Out1.T)
: OUTPUT
= struct
  type T = Out2.T
  type data = Out1.data
  val output = Out2.output o Out1.output;
end;


functor OutputWrapXML (
  val tagname : string
  val attrs : (string*string) list
  structure Output1 : OUTPUT_PRETTY_XML
) : OUTPUT_PRETTY_XML
=
struct
  type data = Output1.data
  type T = Pretty.T

  fun output x = Pretty.chunks [Pretty.str (XML_Output_Utils.start_tag tagname attrs),
                                Pretty.indent 2 (Output1.output x),
                                Pretty.str (XML_Output_Utils.end_tag tagname)];
end;


functor OutputListXML (
  val tagname : string
  val item_tagname : string 
  structure Output1 : OUTPUT_PRETTY_XML
) : OUTPUT_PRETTY_XML
= 
struct
  type data = Output1.data list 
  type T = Pretty.T

  val wrap_item = XML_Output_Utils.pretty_item item_tagname [];
  fun output xs = XML_Output_Utils.pretty_list tagname [] (map (wrap_item o Output1.output) xs);
end;

functor OutputNameBRelXML (
  val tagname : string 
  val rel_tagname : string 
  val cod_tagname : string 
  val dom_tagname : string 
  structure BRel : NAME_BINREL
  structure OutputDom : OUTPUT_PRETTY_XML 
  structure OutputCod : OUTPUT_PRETTY_XML
  sharing type BRel.Dom.name = OutputDom.data 
  sharing type BRel.Cod.name = OutputCod.data 
) : OUTPUT_PRETTY_XML
= struct
  open XML_Parse_Utils;

  type data = BRel.T;
  type T = Pretty.T;

  fun output brel = 
      XML_Output_Utils.pretty_list tagname [] (
        map (fn (codn,domnset) =>
               XML_Output_Utils.pretty_list rel_tagname [] (
                 (XML_Output_Utils.pretty_item cod_tagname [] (OutputCod.output codn))
                 ::(map (fn domn => XML_Output_Utils.pretty_item dom_tagname [] (OutputDom.output domn))
                        (BRel.Dom.NSet.list_of domnset)
                   )
               )
            )
            (BRel.Cod.NTab.list_of (BRel.get_codtab brel))
      );
end;

functor OutputNTabXML (
  val tagname : string 
  val entry_tagname : string 
  val key_tagname : string 
  val value_tagname : string 
  structure NTab : NAME_TAB
  structure OutputName : OUTPUT_PRETTY_XML 
  structure OutputVal : OUTPUT_PRETTY_XML
  sharing type NTab.name = OutputName.data 
) : OUTPUT_PRETTY_XML
= struct
  type data = OutputVal.data NTab.T;
  type T = Pretty.T;
  open XML_Output_Utils;
  fun output ntab = 
      pretty_list tagname [] (
        map (fn (n,v) =>
               pretty_list entry_tagname [] (
                 [pretty_item key_tagname [] (OutputName.output n),
                  pretty_item value_tagname [] (OutputVal.output v)]
               )
            )
            (NTab.list_of ntab)
      );
end;

(* vim:et:sts=2:sw=2
*)
