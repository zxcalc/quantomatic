signature OUTPUT_XML = OUTPUT where type T = XML.tree;
signature OUTPUT_XML_LIST = OUTPUT where type T = XML.tree list;

structure OutputStringXML : OUTPUT_XML where type data = string 
= struct 
  type T = XML.tree;
  type data = string;

  open XML;
  open XML_Output_Utils;
  
  fun output (s) = 
    wrap_text_simple "string_of" (s);
end; (* structure *)

structure OutputBoolXML : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = bool;

  open XML;
  fun output true = Text "true"
    | output false = Text "false"
end;

structure OutputIntXML : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = int;

  open XML;
  fun output i = Text (Int.toString i)
end;

functor OutputSStrNameXML(N : SSTR_NAME) : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = N.name;
  open XML;
  fun output s = (Text (N.string_of_name s));
end;


functor OutputWrapXML (
  val tagname : string
  val attrs : (string*string) list
  structure Output1 : OUTPUT_XML_LIST
) : OUTPUT_XML
=
struct
  type data = Output1.data
  type T = XML.tree

  open XML;
  fun output x = Elem ((tagname, attrs), Output1.output x)
end;


functor OutputListXMLList (
  val item_tagname : string 
  structure Output1 : OUTPUT_XML_LIST
) : OUTPUT_XML_LIST
= 
struct
  type data = Output1.data list 
  type T = XML.tree list
  val output = map ((XML_Output_Utils.wrap_els_simple item_tagname) o Output1.output);
end;

functor OutputNameBRelXMLList (
  val rel_tagname : string 
  val cod_tagname : string 
  val dom_tagname : string 
  structure BRel : NAME_BINREL
  structure OutputDom : OUTPUT_XML_LIST
  structure OutputCod : OUTPUT_XML_LIST
  sharing type BRel.Dom.name = OutputDom.data 
  sharing type BRel.Cod.name = OutputCod.data 
) : OUTPUT_XML_LIST
= struct
  type data = BRel.T;
  type T = XML.tree list;

  open XML;
  structure Util = XML_Output_Utils;
  fun output brel =
      map (fn (codn,domnset) =>
           Util.wrap_els_simple rel_tagname (
                 (Util.wrap_els_simple cod_tagname (OutputCod.output codn))
                 ::(map ((Util.wrap_els_simple dom_tagname) o OutputDom.output) (BRel.Dom.NSet.list_of domnset))
           )
          )
          (BRel.Cod.NTab.list_of (BRel.get_codtab brel));
end;

functor OutputNTabXMLList (
  val entry_tagname : string 
  val key_tagname : string 
  val value_tagname : string 
  structure NTab : NAME_TAB
  structure OutputName : OUTPUT_XML_LIST
  structure OutputVal : OUTPUT_XML_LIST
  sharing type NTab.name = OutputName.data 
) : OUTPUT_XML_LIST
= struct
  type data = OutputVal.data NTab.T;
  type T = XML.tree list;
  open XML;
  structure Util = XML_Output_Utils;
  fun output ntab = 
      map (fn (n,v) => Util.wrap_els_simple entry_tagname
                         [Util.wrap_els_simple key_tagname (OutputName.output n),
                          Util.wrap_els_simple value_tagname (OutputVal.output v)]
          )
          (NTab.list_of ntab);
end;

