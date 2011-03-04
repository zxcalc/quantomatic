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


functor OutputListXML (
  val tagname : string
	structure Output1 : OUTPUT_PRETTY_XML
) : OUTPUT_PRETTY_XML
= 
struct
  type data = Output1.data list 
  type T = Pretty.T

  fun output rewrites = 
      Pretty.chunks [ Pretty.str ("<"^tagname^">"), 
		      Pretty.indent 2 (Pretty.block (map Output1.output rewrites)),
		      Pretty.str ("</"^tagname^">"),
		      Pretty.str ""
		    ];
end;

functor OutputNameBRelXML (
  val tagname : string 
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
      Pretty.block 
       [Pretty.str ("<"^tagname^">"),
        Pretty.indent 2 
          (Pretty.chunks 
            (map 
             (fn (codn,domnset) =>
              Pretty.chunks 
               [Pretty.str "<rel>", 
                (Pretty.indent 2 o Pretty.block)
                  (map (fn domn => Pretty.enclose "<dom>" "</dom>" 
                                   [OutputDom.output domn])
                       (BRel.Dom.NSet.list_of domnset)),
                Pretty.enclose "<cod>" "</cod>" [OutputCod.output codn],
                Pretty.str "</rel>"])
             (BRel.Cod.NTab.list_of (BRel.get_codtab brel)))),
        Pretty.str ("</"^tagname^">")];
end;

functor OutputNTabXML (
  val tagname : string 
  structure NTab : NAME_TAB
  structure OutputName : OUTPUT_PRETTY_XML 
  structure OutputVal : OUTPUT_PRETTY_XML
  sharing type NTab.name = OutputName.data 
) : OUTPUT_PRETTY_XML
= struct
  type data = OutputVal.data NTab.T;
  type T = Pretty.T;
  open XML_Parse_Utils;
  fun output ntab = 
      Pretty.block 
       [Pretty.str ("<"^tagname^">"),
        Pretty.indent 2 
          (Pretty.chunks 
            (map 
             (fn (n,v) =>
              Pretty.chunks 
               [Pretty.str "<map>", 
                (Pretty.indent 2 o Pretty.block)
                  [Pretty.enclose "<name>" "</name>" [OutputName.output n],
                   Pretty.enclose "<val>" "</val>" [OutputVal.output v]],
                Pretty.str "</map>"])
             (NTab.list_of ntab))),
        Pretty.str ("</"^tagname^">")];
end;
