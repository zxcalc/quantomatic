(* BASIC INPUT/OUTPUT *)
signature INPUT =
sig
  type T
  type data
  val input : T -> data
  exception bad_input_exp of string;
end;
signature XML_INPUT = INPUT where type T = XML.tree;

structure InputBoolXML : XML_INPUT 
= struct 
  type T = XML.tree;
  type data = bool;
  exception bad_input_exp of string;

  fun input xml = 
      let val t = String.map Char.toLower (XML_Parse_Utils.get_text xml)
      in
        if (t = "true") then true
        else if (t = "false") then false
        else raise bad_input_exp ("Expected 'true' or 'false' but found '"^t^"'.")
      end;
end;

structure InputIntXML : XML_INPUT 
= struct 
  type T = XML.tree;
  type data = int;
  exception bad_input_exp of string;

  fun input xml = 
      let val s = (XML_Parse_Utils.get_text xml) in
      case Int.fromString s of 
        SOME i => i 
      | NONE => raise bad_input_exp  ("Expected integer but saw '"^s^"'.")
      end;
end;


signature OUTPUT =
sig
  type data
  type T
  val output : data -> T
end;
signature XML_OUTPUT = OUTPUT where type T = XML.tree;
signature PRETTY_XML_OUTPUT = OUTPUT where type T = Pretty.T;


(* composition *)
functor ComposeInputs (
  structure In1 : INPUT
  structure In2 : INPUT
  sharing type In1.data = In2.T)
: INPUT
= struct
  type T = In1.T
  type data = In2.data
  exception bad_input_exp of string;
  fun input i = 
      (In2.input 
        ((In1.input i) 
         handle In1.bad_input_exp s => raise bad_input_exp s))
      handle In2.bad_input_exp s => raise bad_input_exp s;
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


(* INPUT/OUTPUT of lists *)
functor InputListXML (
  val tagname : string 
  structure Input1 : XML_INPUT
) : XML_INPUT
= struct
  type T = XML.tree;
  type data = Input1.data list;
  open XML_Parse_Utils;
  fun input xml = 
      let val () = XML_Parse_Utils.assert_tag tagname xml
      in map Input1.input (XML_Parse_Utils.get_children xml) end;
end;

functor OutputListXML (
  val tagname : string
	structure Output1 : PRETTY_XML_OUTPUT
) : PRETTY_XML_OUTPUT
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


(* INPUT/OUTPUT of Binary relations between named things *)
functor InputNameBRelXML (
  val tagname : string 
  structure BRel : NAME_BINREL
  structure InputDomXML : XML_INPUT 
  structure InputCodXML : XML_INPUT
  sharing type BRel.Dom.name = InputDomXML.data 
  sharing type BRel.Cod.name = InputCodXML.data 
) : XML_INPUT
= struct
  type T = XML.tree;
  type data = BRel.T;
  open XML_Parse_Utils;
  fun input xml = 
      let val () = assert_tag tagname xml;
          val rel_els = filter_elems "rel" (get_children xml)
      in 
        fold 
          (fn rel_el => 
           let 
             val children = get_children rel_el;
             val dom_els = filter_elems "dom" children;
             val cod_els = filter_elems "cod" children;
           in
             BRel.add_many 
              (fold (BRel.Dom.NSet.add o InputDomXML.input) 
                    dom_els BRel.Dom.NSet.empty)
              (fold (BRel.Cod.NSet.add o InputCodXML.input) 
                    cod_els BRel.Cod.NSet.empty)
           end)
          rel_els
          BRel.empty 
      end;
end;

functor OutputNameBRelXML (
  val tagname : string 
  structure BRel : NAME_BINREL
  structure OutputDomXML : PRETTY_XML_OUTPUT 
  structure OutputCodXML : PRETTY_XML_OUTPUT
  sharing type BRel.Dom.name = OutputDomXML.data 
  sharing type BRel.Cod.name = OutputCodXML.data 
) : PRETTY_XML_OUTPUT
= struct
  type data = BRel.T;
  type T = Pretty.T;
  open XML_Parse_Utils;
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
                                   [OutputDomXML.output domn])
                       (BRel.Dom.NSet.list_of domnset)),
                Pretty.enclose "<cod>" "</cod>" [OutputCodXML.output codn],
                Pretty.str "</rel>"])
             (BRel.Cod.NTab.list_of (BRel.get_codtab brel)))),
        Pretty.str ("</"^tagname^">")];
end;



(* INPUT/OUTPUT of Binary relations between named things *)
functor InputNTabXML (
  val tagname : string
  structure NTab : NAME_TAB
  structure InputNameXML : XML_INPUT 
  structure InputValueXML : XML_INPUT
  sharing type NTab.name = InputNameXML.data 
) : XML_INPUT
= struct
  type T = XML.tree;
  type data = InputValueXML.data NTab.T;
  open XML_Parse_Utils;
  fun input xml = 
      let val () = assert_tag tagname xml
          val els = filter_elems "map" (get_children xml)
      in 
        fold 
          (fn el => 
           let 
             val children = get_children el
             val name_el = get_the_elem "name" children
             val val_el = get_the_elem "val" children
           in NTab.ins (InputNameXML.input (get_the_child name_el),
                        InputValueXML.input (get_the_child val_el)) end)
          els
          NTab.empty 
      end;
end;

functor OutputNTabXML (
  val tagname : string 
  structure NTab : NAME_TAB
  structure OutputNameXML : PRETTY_XML_OUTPUT 
  structure OutputValXML : PRETTY_XML_OUTPUT
  sharing type NTab.name = OutputNameXML.data 
) : PRETTY_XML_OUTPUT
= struct
  type data = OutputValXML.data NTab.T;
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
                  [Pretty.enclose "<name>" "</name>" [OutputNameXML.output n],
                   Pretty.enclose "<val>" "</val>" [OutputValXML.output v]],
                Pretty.str "</map>"])
             (NTab.list_of ntab))),
        Pretty.str ("</"^tagname^">")];
end;
