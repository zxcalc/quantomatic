(* BASIC INPUT/OUTPUT *)
signature INPUT =
sig
  type T
  type data
  val input : T -> data
  exception bad_input_exp of string;
end;
signature INPUT_XML = INPUT where type T = XML.tree;

structure InputBoolXML : INPUT_XML 
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

structure InputIntXML : INPUT_XML 
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


functor InputSStrNameXML(N : SSTR_NAMES) : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = N.name;
  exception bad_input_exp of string;
  fun input xml = N.mk (XML_Parse_Utils.get_text xml);
end;


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

(* INPUT/OUTPUT of lists *)
functor InputListXML (
  val tagname : string 
  structure Input1 : INPUT_XML
) : INPUT_XML
= struct
  type T = XML.tree;
  type data = Input1.data list;
  open XML_Parse_Utils;
  fun input xml = 
      let val () = XML_Parse_Utils.assert_tag tagname xml
      in map Input1.input (XML_Parse_Utils.get_children xml) end;
end;



(* INPUT/OUTPUT of Binary relations between named things *)
functor InputNameBRelXML (
  val tagname : string 
  structure BRel : NAME_BINREL
  structure InputDom : INPUT_XML 
  structure InputCod : INPUT_XML
  sharing type BRel.Dom.name = InputDom.data 
  sharing type BRel.Cod.name = InputCod.data 
) : INPUT_XML
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
              (fold (BRel.Dom.NSet.add o InputDom.input) 
                    dom_els BRel.Dom.NSet.empty)
              (fold (BRel.Cod.NSet.add o InputCod.input) 
                    cod_els BRel.Cod.NSet.empty)
           end)
          rel_els
          BRel.empty 
      end;
end;


(* INPUT/OUTPUT of Binary relations between named things *)
functor InputNTabXML (
  val tagname : string
  structure NTab : NAME_TAB
  structure InputName : INPUT_XML 
  structure InputValue : INPUT_XML
  sharing type NTab.name = InputName.data 
) : INPUT_XML
= struct
  type T = XML.tree;
  type data = InputValue.data NTab.T;
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
           in NTab.ins (InputName.input (get_the_child name_el),
                        InputValue.input (get_the_child val_el)) end)
          els
          NTab.empty 
      end;
end;


