(* BASIC INPUT/OUTPUT *)
signature INPUT =
sig
  type T
  type data
  val data_to_string : data -> string
  val input : T -> data
  exception bad_input_exp of string*string; (* message, location *)
end;
signature INPUT_XML = INPUT where type T = XML.tree;

structure InputBoolXML : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = bool;
  exception bad_input_exp of string*string;

  fun data_to_string true = "true"
    | data_to_string false = "false";
  fun input xml = XML_Parse_Utils.get_boolean xml;
end;

structure InputIntXML : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = int;
  exception bad_input_exp of string*string;

  fun data_to_string i = Int.toString i;
  fun input xml = XML_Parse_Utils.get_int xml;
end;


functor InputSStrNameXML(N : SSTR_NAMES) : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = N.name;
  exception bad_input_exp of string*string;
  fun data_to_string s = N.dest s;
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
  exception bad_input_exp of string*string;
  val data_to_string = In2.data_to_string;
  fun input i = 
      (In2.input 
        ((In1.input i) 
         handle In1.bad_input_exp (s,p) => raise bad_input_exp (s,p)))
      handle In2.bad_input_exp (s,p) => raise bad_input_exp (s,p);
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
  fun data_to_string xs = fold (fn item => fn str => ((Input1.data_to_string item)^str)) xs "";
  fun input xml = 
      (let val () = XML_Parse_Utils.assert_tag tagname xml
           val els = filter_elems "item" (get_children xml)
       in map (Input1.input o get_the_child) els end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
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
  fun data_to_string _ = tagname;
  fun input xml = 
      (let val () = assert_tag tagname xml;
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
               (fold (BRel.Dom.NSet.add o InputDom.input o get_the_child) 
                     dom_els BRel.Dom.NSet.empty)
               (fold (BRel.Cod.NSet.add o InputCod.input o get_the_child) 
                     cod_els BRel.Cod.NSet.empty)
            end)
           rel_els
           BRel.empty 
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
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
  fun data_to_string _ = tagname;
  fun input xml = 
      (let val () = assert_tag tagname xml
           val els = filter_elems "map" (get_children xml)
       in 
         fold 
           (fn el => 
            let 
              val children = get_children el
              val name = (get_the_elem_processed (InputName.input o get_the_child) "name" children)
                handle bad_input_exp (s,p) => raise bad_input_exp (s,"map/"^p)
              val value = (get_the_elem_processed (InputValue.input o get_the_child_ignore_wsp) "val" children)
                handle bad_input_exp (s,p) => raise bad_input_exp (s,"map ("^(InputName.data_to_string name)^")/"^p)
            in NTab.ins (name, value) end)
           els
           NTab.empty 
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
end;


(* vim:et:sts=2:sw=2
*)
