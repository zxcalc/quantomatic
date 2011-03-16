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


functor InputUseChildXML(In : INPUT_XML) : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = In.data;
  exception bad_input_exp of string*string;
  val data_to_string = In.data_to_string;
  open XML;
  fun input (Text _) = raise bad_input_exp ("Can't get unique child of text","")
    | input (Elem ((elem, attr),[child])) = ((In.input child)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,elem^"/"^p))
    | input (Elem ((elem, attr),_)) = raise bad_input_exp ("<"^elem^"> does not have unique child","");
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

(* INPUT of lists
 *
 * A list is an element <tagname>, containing <item_tagname> elements.
 *
 * Input1 will be passed each item element to parse, and the result
 * will be a list of parsed elements.
 *
 * For example, if the list looks like
 * <ul><li>...</li><li>...</li></ul>
 * Input1.input will be called on each <li> element.
 *)
functor InputListXML (
  val tagname : string 
  val item_tagname : string 
  structure Input1 : INPUT_XML
) : INPUT_XML
= struct
  type T = XML.tree;
  type data = Input1.data list;
  open XML_Parse_Utils;
  fun data_to_string xs = fold (fn item => fn str => ((Input1.data_to_string item)^str)) xs "";
  fun input xml = 
      (let val () = XML_Parse_Utils.assert_tag tagname xml
           val els = filter_elems item_tagname (get_children xml)
       in map Input1.input els end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
end;



(* INPUT of binary relations between named things
 *
 * A binary relation is an element <tagname>, containing <rel_tagname> elements.
 * Each <rel_tagname> element contains any number of <cod_tagname> and
 * <dom_tagname> elements.
 *
 * InputDom will be passed each <dom_tagname> element to parse, and
 * similarly InputCod will be passed each <cod_tagname> element to parse.
 * For each <rel_tagname> element, the cross product of the sets defined by
 * the <cod_tagname> and <dom_tagname> elements is constructed, and the
 * result is the union of these cross products.
 *)
functor InputNameBRelXML (
  val tagname : string 
  val rel_tagname : string 
  val cod_tagname : string 
  val dom_tagname : string 
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
           val rel_els = filter_elems rel_tagname (get_children xml)
       in 
         fold 
           (fn rel_el => 
            let 
              val children = get_children rel_el;
              val dom_els = filter_elems dom_tagname children;
              val cod_els = filter_elems cod_tagname children;
            in
              BRel.add_many 
               (fold (BRel.Dom.NSet.add o InputDom.input) 
                     dom_els BRel.Dom.NSet.empty)
               (fold (BRel.Cod.NSet.add o InputCod.input) 
                     cod_els BRel.Cod.NSet.empty)
            end)
           rel_els
           BRel.empty 
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
end;


(* INPUT of named tables
 *
 * A named table is an element <tagname>, containing <entry_tagname> elements.
 * Each <entry_tagname> element contains one <key_tagname> and one
 * <value_tagname> element.
 *
 * For each <entry_tagname> element, InputName will be passed the
 * <key_tagname> element to parse, and similarly InputValue will be passed the
 * <value_tagname> element to parse.
 *)
functor InputNTabXML (
  val tagname : string
  val entry_tagname : string 
  val key_tagname : string 
  val value_tagname : string 
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
           val els = filter_elems entry_tagname (get_children xml)
       in 
         fold 
           (fn el => 
            let 
              val children = get_children el
              val name = (get_the_elem_processed InputName.input key_tagname children)
                handle bad_input_exp (s,p) => raise bad_input_exp (s,entry_tagname^"/"^p)
              val value = (get_the_elem_processed InputValue.input value_tagname children)
                handle bad_input_exp (s,p) => raise bad_input_exp (s,entry_tagname^" ("^(InputName.data_to_string name)^")/"^p)
            in NTab.ins (name, value) end)
           els
           NTab.empty 
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
end;


(* vim:et:sts=2:sw=2
*)
