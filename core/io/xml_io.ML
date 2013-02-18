signature INPUT_XML = INPUT where type T = XML.tree;
signature OUTPUT_XML = OUTPUT where type T = XML.tree;
signature OUTPUT_XML_LIST = OUTPUT where type T = XML.tree list;

structure InputStringXML : INPUT_XML where type data = string
= struct

  open XML_Parse_Utils; 
  
  type T = XML.tree;  (* XML.tree *)
  type data = string;
  
  fun input xml  =
    (let val s = get_unique_child_text xml ;
     in s end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"string data/"^p);

end;

structure OutputStringXML : OUTPUT_XML where type data = string 
= struct 
  type T = XML.tree;
  type data = string;

  open XML;
  open XML_Output_Utils;
  
  fun output (s) = 
    wrap_text_simple "string_of" (s);
end; (* structure *)



structure InputBoolXML : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = bool;
  exception bad_input_exp of string*string;

  fun input xml = XML_Parse_Utils.get_boolean xml;
end;

structure OutputBoolXML : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = bool;

  open XML;
  fun output true = Text "true"
    | output false = Text "false"
end;




structure InputIntXML : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = int;
  exception bad_input_exp of string*string;

  fun input xml = XML_Parse_Utils.get_int xml;
end;

structure OutputIntXML : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = int;

  open XML;
  fun output i = Text (Int.toString i)
end;



functor InputSStrNameXML(N : SSTR_NAME) : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = N.name;
  exception bad_input_exp of string*string;
  fun input xml = N.mk (XML_Parse_Utils.get_text xml);
end;

functor OutputSStrNameXML(N : SSTR_NAME) : OUTPUT_XML 
= struct 
  type T = XML.tree;
  type data = N.name;
  open XML;
  fun output s = (Text (N.string_of_name s));
end;



functor InputUseChildXML(In : INPUT_XML) : INPUT_XML 
= struct 
  type T = XML.tree;
  type data = In.data;
  exception bad_input_exp of string*string;
  open XML;
  fun input (Elem ((elem, _),[child])) = ((In.input child)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,elem^"/"^p))
    | input (Elem ((elem, _),_)) = raise bad_input_exp ("<"^elem^"> does not have unique child","")
    | input _ = raise bad_input_exp ("Expected element","");
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
  fun input xml = 
      (let val () = XML_Parse_Utils.assert_tag tagname xml
           val els = filter_elems item_tagname (get_children xml)
       in map Input1.input els end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
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
  fun input xml = 
      (let val () = assert_tag tagname xml
           val els = filter_elems entry_tagname (get_children xml)
       in 
         fold 
           (fn el =>
            let
              val children = get_children el
              val name = (apply_to_unique_elem InputName.input key_tagname children)
                handle InputName.bad_input_exp e => raise bad_input_exp e
              val value = (apply_to_unique_elem InputValue.input value_tagname children)
                handle InputValue.bad_input_exp e => raise bad_input_exp e
            in NTab.ins (name, value) end)
           els
           NTab.empty 
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,tagname^"/"^p);
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

structure OutputConcatPairXMLList : OUTPUT_XML_LIST
=struct
  type T = XML.tree list
  type data = XML.tree * XML.tree;

  fun output (t1,t2) = [t1]@[t2];
end;

