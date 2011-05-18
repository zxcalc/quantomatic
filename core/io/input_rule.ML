(* XML parser for rules *)
(* ------------------------------------------------------- *)
functor InputRuleXML (
  structure Rule : RULE
  structure InputGraph : INPUT_XML
  sharing type InputGraph.data = Rule.Graph.T
) : INPUT_XML
= struct
  open XML_Parse_Utils;

  type T = XML.tree;
  type data = Rule.T;

  fun data_to_string _ = "[Rule]";
  
  fun input xml = 
      (let val children = get_children xml
           val lhs_xml = (get_unique_elem "graph" o get_children 
                          o get_unique_elem "lhs") children;
           val lhs = (InputGraph.input lhs_xml)
                     handle bad_input_exp (s,p) => 
                       raise bad_input_exp (s,"lhs/graph/"^p);
           val rhs_xml = (get_unique_elem "graph" o get_children 
                          o get_unique_elem "rhs") children;
           val rhs = (InputGraph.input rhs_xml)
                     handle bad_input_exp (s,p) => 
                       raise bad_input_exp (s,"rhs/graph/"^p);
       in
          Rule.mk (lhs,rhs)
            handle Rule.bad_rule_exp (msg,_,_) => 
              raise bad_input_exp ("Invalid rule: "^msg,"rule")
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"rule/"^p);

end;

(* vim:et:sts=2:sw=2
*)
