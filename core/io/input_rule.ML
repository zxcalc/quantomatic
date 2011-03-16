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
           val lhs = (InputGraph.input o get_the_elem "graph" 
                      o get_children o get_the_elem "lhs") children
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"lhs/"^p)
           val rhs = (InputGraph.input o get_the_elem "graph" 
                      o get_children o get_the_elem "rhs") children
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"rhs/"^p)
       in Rule.mk (lhs,rhs) end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"rule/"^p);

end;

(* vim:et:sts=2:sw=2
*)
