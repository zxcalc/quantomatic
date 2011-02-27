(* XML parser for rules *)
(* ------------------------------------------------------- *)
functor InputRuleXML (
  structure Rule : RULE
  structure InputGraph : XML_INPUT
  sharing type InputGraph.data = Rule.Graph.T
) : XML_INPUT
= struct
  open XML_Parse_Utils;

  type T = XML.tree;
  type data = Rule.T;
  
  fun input xml = 
      let val _ = assert_tag "rule" xml
          val children = get_children xml
          val lhs = (InputGraph.input o get_the_elem "graph" 
                     o get_children o get_the_elem "lhs") children
          val rhs = (InputGraph.input o get_the_elem "graph" 
                     o get_children o get_the_elem "rhs") children
      in Rule.mk (lhs,rhs) end;

end;

(* ------------------------------------------------------- *)
structure InputRGRuleXML : XML_INPUT
  = InputRuleXML(
      structure Rule = RGRule
      and InputGraph = InputRGGraphXML
);
