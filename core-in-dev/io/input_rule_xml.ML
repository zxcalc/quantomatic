(* XML parser for rules *)
(* ------------------------------------------------------- *)
functor RuleInputXML (
  structure Rule : RULE
  structure RuleName : SSTR_NAMES
  structure InputGraph : XML_INPUT
  sharing type InputGraph.data = Rule.Graph.T
) : XML_INPUT
= struct
  open XML_Parse_Utils;

  type T = XML.tree;
  type data = RuleName.name * Rule.T;
  
  fun input xml = 
      let val _ = assert_tag "rule" xml
          val children = get_children xml
          val name = RuleName.mk (get_the_child_text (get_the_elem "name" children))
          val lhs = (InputGraph.input o get_the_elem "graph" 
                     o get_children o get_the_elem "lhs") children
          val rhs = (InputGraph.input o get_the_elem "graph" 
                     o get_children o get_the_elem "rhs") children
      in (name, Rule.mk (lhs,rhs)) end;

end;

(* ------------------------------------------------------- *)
structure RGRuleInputXML : XML_INPUT where type data = RuleName.name * Rule.T
  = RuleInputXML(
      structure Rule = Rule
      and RuleName = RuleName
      and InputGraph = RGGraphInputXML
);
