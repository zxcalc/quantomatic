(* XML parser for rules *)
(* ------------------------------------------------------- *)
functor RuleInputXML (
  structure Rule : RULE
  structure RuleName : SSTR_NAMES
  structure InputGraph : XML_INPUT
  sharing type InputGraph.data = Rule.Graph.T
) : XML_INPUT
= struct
  type T = XML.tree;
  type data = RuleName.name * Rule.T;
  
  open XML_Utils;

  fun input xml = 
      let val _ = force_unpack_elem "rule" xml
          val name = RuleName.mk (input_name (get_required_child "name" xml))
          val lhs = InputGraph.input 
                (get_required_child "graph" (get_required_child "lhs" xml))
          val rhs = InputGraph.input 
                (get_required_child "graph" (get_required_child "rhs" xml))
          (* val active = case (get_child false "active" xml) of
                         (NONE,_) => true
                       | (SOME b, _) => input_boolean b *)
      in (name, Rule.mk (lhs,rhs)) end;

end;

(* ------------------------------------------------------- *)
structure RGRuleInputXML : XML_INPUT where type data = RuleName.name * Rule.T
  = RuleInputXML(
      structure Rule = Rule
      and RuleName = RuleName
      and InputGraph = RGGraphInputXML
);
