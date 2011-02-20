(* input a set of rules *)
functor RuleSetInputXML(
  structure RuleSet : RULESET 
  structure RuleInput : XML_INPUT
  sharing RuleInput.data = RuleSet.Rule.T
) : XML_INPUT
= struct 
  type T = XML.tree
  type data = RuleSet.T

  structure RuleName = RuleSet.RuleName;
  
  open XML_Parse_Utils
  
  fun add_rule setname_opt ch rset = 
      let val (nm, rl) = RuleInput.input ch
          val (rnm,rset') = RuleSet.add_rule nm rl rset
      in case rset |>  end;

  fun input xml = 
      let
        val () = assert_tag "ruleset" xml
        val children = get_children xml
        val setname_opt = lookup_the_elem "name" children
      in
        fold (add_rule setname_opt) 
             (filter_elems "rule" children') 
             RuleSet.empty
      end
end;

structure RGRuleSetInputXML : XML_INPUT where type data = Rule.T 
  = RuleSetInputXML(
      structure InputRule = RGRuleInputXML
      and Rule = Rule); 
