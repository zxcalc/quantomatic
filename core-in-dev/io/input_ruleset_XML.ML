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
  
  open XML_Utils
  
  fun add_rule setname_opt ch rset = 
      let val (nm, rl) = RuleInput.input ch
          val (rnm,rset') = RuleSet.add_rule nm rl rset
      in case rset |>  end;

  fun input xml = 
      let
        val (_,_,children) = force_unpack_elem "ruleset" xml
        val (setname_opt,children') = get_elem false "name" children
      in
        fold (add_rule setname_opt) 
             (get_all_elems "rule" children') 
             RuleSet.empty
      end
end;

structure RGRuleSetInputXML : XML_INPUT where type data = Rule.T 
  = RuleSetInputXML(
      structure InputRule = RGRuleInputXML
      and Rule = Rule); 
