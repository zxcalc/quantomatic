(* input a set of rules *)
functor RuleSetInputXML(
  structure RuleName 
  structure RuleSet : RULESET 
  structure RuleInput : XML_INPUT
  sharing RuleInput.data = 
) : XML_INPUT
= struct 
  type T = XML.tree
  type data = RuleSet.T
  
  open XML_Utils
  
  fun add_rule ch rset = 
      let val (nm, rl, active) = RuleInput.input ch
      in snd (rset |> RuleSet. (nm, (rl, active))) end;

  fun input xml = 
      let
        val (_,_,children) = force_unpack_elem "ruleset" xml
        
      in
        fold add_rule (get_all_elems "rule" children) RuleName.NTab.empty
      end
end;

structure RGRuleSetInputXML : XML_INPUT where type data = Rule.T 
  = RuleSetInputXML(
      structure InputRule = RGRuleInputXML
      and Rule = Rule); 
