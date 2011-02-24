structure InputRuleNameXML : XML_INPUT = InputSStrNameXML(RuleName);
structure InputTagNameXML : XML_INPUT = InputSStrNameXML(TagName);

(* input a set of rules *)
functor InputRuleSetXML(
  structure RuleSet : RULESET 
  structure InputRule : XML_INPUT
  sharing type InputRule.data = RuleSet.Rule.T
) : XML_INPUT
= struct   
  open XML_Parse_Utils;
  
  type T = XML.tree;
  type data = RuleSet.T;

  structure InputAllRules = 
  InputNTabXML(val tagname = "allrules"
    structure NTab = RuleName.NTab
    structure InputName = InputRuleNameXML
    structure InputValue = InputRule
  );

  structure InputBRel = 
  InputNameBRelXML(val tagname = "tags"
    structure BRel = TagRel
    structure InputDom = InputRuleNameXML
    structure InputCod = InputTagNameXML
  );

  structure InputRuleNameList = InputListXML(val tagname = "active" 
    structure Input1 = InputRuleNameXML);
  
  fun input xml = 
      let val () = assert_tag "ruleset" xml
          val children = get_children xml;
      in 
        RuleSet.empty 
          |> RuleSet.set_allrules (InputAllRules.input (get_the_elem "allrules" children))
          |> RuleSet.set_brel (InputBRel.input (get_the_elem "tags" children))
          |> RuleSet.set_active (RuleName.NSet.of_list 
              (InputRuleNameList.input (get_the_elem "active" children)))
      end;
end;



structure InputRGRuleSetXML : XML_INPUT where type data = RGRuleSet.T 
  = InputRuleSetXML(
      structure RuleSet = RGRuleSet
      structure InputRule = InputRGRuleXML
      structure InputRuleName = InputRuleNameXML
      structure InputTagName = InputTagNameXML); 
