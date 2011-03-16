structure InputRuleNameXML : INPUT_XML = InputSStrNameXML(RuleName);
structure InputTagNameXML : INPUT_XML = InputSStrNameXML(TagName);

(* input a set of rules *)
functor InputRulesetXML(
  structure Ruleset : RULESET 
  structure InputRule : INPUT_XML
  sharing type InputRule.data = Ruleset.Rule.T
) : INPUT_XML
= struct   
  open XML_Parse_Utils;
  
  type T = XML.tree;
  type data = Ruleset.T;

  fun data_to_string _ = "[Ruleset]";

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
      (let val () = assert_tag "ruleset" xml
           val children = get_children xml;
       in 
         Ruleset.empty 
           |> Ruleset.set_allrules (InputAllRules.input (get_the_elem "allrules" children))
           |> Ruleset.set_brel (InputBRel.input (get_the_elem "tags" children))
           |> Ruleset.set_active (RuleName.NSet.of_list 
               (InputRuleNameList.input (get_the_elem "active" children)))
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"ruleset/"^p);
end;


