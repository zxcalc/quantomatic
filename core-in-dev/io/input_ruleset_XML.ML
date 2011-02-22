(* input a set of rules *)
functor RuleSetInputXML(
  structure RuleSet : RULESET 
  structure InputRule : XML_INPUT
  structure InputSubsetName : XML_INPUT
  structure InputRuleName : XML_INPUT
  sharing type InputRule.data = RuleSet.Rule.T
  sharing type InputSubsetName.data = RuleSet.SubsetName.name
  sharing type InputRuleName.data = RuleSet.RuleName.name
) : XML_INPUT
= struct   
  open XML_Parse_Utils;
  
  type T = XML.tree;
  type data = RuleSet.T;

  structure RuleName = RuleSet.RuleName;
  
  fun add_tags nm xml rset = 
      let val () = assert_tag "tags" xml
        val children = get_children xml
        val tag_xmls = filter_elems "tag" children
      in 
        fold (RuleSet.add_rule_to_subset nm o InputSubsetName.input) tag_xmls rset
      end;

  fun add_rule xml rset = 
      let val () = assert_tag "rule" xml;
        val rule = InputRule.input xml;
        val children = get_children xml;
        val tags_xml_opt = lookup_the_elem "tags" children;
        val name_xml = get_the_elem "name" children;
        val nm = InputRuleName.input name_xml;
        val (nm',rset') = RuleSet.add_rule (nm,rule) rset
      in rset' |> (case tags_xml_opt of NONE => I 
                  | SOME tags_xml => add_tags nm' tags_xml)
      end; (* IMPROVE: wanring for renamed stuff? *)

  fun input xml = 
      let val () = assert_tag "rules" xml in
        RuleSet.empty 
          |> fold add_rule (filter_elems "rule" (get_children xml))
      end
end;


structure InputRuleNameXML = InputSStrNameXML(RGRuleSet.RuleName);
structure InputSubsetNameXML = InputSStrNameXML(RGRuleSet.SubsetName);

structure RGRuleSetInputXML : XML_INPUT where type data = RGRuleSet.T 
  = RuleSetInputXML(
      structure RuleSet = RGRuleSet
      structure InputRule = InputRGRuleXML
      structure InputRuleName = InputRuleNameXML
      structure InputSubsetName = InputSubsetNameXML); 
