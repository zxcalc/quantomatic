structure OutputRuleNameXML : PRETTY_XML_OUTPUT where type data = RuleName.name  
  = OutputSStrNameXML(RuleName);
structure OutputTagNameXML = OutputSStrNameXML(TagName);

structure OutputTagRelXML : PRETTY_XML_OUTPUT = 
  OutputNameBRelXML(val tagname = "tags"
    structure BRel = TagRel
    structure OutputDom = OutputRuleNameXML
    structure OutputCod = OutputTagNameXML
  );

(* -------------------------------------------------------- *)
functor OutputRuleSetXML (
  structure RuleSet : RULESET
	structure OutputRule : PRETTY_XML_OUTPUT 
	  where type data = RuleSet.Rule.T
) : PRETTY_XML_OUTPUT 
= struct
  structure OR = OutputRule
  structure RS = RuleSet

  structure OutputAllRules = OutputNTabXML(val tagname = "allrules"
    structure NTab = RuleName.NTab
    structure OutputName = OutputRuleNameXML
    structure OutputVal = OutputRule
  );

  structure OutputRuleNameList = OutputListXML(val tagname = "active" 
    structure Output1 = OutputRuleNameXML);
  
  type data = RS.T
  type T = Pretty.T

  fun output rset = 
      Pretty.chunks 
      [Pretty.str ("<ruleset>"),
       Pretty.indent 2 ( 
         Pretty.chunks 
          [OutputAllRules.output (RuleSet.get_allrules rset),
           OutputTagRelXML.output (RuleSet.get_tagrel rset),
           OutputRuleNameList.output (RuleName.NSet.list_of (RuleSet.get_active rset))]),
       Pretty.str ("</ruleset>")];
end;

(* -------------------------------------------------------- *)

structure OutputRGRuleSetXML : PRETTY_XML_OUTPUT
 = OutputRuleSetXML(
    structure RuleSet = RGRuleSet
    structure OutputRule = OutputRGRuleXML
   );

