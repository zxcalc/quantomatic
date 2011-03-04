structure OutputRuleNameXML : OUTPUT_PRETTY_XML where type data = RuleName.name  
  = OutputSStrNameXML(RuleName);
structure OutputTagNameXML = OutputSStrNameXML(TagName);

structure OutputTagRelXML : OUTPUT_PRETTY_XML = 
  OutputNameBRelXML(val tagname = "tags"
    structure BRel = TagRel
    structure OutputDom = OutputRuleNameXML
    structure OutputCod = OutputTagNameXML
  );

(* -------------------------------------------------------- *)
functor OutputRulesetXML (
  structure Ruleset : RULESET
	structure OutputRule : OUTPUT_PRETTY_XML 
	  where type data = Ruleset.Rule.T
) : OUTPUT_PRETTY_XML 
= struct
  structure OR = OutputRule
  structure RS = Ruleset

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
          [OutputAllRules.output (Ruleset.get_allrules rset),
           OutputTagRelXML.output (Ruleset.get_tagrel rset),
           OutputRuleNameList.output (RuleName.NSet.list_of (Ruleset.get_active rset))]),
       Pretty.str ("</ruleset>")];
end;
