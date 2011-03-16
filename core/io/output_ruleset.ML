structure OutputRuleNameXML : OUTPUT_PRETTY_XML where type data = RuleName.name  
  = OutputSStrNameXML(RuleName);
structure OutputTagNameXML = OutputSStrNameXML(TagName);

structure OutputTagRelXML : OUTPUT_PRETTY_XML = 
  OutputNameBRelXML(
    val tagname = "tags"
    val rel_tagname = "tag"
    val cod_tagname = "name"
    val dom_tagname = "rule"
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
  structure OutputAllRules = OutputNTabXML(
    val tagname = "allrules"
    val entry_tagname = "rule"
    val key_tagname = "name"
    val value_tagname = "definition"
    structure NTab = RuleName.NTab
    structure OutputName = OutputRuleNameXML
    structure OutputVal = OutputRule
  );

  structure OutputRuleNameList = OutputListXML(
    val tagname = "active" 
    val item_tagname = "rule" 
    structure Output1 = OutputRuleNameXML);
  
  type data = Ruleset.T
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

(* vim:et:sts=2:sw=2:ts=2
*)
