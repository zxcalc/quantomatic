structure OutputRuleNameXMLList : OUTPUT_XML_LIST where type data = RuleName.name  
  = OutputListify(OutputSStrNameXML(RuleName));
structure OutputTagNameXMLList = OutputListify(OutputSStrNameXML(TagName));

structure OutputTagRelXMLList : OUTPUT_XML_LIST = 
  OutputNameBRelXMLList(
    val rel_tagname = "tag"
    val cod_tagname = "name"
    val dom_tagname = "rule"
    structure BRel = TagRel
    structure OutputDom = OutputRuleNameXMLList
    structure OutputCod = OutputTagNameXMLList
  );

(* -------------------------------------------------------- *)
functor OutputRulesetDescXMLList (
  structure Ruleset : RULESET
	structure OutputRule : OUTPUT_XML_LIST
	  where type data = Ruleset.Rule.T
) : OUTPUT_XML_LIST
= struct
  structure OutputAllRules = OutputWrapXML(
    val tagname = "allrules"
    val attrs = []
    structure Output1 = OutputNTabXMLList(
      val entry_tagname = "rule"
      val key_tagname = "name"
      val value_tagname = "definition"
      structure NTab = RuleName.NTab
      structure OutputName = OutputRuleNameXMLList
      structure OutputVal = OutputRule
    )
  );

  structure OutputTags = OutputWrapXML(
    val tagname = "tags"
    val attrs = []
    structure Output1 = OutputTagRelXMLList
  );

  structure OutputActiveRules = OutputWrapXML(
    val tagname = "active" 
    val attrs = []
    structure Output1 = OutputListXMLList(
      val item_tagname = "rule" 
      structure Output1 = OutputRuleNameXMLList
    )
  );
  
  type data = Ruleset.T
  type T = XML.tree list

  fun output rset = 
      [OutputAllRules.output (Ruleset.get_allrules rset),
       OutputTags.output (Ruleset.get_tagrel rset),
       OutputActiveRules.output (RuleName.NSet.list_of (Ruleset.get_active rset))];
end;

functor OutputRulesetXML (
  structure Ruleset : RULESET
	structure OutputRule : OUTPUT_XML_LIST
	  where type data = Ruleset.Rule.T
) : OUTPUT_XML
= OutputWrapXML(
    val tagname = "ruleset"
    val attrs = []
    structure Output1 = OutputRulesetDescXMLList(
      structure Ruleset = Ruleset
      structure OutputRule = OutputRule
    )
)

(* vim:et:sts=2:sw=2:ts=2
*)
