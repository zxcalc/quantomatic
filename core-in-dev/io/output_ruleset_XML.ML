(* -------------------------------------------------------- *)
functor OutputRuleSetXML (
  structure RuleSet : RULESET
	structure OutputRule : PRETTY_XML_OUTPUT 
	  where type data = RuleSet.RuleName.name * RuleSet.Rule.T
) : PRETTY_XML_OUTPUT 
= struct
  structure OR = OutputRule
  structure RS = RuleSet
  
  type data = RS.RuleName.name * RS.T
  type T = Pretty.T

  fun output (setname, rules) =
      Pretty.chunks [ Pretty.str "<ruleset>", 
           Pretty.str "<name>",
           RS.RuleName.pretty_name setname,
           Pretty.str "</name>",
		       Pretty.indent 2 (Pretty.block (map OR.output 
		                         (RS.RuleName.NTab.list_of (RS.get_allrules rules)))),
		       Pretty.str "</ruleset>",
		       Pretty.str ""
		    ];

end;

(* -------------------------------------------------------- *)

structure OutputRGRuleSetXML : PRETTY_XML_OUTPUT
 = OutputRuleSetXML(
    structure RuleSet = RGRuleSet
    structure OutputRule = RGRuleOutputXML
   );

