(* -------------------------------------------------------- *)
functor RGRuleSetOutputXML (
	structure OutputRule : PRETTY_XML_OUTPUT
  structure RuleSet : RULESET
  sharing type RuleSet.Rule.T = OutputRule.data
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
		                         (RS.Name.NTab.list_of (RS.get_allrules rules)))),
		       Pretty.str "</ruleset>",
		       Pretty.str ""
		    ];

end;

(* -------------------------------------------------------- *)

structure RuleSetOutputXML : OUTPUT_RULESET
 = RuleSetOutputXML(
    structure OutputRule = RuleOutputXML
    structure RuleSet = RuleSet
   );

