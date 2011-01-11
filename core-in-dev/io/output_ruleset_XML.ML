(* -------------------------------------------------------- *)

signature OUTPUT_RULESET =
sig
  structure RS : RULESET
  structure OR : OUTPUT_RULE
	sharing RS.Rule.Sharing	 = OR.R.Sharing

  include OUTPUT where type outtype = OR.outtype
		where type intype = RS.Name.name * RS.T * bool
end

(* -------------------------------------------------------- *)

functor RGRuleSetOutputXML (
	structure OutputRule : OUTPUT_RULE
				  where type outtype = Pretty.T
  structure RuleSet : RULESET
  sharing RuleSet.Rule.Sharing = OutputRule.R.Sharing
  sharing type RuleSet.Rule.T = OutputRule.R.T
	) : OUTPUT_RULESET
= 
struct
  structure OR = OutputRule
  structure RS = RuleSet
  
  type intype = RS.Name.name * RS.T * bool 
  type outtype = Pretty.T

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]	    

  fun output (setname, rules, active) =
      let val activestring = if (active) then "true" else "false"
      in
      Pretty.chunks [ Pretty.str "<ruleset>", 
           Pretty.str "<name>",
           RS.Name.pretty_name setname,
           Pretty.str "</name>",
           Pretty.str "<active>",
           Pretty.str activestring,
           Pretty.str "</active>",
		      indent_chunks (map OR.output (RS.Rule.Name.NTab.list_of rules)),
		      Pretty.str "</ruleset>",
		      Pretty.str ""
		    ]
       end

end;

(* -------------------------------------------------------- *)

structure RuleSetOutputXML : OUTPUT_RULESET
 = RuleSetOutputXML(
    structure OutputRule = RuleOutputXML
    structure RuleSet = RuleSet
   );

