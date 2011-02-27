(* ------------------------------------------------------- *)
functor RuleOutputXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : PRETTY_XML_OUTPUT
	sharing type OutputGraph.data = Rule.Graph.T
) : PRETTY_XML_OUTPUT
= struct
  
  type data = Rule.T
  type T = Pretty.T

  fun output rule = 
	      Pretty.chunks
	        [Pretty.str "<rule>",
	         Pretty.str "<lhs>",
	         Pretty.indent 2 (OutputGraph.output (Rule.get_lhs rule)),
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         Pretty.indent 2 (OutputGraph.output (Rule.get_rhs rule)),
	         Pretty.str "</rhs>",
	         Pretty.str "</rule>",
	         Pretty.str ""]

end;

structure OutputRGRuleXML : PRETTY_XML_OUTPUT
 = RuleOutputXML(
    structure Rule = RGRule
    and OutputGraph = OutputRGGraphXML
   );

(* -------------------------------------------------------- *)
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)

(* -------------------------------------------------------- *)

functor OutputRewriteXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : PRETTY_XML_OUTPUT
  sharing type OutputGraph.data = Rule.Graph.T
) : PRETTY_XML_OUTPUT
= struct
  
  structure R = Rule
  structure OG = OutputGraph
  type data = (RuleName.name * R.T) (* rulename, and rule *) 
              * Rule.Graph.T (* what the graph got rewritten to *)
  type T = Pretty.T

  fun output ((rule_name, rule), rewrittengraph)  = 
	      Pretty.chunks
	        [Pretty.str "<rewrite>",
	         Pretty.indent 2 (Pretty.chunks [
             (Pretty.str ("<rulename>"^(RuleName.string_of_name rule_name)^"</rulename>")),
             Pretty.str "<newgraph>",
             Pretty.indent 2 (OG.output rewrittengraph),
             Pretty.str "</newgraph>",
             Pretty.str "<lhs>",
             Pretty.indent 2 (OG.output (Rule.get_lhs rule)),
             Pretty.str "</lhs>",
             Pretty.str "<rhs>", 
             Pretty.indent 2 (OG.output (Rule.get_rhs rule)),
             Pretty.str "</rhs>"]),
	         Pretty.str "</rewrite>",
	         Pretty.str ""]

end;

(* -------------------------------------------------------- *)
structure OutputRGRewriteXML 
: PRETTY_XML_OUTPUT where type data = (RuleName.name *RGRule.T) * RGGraph.T
 = OutputRewriteXML(
    structure Rule = RGRule
    and OutputGraph = OutputRGGraphXML
   );

(* -------------------------------------------------------- *)
structure OutputRGRewriteListXML 
: PRETTY_XML_OUTPUT where type data = ((RuleName.name * RGRule.T) * RGGraph.T) list
 = OutputListXML(
    val tagname = "rewrites"
    structure Output1 = OutputRGRewriteXML
   );
