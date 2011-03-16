(* ------------------------------------------------------- *)
functor OutputBareRuleXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_PRETTY_XML
	sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_PRETTY_XML
= struct
  
  type data = Rule.T
  type T = Pretty.T

  fun output rule = 
	      Pretty.chunks
	        [Pretty.str "<lhs>",
	         Pretty.indent 2 (OutputGraph.output (Rule.get_lhs rule)),
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         Pretty.indent 2 (OutputGraph.output (Rule.get_rhs rule)),
	         Pretty.str "</rhs>"]

end;

functor OutputRuleXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_PRETTY_XML
	sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_PRETTY_XML
= OutputWrapXML (
  val tagname = "rule"
  val attrs = []
  structure Output1 = OutputBareRuleXML (
    structure Rule = Rule
    structure OutputGraph = OutputGraph
  )
);


(* -------------------------------------------------------- *)
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)

(* -------------------------------------------------------- *)

functor OutputRewriteXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_PRETTY_XML
  sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_PRETTY_XML
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

(* vim:et:sts=2:sw=2:ts=2
*)
