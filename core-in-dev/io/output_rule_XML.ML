(* ------------------------------------------------------- *)

(* FIXME: rules should *JUST* be left and right hand side, (and I think 
   we should have 1-file per rule representation - that way rule sets can 
   simply identify rules, and we get less problems of clashing names in
   rulesets, and we can use filenames for rule names. *)

functor RuleOutputXML (
  structure RuleName : SSTR_NAMES
	structure Rule : BASIC_RULE
	structure OutputGraph : PRETTY_XML_OUTPUT
	sharing type OutputGraph.data = Rule.Graph.T
) : PRETTY_XML_OUTPUT
= struct
  
  structure RuleName = RuleName
  structure R = Rule
  structure OG = OutputGraph
  type data = RuleName.name * R.T
  type T = Pretty.T

  val output_graph = OG.output

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]

  fun output (rule_name,rule)  = 
	      Pretty.chunks
	        [Pretty.str "<rule>",
	         indent_chunks [
		 Pretty.block [Pretty.str "<name>",
			       RuleName.pretty_name rule_name,
			       Pretty.str "</name>"],
	         Pretty.str "<lhs>",
	         Pretty.indent 2 (output_graph (Rule.get_lhs rule)),
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         indent_chunks [output_graph (Rule.get_rhs rule)],
	         Pretty.str "</rhs>"],
	         Pretty.str "</rule>",
	         Pretty.str ""]

end;

structure RGRuleOutputXML : PRETTY_XML_OUTPUT
 = RuleOutputXML(
    structure RuleName = RuleName
    structure Rule = Rule
    and OutputGraph = RGGraphOutputXML
   );

(* -------------------------------------------------------- *)
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)

(* -------------------------------------------------------- *)

functor RewriteOutputXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : PRETTY_XML_OUTPUT
  sharing type OutputGraph.data = Rule.Graph.T
) : PRETTY_XML_OUTPUT
= struct
  
  structure R = Rule
  structure OG = OutputGraph
  type data = RuleName.name * R.T * Rule.Graph.T
  type T = Pretty.T

  fun output (rule_name, rule, fused)  = 
	      Pretty.chunks
	        [Pretty.str "<rewrite>",
	         Pretty.indent 2 (Pretty.str ("<rulename>"^(RuleName.string_of_name rule_name)^"</rulename>")),
	         OG.output fused,
	         Pretty.str "<lhs>",
	         Pretty.indent 2 (OG.output (Rule.get_lhs rule)),
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         Pretty.indent 2 (OG.output (Rule.get_rhs rule)),
	         Pretty.str "</rhs>",
	         Pretty.str "</rewrite>",
	         Pretty.str ""]

end;

(* -------------------------------------------------------- *)
structure RGRewriteOutputXML : PRETTY_XML_OUTPUT
 = RewriteOutputXML(
    structure Rule = Rule
    and OutputGraph = RGGraphOutputXML
   );

(* -------------------------------------------------------- *)
structure RGRewriteListOutputXML : PRETTY_XML_OUTPUT
 = OutputListXML(
    val list_name = "rewrites"
    structure Output1 = RGRewriteOutputXML
   );
