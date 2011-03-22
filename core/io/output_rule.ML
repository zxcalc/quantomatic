(* ------------------------------------------------------- *)
functor OutputRulePartsXMLList (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_XML
	sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_XML_LIST
= struct
  
  type data = Rule.T
  type T = XML.tree list

  open XML;
  fun output rule = 
      [Elem (("lhs", []), [OutputGraph.output (Rule.get_lhs rule)]),
       Elem (("rhs", []), [OutputGraph.output (Rule.get_rhs rule)])]

end;

functor OutputRuleXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_XML
	sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_XML
= OutputWrapXML (
  val tagname = "rule"
  val attrs = []
  structure Output1 = OutputRulePartsXMLList (
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
	structure OutputGraph : OUTPUT_XML
  sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_XML
= struct
  structure R = Rule
  structure OG = OutputGraph
  structure OR = OutputRuleXML (
    structure Rule = Rule
    structure OutputGraph = OutputGraph
  );
  type data = (RuleName.name * R.T) (* rulename, and rule *) 
              * Rule.Graph.T (* what the graph got rewritten to *)
  type T = XML.tree

  open XML;
  fun output ((rule_name, rule), rewrittengraph)  = 
      Elem (("rewrite", [("name", (RuleName.string_of_name rule_name))]),
            [(Elem (("newgraph", []), [(OG.output rewrittengraph)])), OR.output rule])

end;

(* vim:et:sts=2:sw=2:ts=2
*)
