(* ------------------------------------------------------- *)
functor OutputRulePartsXMLList (
	structure Rule : RULE
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
	structure Rule : RULE
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

(* This matches the format of OutputRulesetDescXMLList in output_ruleset.ML *)
functor OutputNamedRuleXML (
	structure Rule : RULE
	structure OutputGraph : OUTPUT_XML
	sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_XML
= struct
  structure ORParts = OutputRulePartsXMLList (
    structure Rule = Rule
    structure OutputGraph = OutputGraph
  )
  structure ORName = OutputSStrNameXML(R)

  type data = (R.name * ORParts.data)
  type T = XML.tree

  open XML;
  fun output (name, rule) = 
      Elem (("rule", []), 
            [Elem (("name", []), [ORName.output name]),
             Elem (("definition", []), ORParts.output rule)]);
end;


(* -------------------------------------------------------- *)
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)

(* -------------------------------------------------------- *)

functor OutputRewriteXML (
	structure Rule : RULE
	structure OutputGraph : OUTPUT_XML
  sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_XML
= struct
  structure OG = OutputGraph
  structure OR = OutputNamedRuleXML (
    structure Rule = Rule
    structure OutputGraph = OutputGraph
  );
  type data = (R.name * Rule.T) (* rulename, and rule *) 
              * Rule.Graph.T (* what the graph got rewritten to *)
  type T = XML.tree

  open XML;
  fun output ((rule_name, rule), rewrittengraph)  = 
      Elem (("rewrite", []),
            [(Elem (("newgraph", []), [(OG.output rewrittengraph)])), OR.output (rule_name, rule)])

end;

(* vim:et:sts=2:sw=2:ts=2
*)
