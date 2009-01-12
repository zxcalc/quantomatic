signature OUTPUT_RULE =
sig
  structure R : RULE
  structure OG : OUTPUT_GRAPH
  sharing OG.Graph = R.RGGraph
		 
  include OUTPUT where type outtype = OG.outtype
		where type intype = string * R.T
end;

(* ------------------------------------------------------- *)

functor RuleOutputXML (
	structure Rule : RULE
	structure OutputGraph : OUTPUT_GRAPH
				  where type outtype = Pretty.T
	sharing OutputGraph.Graph = Rule.RGGraph
	) : OUTPUT_RULE
= 
struct
  structure R = Rule
  structure OG = OutputGraph
  type intype = string * R.T
  type outtype = Pretty.T

  val output_graph = OG.output

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]

  fun output (rule_name, rule)  = 
	      Pretty.chunks
	        [Pretty.str "<rule>",
	         indent_chunks [
		 Pretty.block [Pretty.str "<name>",
			       Pretty.str rule_name,
			       Pretty.str "</name>"],
	         Pretty.str "<lhs>",
	         indent_chunks [output_graph (Rule.get_lhs rule)],
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         indent_chunks [output_graph (Rule.get_rhs rule)],
	         Pretty.str "</rhs>"],
	         Pretty.str "</rule>",
	         Pretty.str ""]

end;

(* -------------------------------------------------------- *)

structure RGRuleOutputXML : OUTPUT_RULE
 = RuleOutputXML(
    structure Rule = Rule
    and OutputGraph = RGGraphOutputXML
   );

(* -------------------------------------------------------- *)

signature OUTPUT_THEORY_HACK =
sig
  structure OR : OUTPUT_RULE
		 
  include OUTPUT where type outtype = OR.outtype
		where type intype = (OR.intype) list
end

(* -------------------------------------------------------- *)

functor TheoryHackOutputXML (
	structure OutputRule : OUTPUT_RULE
				  where type outtype = Pretty.T
	) : OUTPUT_THEORY_HACK
= 
struct
  structure OR = OutputRule
  type intype = OR.intype list 
  type outtype = Pretty.T

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]	    

  fun output rules = 
      Pretty.chunks [ Pretty.str "<theoryhack>", 
		      indent_chunks (map OR.output rules),
		      Pretty.str "</theoryhack>",
		      Pretty.str ""
		    ]

end;

(* -------------------------------------------------------- *)

structure RGTheoryHackOutputXML : OUTPUT_THEORY_HACK
 = TheoryHackOutputXML(
    structure OutputRule = RGRuleOutputXML
   );

(* -------------------------------------------------------- *)
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)


signature OUTPUT_REWRITE =
sig
  structure R : RULE
  structure OG : OUTPUT_GRAPH
  sharing OG.Graph = R.RGGraph
		 
  include OUTPUT where type outtype = OG.outtype
		where type intype = string * R.T * OG.Graph.T
end

(* -------------------------------------------------------- *)

functor RewriteOutputXML (
	structure Rule : RULE
	structure OutputGraph : OUTPUT_GRAPH
				  where type outtype = Pretty.T
	sharing OutputGraph.Graph = Rule.RGGraph
	) : OUTPUT_REWRITE
= 
struct
  structure R = Rule
  structure OG = OutputGraph
  type intype = string * R.T * OG.Graph.T
  type outtype = Pretty.T

  val output_graph = OG.output

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]

  fun output (rule_name, rule, fused)  = 
	      Pretty.chunks
	        [Pretty.str "<rewrite>",
	         indent_chunks [
	         Pretty.str ("<rulename>"^rule_name^"</rulename>"),
	         output_graph fused,
	         Pretty.str "<lhs>",
	         indent_chunks [output_graph (Rule.get_lhs rule)],
	         Pretty.str "</lhs>",
	         Pretty.str "<rhs>", 
	         indent_chunks [output_graph (Rule.get_rhs rule)],
	         Pretty.str "</rhs>"],
	         Pretty.str "</rewrite>",
	         Pretty.str ""]

end;

(* -------------------------------------------------------- *)

structure RGRewriteOutputXML : OUTPUT_REWRITE
 = RewriteOutputXML(
    structure Rule = Rule
    and OutputGraph = RGGraphOutputXML
   );

(* -------------------------------------------------------- *)

signature OUTPUT_REWRITE_LIST =
sig
  structure ORw : OUTPUT_REWRITE
		 
  include OUTPUT where type outtype = ORw.outtype
		where type intype = (ORw.intype) list
end

(* -------------------------------------------------------- *)

functor RewriteOutputListXML (
	structure OutputRewrite : OUTPUT_REWRITE
				  where type outtype = Pretty.T
	) : OUTPUT_REWRITE_LIST
= 
struct
  structure ORw = OutputRewrite
  type intype = ORw.intype list 
  type outtype = Pretty.T

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]	    

  fun output rewrites = 
      Pretty.chunks [ Pretty.str "<rewrites>", 
		      indent_chunks (map ORw.output rewrites),
		      Pretty.str "</rewrites>",
		      Pretty.str ""
		    ]

end;

(* -------------------------------------------------------- *)

structure RGRewriteListOutputXML : OUTPUT_REWRITE_LIST
 = RewriteOutputListXML(
    structure OutputRewrite = RGRewriteOutputXML
   );
