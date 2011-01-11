signature OUTPUT_RULE =
sig
  structure R : BASIC_RULE
  structure OG : OUTPUT_GRAPH
  sharing OG.Graph.SharingBangGraph = R.Graph.SharingBangGraph
	 
  include OUTPUT where type outtype = OG.outtype
		where type intype = R.Name.name * (R.T * bool)
end;

(* ------------------------------------------------------- *)

functor RuleOutputXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_GRAPH
  where type outtype = Pretty.T
	sharing OutputGraph.Graph.SharingBangGraph = Rule.Graph.SharingBangGraph   
  sharing type OutputGraph.intype = Rule.Graph.T
) : OUTPUT_RULE
= struct
  structure R = Rule
  structure OG = OutputGraph
  type intype = R.Name.name * (R.T * bool)
  type outtype = Pretty.T

  val output_graph = OG.output

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]

  fun output (rule_name, (rule, active))  = 
	      Pretty.chunks
	        [Pretty.str "<rule>",
	         indent_chunks [
		 Pretty.block [Pretty.str "<name>",
			       R.Name.pretty_name rule_name,
			       Pretty.str "</name>"],
		Pretty.block [Pretty.str "<active>",
			       Pretty.str (Bool.toString active),
			       Pretty.str "</active>"],
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
(* -------- REWRITES ARE MUCH LIKE RULES -------------------*)
(* -------------------------------------------------------- *)


signature OUTPUT_REWRITE =
sig
  structure R : BASIC_RULE
  structure OG : OUTPUT_GRAPH
  sharing OG.Graph.SharingBangGraph = R.Graph.SharingBangGraph
		 
  include OUTPUT where type outtype = OG.outtype
		where type intype = R.Name.name * R.T * OG.Graph.T
end

(* -------------------------------------------------------- *)

functor RewriteOutputXML (
	structure Rule : BASIC_RULE
	structure OutputGraph : OUTPUT_GRAPH
				  where type outtype = Pretty.T
	sharing OutputGraph.Graph.SharingBangGraph = Rule.Graph.SharingBangGraph
  (* FIXME: why do I need this, I think it should be implied by the above and def of output graph... *)
  sharing type OutputGraph.intype = Rule.Graph.T
	) : OUTPUT_REWRITE
= 
struct
  structure R = Rule
  structure OG = OutputGraph
  type intype = R.Name.name * R.T * OG.Graph.T
  type outtype = Pretty.T

  val output_graph = OG.output

  fun indent_chunks cs = Pretty.block [Pretty.str "  ", Pretty.chunks cs]

  fun output (rule_name, rule, fused)  = 
	      Pretty.chunks
	        [Pretty.str "<rewrite>",
	         indent_chunks [
	         Pretty.str ("<rulename>"^(R.Name.string_of_name rule_name)^"</rulename>"),
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
