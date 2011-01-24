(* XML parser for rules *)

signature INPUT_RULE =
sig
  structure RuleName : SSTR_NAMES
  structure Rule : RULE
  structure InputGraph : INPUT_GRAPH
  sharing InputGraph.G.SharingBangGraph = Rule.Graph.SharingBangGraph

  include INPUT where type intype = InputGraph.intype 
				        where type outtype = RuleName.name * Rule.T * bool

end;


(* ------------------------------------------------------- *)


functor RuleInputXML (structure Rule : RULE
  structure RuleName : SSTR_NAMES
  structure InputGraph : INPUT_GRAPH 
    where type intype = XML.tree
  sharing InputGraph.G.SharingBangGraph = Rule.Graph.SharingBangGraph
  sharing type InputGraph.outtype = Rule.Graph.T
) : INPUT_RULE
= struct 

structure RuleName = RuleName;
structure Rule = Rule;
structure InputGraph = InputGraph;
type rule = Rule.T;

open XML_Utils;

type intype = tree;
type outtype = RuleName.name * rule * bool;

val input_graph = InputGraph.input;

fun input xml = 
    let val _ = force_unpack_elem "rule" xml
	val name = RuleName.mk (input_name (get_required_child "name" xml))
	val lhs = input_graph 
		    (get_required_child "graph" (get_required_child "lhs" xml))
	val rhs = input_graph 
		    (get_required_child "graph" (get_required_child "rhs" xml))
  val active = case (get_child false "active" xml) of
                 (NONE,_) => true
               | (SOME b, _) => input_boolean b
    in
      (name, Rule.mk (lhs,rhs), active)
    end
end;

(* ------------------------------------------------------- *)

structure RGRuleInputXML : INPUT_RULE
  = RuleInputXML(
      structure RuleName = RuleName
      structure Rule = Rule
      and InputGraph = RGGraphInputXML
    );

(* ------------------------------------------------------- *)
(* -------- --------- !! THEORY HACK !! -------- --------  *)
(* ------------------------------------------------------- *)

(*
signature INPUT_THEORY_HACK =
sig
  structure InputRule : INPUT_RULE

  include INPUT where type intype = InputRule.intype 
				        where type outtype = InputRule.outtype list
end;
*)

(* ------------------------------------------------------- *)

(*
functor TheoryHackInputXML (structure InputRule : INPUT_RULE
                            where type intype = XML.tree
) : INPUT_THEORY_HACK
= struct 

structure InputRule = InputRule;

open XML_Utils
type intype = tree;
type outtype = InputRule.outtype list

val input_rule = InputRule.input

fun input xml = 
    let val (_,_,children) = force_unpack_elem "theoryhack" xml
    in
      List.map input_rule (get_all_elems "rule" children)
    end

end;
*)

(* ------------------------------------------------------- *)

(*
structure RGTheoryHackInputXML : INPUT_THEORY_HACK
  = TheoryHackInputXML(
      structure InputRule = RGRuleInputXML
    );
 *)
