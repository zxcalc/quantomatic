(* XML parser for rules *)

signature INPUT_RULE =
sig
  structure Rule : RULE
  structure InputGraph : INPUT_GRAPH
  sharing InputGraph.G.SharingWithIData = Rule.RGGraph.SharingWithIData

  include INPUT where type intype = InputGraph.intype 
				        where type outtype = Rule.Name.name * Rule.T * bool

end;


(* ------------------------------------------------------- *)


functor RuleInputXML (structure Rule : RULE
  structure InputGraph : INPUT_GRAPH 
    where type intype = XML.tree
  sharing InputGraph.G.SharingWithIData = Rule.RGGraph.SharingWithIData
  sharing type InputGraph.outtype = Rule.RGGraph.T
) : INPUT_RULE
= struct 

structure Rule = Rule;
structure InputGraph = InputGraph;
type rule = Rule.T;

open XML_Utils;

type intype = tree;
type outtype = Rule.Name.name * rule * bool;

val input_graph = InputGraph.input;

fun input xml = 
    let val _ = force_unpack_elem "rule" xml
	val name = Rule.Name.mk (input_name (get_required_child "name" xml))
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
      structure Rule = Rule
      and InputGraph = RGGraphInputXML
    );

signature INPUT_RULESET =
sig
  structure InputRule : INPUT_RULE
  (* structure RuleSet : RULESET *)
  include INPUT
    where type intype = InputRule.intype (* XML, etc. *)
      and type outtype = (InputRule.Rule.T * bool) InputRule.Rule.Name.NTab.T
          (* outtype will = RuleSet.T once we can generify Rule (no spider hack) *)
end;

(* TODO: functor-ify *)
structure RGRuleSetInputXML : INPUT_RULESET where type intype = XML.tree =
struct

structure InputRule = RGRuleInputXML
structure Rule = InputRule.Rule
(* structure RuleSet = RuleSet *)
val input_rule = InputRule.input

type intype = XML.tree
type outtype = (Rule.T * bool) Rule.Name.NTab.T (* will = RuleSet.T *)

open XML_Utils

fun input xml = 
    let
      val (_,_,children) = force_unpack_elem "theoryhack" xml
      fun add_rule ch rset = let
        val (nm, rl, active) = input_rule ch
      in snd (rset |> Rule.Name.NTab.add (nm, (rl, active)))
      end
    in
      fold add_rule (get_all_elems "rule" children) Rule.Name.NTab.empty
    end
end;

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
