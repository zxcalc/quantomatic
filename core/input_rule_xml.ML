(* XML parser for rules *)

signature INPUT_RULE =
sig
  structure R : RULE
  structure IG : INPUT_GRAPH
  sharing IG.G.SharingWithIData = R.RGGraph.SharingWithIData

  include INPUT where type intype = IG.intype 
				        where type outtype = R.Name.name * R.T * bool

end;


(* ------------------------------------------------------- *)


functor RuleInputXML (structure Rule : RULE
  structure InputGraph : INPUT_GRAPH 
    where type intype = XML.tree
  sharing InputGraph.G.SharingWithIData = Rule.RGGraph.SharingWithIData
  sharing type InputGraph.outtype = Rule.RGGraph.T
) : INPUT_RULE
= struct 

structure R = Rule;
structure IG = InputGraph;
type rule = R.T;

open XML_Utils;

type intype = tree;
type outtype = Rule.Name.name * rule * bool;

val input_graph = IG.input;

fun input xml = 
    let val _ = force_unpack_elem "rule" xml
	val name = R.Name.mk (input_name (get_required_child "name" xml))
	val lhs = input_graph 
		    (get_required_child "graph" (get_required_child "lhs" xml))
	val rhs = input_graph 
		    (get_required_child "graph" (get_required_child "rhs" xml))
  val active = case (get_child false "active" xml) of
                 (NONE,_) => true
               | (SOME b, _) => input_boolean b
    in
      (name, R.mk (lhs,rhs), active)
    end
end;

(* ------------------------------------------------------- *)

structure RGRuleInputXML : INPUT_RULE
  = RuleInputXML(
      structure Rule = Rule
      and InputGraph = RGGraphInputXML
    );
    

(* ------------------------------------------------------- *)
(* -------- --------- !! THEORY HACK !! -------- --------  *)
(* ------------------------------------------------------- *)

signature INPUT_THEORY_HACK =
sig
  structure IR : INPUT_RULE

  include INPUT where type intype = IR.intype 
				        where type outtype = IR.outtype list
end;

(* ------------------------------------------------------- *)

functor TheoryHackInputXML (structure InputRule : INPUT_RULE
                            where type intype = XML.tree
) : INPUT_THEORY_HACK
= struct 

structure IR = InputRule;

open XML_Utils
type intype = tree;
type outtype = IR.outtype list

val input_rule = IR.input

fun input xml = 
    let val (_,_,children) = force_unpack_elem "theoryhack" xml
    in
      List.map input_rule (get_all_elems "rule" children)
    end

end;

(* ------------------------------------------------------- *)

structure RGTheoryHackInputXML : INPUT_THEORY_HACK
  = TheoryHackInputXML(
      structure InputRule = RGRuleInputXML
    );
