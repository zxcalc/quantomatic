(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* rules for rewriting graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* 

 IDEA: use matching to find symmetries in lhs and rhs: find how many
ways a rule can rewrite a graph in the same way. Do this by matching a
graph with itself, with the extra initial matching agenda of boundary
nodes must match boundary nodes. Results in symmetric rewrites. 

QUESTION: are any axioms non-symmetric in application? Do all rewrites
necessarily rewrite symmetrically? or can you have lhs symmetries
without the rhs ones? (yes in general, but what about quantum
computation?)

*)

signature RULE_SHARING =
sig
  structure Name : NAMES_SHARING
  structure BangGraph : BANG_GRAPH_SHARING
  (*structure MatchSearch : MATCH_SEARCH_SHARING *)
end
 

signature RULE 
= sig

structure Name : SSTR_NAMES
structure BangGraph : BANG_GRAPH

structure Sharing : RULE_SHARING
sharing Sharing.Name = Name
sharing Sharing.BangGraph = BangGraph.SharingBangGraph

type T (* rule *)

exception bad_rule_exp of string * BangGraph.T * BangGraph.T;
val mk : (BangGraph.T * BangGraph.T) -> T;

val get_lhs : T -> BangGraph.T
val get_rhs : T -> BangGraph.T

(* swaps lhs and rhs *)
val symmetric : T -> T

(* rule is a trivial identity : lhs matches rhs *)
val is_trivial : T -> bool

val pretty : T -> Pretty.T
val print : T -> unit


end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor RuleFun( BangGraph : BANG_GRAPH )
= struct

structure BangGraph = BangGraph;
structure Vertex = BangGraph.Vertex;
structure Edge = BangGraph.Edge;

(* Names of rules *)
structure Name 
= struct
  open SStrName;
  val default_name = SStrName.mk "new-rule-1"
end

structure Sharing = struct
  structure Name = Name.Sharing
  structure BangGraph = BangGraph.SharingBangGraph
end

datatype T = Rule of { lhs : BangGraph.T,
                       rhs : BangGraph.T,
                       lhs_aut : (BangGraph.VUmorph.T*BangGraph.EUmorph.T) list,
                       selfapps : BangGraph.VUmorph.T list };

exception bad_rule_exp of string * BangGraph.T * BangGraph.T;
fun mk (lhs,rhs) = 
    let val lhsbndry : Vertex.NSet.T = BangGraph.get_boundary lhs in
    (* check that rule is well-formed! *)
    if Vertex.NSet.eq lhsbndry (BangGraph.get_boundary rhs) then 
      let (* rename rhs; shared bvertex names, all else is distinct. *)
        val vrn = Vertex.mk_renaming lhsbndry (BangGraph.get_vnames lhs) 
                                     Vertex.NTab.empty
        val ern = Edge.mk_renaming Edge.NSet.empty 
                                   (BangGraph.get_enames lhs) 
                                   Edge.NTab.empty
        val (rn, rhs') = BangGraph.rename (vrn,ern) rhs
        val lhsauts = (Seq.list_of oo BangGraph.get_isomorphisms) lhs lhs
        val rhsauts = (Seq.list_of oo BangGraph.get_isomorphisms) rhs rhs
        val lhsba = map (BangGraph.VUmorph.restrict_dom_to lhsbndry o fst) lhsauts
        val rhsba = map (BangGraph.VUmorph.restrict_dom_to lhsbndry o fst) rhsauts
        fun filterorbits (x::xs) os = let val orbit = map (BangGraph.VUmorph.compose x) rhsba 
                                          fun eq a b = ((BangGraph.VUmorph.Dom.NTab.fold 
                                                         (fn (d,c) => BangGraph.VUmorph.add d c) (BangGraph.VUmorph.get_domtab a) b;true)
                                                       handle BangGraph.VUmorph.add_exp(_,_,_) => false)
                                          val xs' = filter_out
                                                      (fn a => exists (eq a) orbit)
                                                      xs
                                      in filterorbits xs' (x::os) end
          | filterorbits [] os = os
      in Rule { lhs = lhs, rhs = rhs',
                lhs_aut = lhsauts,
                selfapps = filterorbits lhsba [] } end
    else
      raise bad_rule_exp ("mk: Left and right hand side boundaries are different", lhs, rhs)
    end;

(* 
fun rename_interior_vs xvs (avoidvs, avoides, avoidbs) g = 
     let (* rename rhs; shared bvertex names, all else is distinct. *)
        val vrn = Vertex.mk_renaming xvs avoidvs Vertex.NTab.empty
  in BangGraph.rename (vrn,Edge.Rnm.empty) g  end;
*)

(* match anything = no set matching *)
(* val any_match = Match.empty; *)

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;

fun update_lhs f (Rule rep) = 
    Rule { lhs = f (#lhs rep), rhs = #rhs rep, lhs_aut = #lhs_aut rep, selfapps = #selfapps rep };
fun update_rhs f (Rule rep) = 
    Rule { lhs = #lhs rep, rhs = f (#rhs rep), lhs_aut = #lhs_aut rep, selfapps = #selfapps rep };

val set_lhs = update_lhs o K;
val set_rhs = update_rhs o K;

(* THINK: cache this info? *)
(* 
fun get_rhs_newnodes r = 
    let val (lhsns,rhsns) = BangGraph.get_vnames (get_rhs r)
    in Vertex.NSet.subtract rhsns lhsns end;
fun get_lhs_delnodes r =
    let val (lhsns,rhsns) = BangGraph.get_vnames (get_lhs r)
    in Vertex.NSet.subtract lhsns rhsns  end;
fun get_rhs_newedges r =
    let val (lhsns,rhsns) = BangGraph.get_enames (get_rhs r)
    in Edge.NSet.subtract rhsns lhsns end;
fun get_lhs_deledges r =
    let val (lhsns,rhsns) = BangGraph.get_enames (get_lhs r)
    in Edge.NSet.subtract lhsns rhsns  end;
*)

(* swap lhs and rhs of a rule *)
fun symmetric r = mk (get_rhs r, get_lhs r);

(* a rule is trivially true if the left and right hand sides match.
   Proof is by observing that bounary nodes can only be matched by
   boundary nodes and the types of the lhs and rhs must be equal to be
   a rule. *)
fun is_trivial rule = raise ERROR "not implemented"; (* FIXME *)
(* 
    let val init_renaming = 
        val rhs' = BangGraph.rename_apart
    in
    (case Seq.pull (MatchSearch.match (get_lhs rule) (get_rhs rule) 
                                     MatchSearch.Vertex.NTab.empty)
      of NONE => false
       | _ => true);
*)

fun pretty (Rule rep) = 
    Pretty.chunks 
    [Pretty.block [Pretty.str "Rule: "],
     Pretty.block [Pretty.str "LHS: ", BangGraph.pretty (#lhs rep)],
     Pretty.block [Pretty.str "RHS: ", BangGraph.pretty (#rhs rep)]];

val print = Pretty.writeln o pretty;

end;


structure DB_Rule = RuleFun(RGGraph);
structure Rule : RULE = DB_Rule;
