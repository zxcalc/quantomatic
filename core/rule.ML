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
 
signature RULE 
= sig

structure MatchState : MATCH_STATE
(* these are really in MatchState - this is just a local abbreviation *)
structure RGGraph : RG_GRAPH
structure Match : MATCH
sharing RGGraph.IVertex = MatchState.RGGraph.IVertex;
sharing RGGraph.Edge = MatchState.RGGraph.Edge;
sharing Match = MatchState.Match;

type T (* rules *)

exception bad_rule_exp of string * RGGraph.T * RGGraph.T;
val mk : (RGGraph.T * RGGraph.T) -> T;

val get_lhs : T -> RGGraph.T
val get_rhs : T -> RGGraph.T

(* swaps lhs and rhs *)
val symmetric : T -> T

(* rule is a trivial identity : lhs matches rhs *)
val is_trivial : T -> bool

(* find all matches, evaluated lazily *)
val matches : T -> RGGraph.T -> Match.T Seq.seq
(* gives back renamed (renamed to match graph) rules *)
val rule_matches : T -> RGGraph.T -> T Seq.seq
val rule_matches_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> T Seq.seq

(* rename rule w.r.t. matching of tgt *)
val inst_rule : T -> Match.T -> T

(* assume rule has been renamed (by matchify) to correspond to tgt graph,
   now add the rhs and delete the lhs *)
val add_rhs : T -> RGGraph.T -> RGGraph.T
val del_lhs : T -> RGGraph.T -> RGGraph.T

(* rewriting pattern graph *)
val rewrites : T -> RGGraph.T -> RGGraph.T Seq.seq
val rewrites_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> RGGraph.T Seq.seq

exception not_applicable_exp of string * T * RGGraph.T;
val fst_rewrite : T -> RGGraph.T -> RGGraph.T
val fst_rewrite_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> RGGraph.T



(* 
val do_rewrite_rrhs : (* rewriting a rules rhs *)
    T (* use this rule *)
    -> Match.T (* with this matching *)
    -> T (* to rewrite the rhs of this rule *)
    -> T (* to produce this new rule *)

val rewritings : T -> (MatchState.T * RGGraph.T) -> (MatchState.T * RGGraph.T) Seq.seq
val rewritings_rrhs : T -> (MatchState.T * T) -> (MatchState.T * T) Seq.seq
*)

val pretty : T -> Pretty.T
val print : T -> unit


end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor RuleFun( MatchState : MATCH_STATE )
= struct

structure MatchState = MatchState;
(* local stuff for abbreviations *)
structure RGGraph = MatchState.RGGraph;
structure Match = MatchState.Match;
structure Vertex = MatchState.Vertex;
structure Edge = MatchState.Edge;

datatype T = Rule of { lhs : RGGraph.T,
                       rhs : RGGraph.T };

exception bad_rule_exp of string * RGGraph.T * RGGraph.T;
fun mk (lhs,rhs) = 
    let val lhsbndry = RGGraph.get_boundary lhs in
    (* check that rule is well-formed! *)
    if Vertex.NSet.eq lhsbndry (RGGraph.get_boundary rhs) then 
      let (* rename rhs; shared bvertex names, all else is distinct. *)
        val vrn = Vertex.mk_renaming lhsbndry (RGGraph.get_vnames lhs) 
                                     Vertex.NTab.empty
        val ern = Edge.mk_renaming Edge.NSet.empty 
                                   (RGGraph.get_enames lhs) 
                                   Edge.NTab.empty
        val (rn, rhs') = RGGraph.rename (vrn,ern) rhs
      in Rule { lhs = lhs, rhs = rhs' } end
    else
      raise bad_rule_exp ("mk: Left and right hand side boundaries are different", lhs, rhs)
    end;

(* match anything = no set matching *)
val any_match = Match.empty;

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;

fun update_lhs f (Rule rep) = 
    Rule { lhs = f (#lhs rep), rhs = #rhs rep };
fun update_rhs f (Rule rep) = 
    Rule { lhs = #lhs rep, rhs = f (#rhs rep) };

val set_lhs = update_lhs o K;
val set_rhs = update_rhs o K;

(* THINK: cache this info? *)
(* 
fun get_rhs_newnodes r = 
    let val (lhsns,rhsns) = RGGraph.get_vnames (get_rhs r)
    in Vertex.NSet.subtract rhsns lhsns end;
fun get_lhs_delnodes r =
    let val (lhsns,rhsns) = RGGraph.get_vnames (get_lhs r)
    in Vertex.NSet.subtract lhsns rhsns  end;
fun get_rhs_newedges r =
    let val (lhsns,rhsns) = RGGraph.get_enames (get_rhs r)
    in Edge.NSet.subtract rhsns lhsns end;
fun get_lhs_deledges r =
    let val (lhsns,rhsns) = RGGraph.get_enames (get_lhs r)
    in Edge.NSet.subtract lhsns rhsns  end;
*)

(* swap lhs and rhs of a rule *)
fun symmetric r = mk (get_rhs r, get_lhs r);

(* rename a rule to avoid all names in a graph *)
fun rename_rule_apart g rule = 
    let 
      val (rn,lhs2) = RGGraph.rename_apart g (get_lhs rule)
      val (rn2, rhs2) = RGGraph.rename rn (get_rhs rule)
    in 
      (rn2, rule |> set_lhs lhs2
                 |> set_rhs rhs2)
    end;

(* find all matches between a rule's lhs and a tgt, evaluated lazily *)
fun matches rule tgt = 
    MatchState.match (get_lhs rule) tgt MatchState.Vertex.NTab.empty;

(* a rule is trivially true if the left and right hand sides match.
   Proof is by observing that bounary nodes can only be matched by
   boundary nodes and the types of the lhs and rhs must be equal to be
   a rule. *)
fun is_trivial rule = true
(* 
    let val init_renaming = 
        val rhs' = RGGraph.rename_apart
    in
    (case Seq.pull (MatchState.match (get_lhs rule) (get_rhs rule) 
                                     MatchState.Vertex.NTab.empty)
      of NONE => false
       | _ => true);
*)

(* clever efficient renaming of lhs to tgt *)
(* 
      (* terminates because renaming is not cyclic (no swaps!), well formed
         because provide an ordering on how renaming happens which avoids
         clashes. *)
      fun matchify_lhs_namelist [] lhs = lhs
        | matchify_lhs_namelist ((n1,n2)::rns) lhs = 
          RGGraph.rename_vname n1 n2 lhs;
          
      (* rename lhs to be same as tgt graph *)
      fun matchify_lhs_name (waitlist,vmap,lhs) = 
          (case (Vertex.NTab.pull_elem vmap) of 
             NONE => matchify_lhs_namelist waitlist lhs
           | SOME ((n1,n2),vmap') => 
             if RGGraph.has_vname lhs n2 then 
               matchify_lhs_name ((n1,n2)::waitlist) vmap' lhs
             else 
               matchify_lhs_name waitlist vmap' (RGGraph.rename_vname n1 n2 lhs))
 *)



(* rename rule w.r.t. matching of tgt - assumes rule was named distinct 
   from tgt *)
fun inst_rule rule match = 
    let 
      val vmap = Match.get_vmap match
      val emap = Match.get_emap match

      val lhs = (get_lhs rule)
                  |> (Vertex.NTab.fold 
                        (fn (n1,n2) => RGGraph.rename_vname n1 n2)
                        vmap)
                  |> (Edge.NTab.fold 
                        (fn (n1,n2) => RGGraph.rename_ename n1 n2)
                        emap)

      (* only boundary nodes are mapped/renamed on rhs *)
      val oldrhs = get_rhs rule;
      val rhs = oldrhs
                  |> (Vertex.NSet.fold 
                        (fn n1 => RGGraph.rename_vname n1 (Vertex.NTab.get vmap n1))
                        (RGGraph.get_boundary oldrhs))
    in
      rule |> set_lhs lhs
           |> set_rhs rhs
    end;

(* renamings of rule so that it macthes tgt *)
fun rule_matches rule tgt = 
    let val (rn, rule') = (rename_rule_apart tgt rule)
    in Seq.map (inst_rule rule')
               (matches rule' tgt)
    end;


fun rule_matches_within rule vns tgt = 
    let val (rn, rule') = (rename_rule_apart tgt rule)
    in Seq.map (inst_rule rule')
               (matches rule' (RGGraph.subgraph_of_selection vns tgt))
    end;


(* assume rule has been renamed (by matchify) to correspond to tgt graph,
   now add the rhs and delete the lhs *)
fun add_rhs rule tgt = RGGraph.merge_by_vertices (get_rhs rule) tgt;
fun del_lhs rule tgt = 
    let val lhs = (get_lhs rule) 
        val internal_lhs_vns = 
            Vertex.NSet.subtract (RGGraph.get_vnames lhs) (RGGraph.get_boundary lhs) 
    in Vertex.NSet.fold RGGraph.delete_vertex internal_lhs_vns tgt end;

(* rewriting pattern graph *)

(* all rewrites of g using rule *)
fun rewrites rule g = 
    Seq.map 
      (fn matched_rule => 
          g |> (add_rhs matched_rule)
            |> (del_lhs matched_rule)) 
      (rule_matches rule g);

fun rewrites_within rule vns g = 
    Seq.map 
      (fn matched_rule => 
          g |> (add_rhs matched_rule)
            |> (del_lhs matched_rule))
      (rule_matches_within rule vns g);

exception not_applicable_exp of string * T * RGGraph.T;

fun fst_rewrite rule g = 
    (case Seq.pull (rewrites rule g) of 
       NONE => raise not_applicable_exp ("find_and_apply_rewrite",rule,g)
     | SOME (g2,g2s) => g2);

fun fst_rewrite_within rule vns g = 
    (case Seq.pull (rewrites_within rule vns g) of 
       NONE => raise not_applicable_exp ("find_and_apply_rewrite_within",rule,
                                         RGGraph.subgraph_of_selection vns g)
     | SOME (g2,g2s) => g2);


(* 
val do_rewrite_rrhs : (* rewriting a rules rhs *)
 
val rewritings : T -> (match * RGGraph.T) -> (match * RGGraph.T) Seq.seq
val rewritings_rrhs : T -> (match * T) -> (match * T) Seq.seq
*)


fun pretty (Rule rep) = 
    Pretty.chunks 
    [Pretty.block [Pretty.str "Rule: "],
     Pretty.block [Pretty.str "LHS: ", RGGraph.pretty (#lhs rep)],
     Pretty.block [Pretty.str "RHS: ", RGGraph.pretty (#rhs rep)]];

val print = Pretty.writeln o pretty;

end;


structure DB_Rule = RuleFun(MatchState);
structure Rule : RULE = DB_Rule;
