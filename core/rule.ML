(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* rules for rewriting graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature RULE 
= sig

structure MatchState : MATCH_STATE
structure RGGraph : RG_PATGRAPH
structure Match : MATCH

sharing RGGraph = MatchState.Graph;
sharing Match = MatchState.Match;

type T (* rules *)

val mk : string * (RGGraph.T * RGGraph.T) -> T;

val get_lhs : T -> RGGraph.T
val get_rhs : T -> RGGraph.T

val get_name : T -> string;
val set_name : string -> T -> T;

val get_rhs_newnodes : T -> NName.NTab.N 
val get_lhs_delnodes : T -> NName.NTab.N

val get_rhs_newedges : T -> EName.NTab.N
val get_lhs_deledges : T -> EName.NTab.N

(* swaps lhs and rhs *)
val mk_symmetric : string (* new name *)
          -> T -> T

(* find all matches, evaluated lazily *)
val matches : T -> RGGraph.T -> RGGraph.match Seq.seq

(* rename rule w.r.t. matching of tgt *)
val matchify_rule : RGGraph.T -> Match.T -> T -> T

(* assume rule has been renamed (by matchify) to correspond to tgt graph,
   now add the rhs and delete the lhs *)
val add_rhs : T -> RGGraph.T -> RGGraph.T
val del_lhs : T -> RGGraph.T -> RGGraph.T

(* rewriting pattern graph *)
(* assumes rule has been renamed (by matchify) to correspond to tgt
graph; given matchified rule, target graph, produces rewritten graph *)
val do_rewrite : T -> RGGraph.T -> RGGraph.T


val rewrites : T -> RGGraph.T -> RGGraph.T Seq.seq

(* 
val do_rewrite_rrhs : (* rewriting a rules rhs *)
    T (* use this rule *)
    -> Match.T (* with this matching *)
    -> T (* to rewrite the rhs of this rule *)
    -> T (* to produce this new rule *)

val rewritings : T -> (MatchState.T * RGGraph.T) -> (MatchState.T * RGGraph.T) Seq.seq
val rewritings_rrhs : T -> (MatchState.T * T) -> (MatchState.T * T) Seq.seq
*)

end;


functor Rule(RGGraph : RG_PATGRAPH)
= struct

structure RGGraph = RGGraph;

datatype T = Rule of { name : string,
                       lhs : RGGraph.T,
                       rhs : RGGraph.T };

fun mk (n,(lhs,rhs)) = 
    Rule { name = n, lhs = lhs, rhs = rhs };

(* match anything = no set matching *)
val any_match = Match.empty;

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;
fun get_name (Rule rep) = #name rep;

fun update_lhs f (Rule rep) = 
    Rule { name = #name rep, lhs = f (#lhs rep), rhs = #rhs rep };
fun update_rhs f (Rule rep) = 
    Rule { name = #name rep, lhs = #lhs rep, rhs = f (#rhs rep) };
fun update_name f (Rule rep) = 
    Rule { name = f (#name rep), lhs = #lhs rep, rhs = #rhs rep };

val set_lhs = update_lhs o K;
val set_rhs = update_rhs o K;
val set_name = update_name o K;

fun get_nnames r = 
    ((get_lhs r) 
      |> RGGraph.get_nodes 
      |> NName.NTab.get_names,
     (get_rhs r) 
       |> RGGraph.get_nodes 
       |> NName.NTab.get_names);

fun get_enames r =
   ((get_lhs r) 
      |> RGGraph.get_edges 
      |> EName.NTab.get_names,
    (get_rhs r) 
      |> RGGraph.get_edges 
      |> eName.NTab.get_names);

(* THINK: cache this info? *)
fun get_rhs_newnodes r = 
    let val (lhsns,rhsns) = get_nnames r
    in NName.NTab.N.subtract rhsns lhsns end;
fun get_lhs_delnodes r =
    let val (lhsns,rhsns) = get_nnames r
    in NName.NTab.N.subtract lhsns rhsns  end;
fun get_rhs_newedges r =
    let val (lhsns,rhsns) = get_enames r
    in EName.NTab.N.subtract rhsns lhsns end;
fun get_lhs_deledges r =
    let val (lhsns,rhsns) = get_enames r
    in EName.NTab.N.subtract lhsns rhsns  end;


fun mk_symmetric n r = mk (n,(get_rhs r, get_lhs r));


(* find all matches, evaluated lazily *)
fun matches rule tgt = 
    MatchState.match pat tgt MatchState.Vertex.NTab.empty;


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


(* rename rule w.r.t. matching of tgt - assumes rule was named distinct from tgt *)
fun matchify_rule tgt match rule = 
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

      val rhs = get_rhs rule
      val tgt_internals = Vertex.NSet.subtract (RGGraph.get_vnames tgt) (RGGraph.get_boundary tgt)
    in
      
    end;

(* assume rule has been renamed (by matchify) to correspond to tgt graph,
   now add the rhs and delete the lhs *)
val add_rhs : T -> RGGraph.T -> RGGraph.T
val del_lhs : T -> RGGraph.T -> RGGraph.T

(* rewriting pattern graph *)
(* assumes rule has been renamed (by matchify) to correspond to tgt
graph; given matchified rule, target graph, produces rewritten graph *)
val do_rewrite : T -> RGGraph.T -> RGGraph.T


val rewrites : T -> RGGraph.T -> RGGraph.T Seq.seq



(* 
val do_rewrite_rrhs : (* rewriting a rules rhs *)
 
val rewritings : T -> (match * RGGraph.T) -> (match * RGGraph.T) Seq.seq
val rewritings_rrhs : T -> (match * T) -> (match * T) Seq.seq
*)

end;
