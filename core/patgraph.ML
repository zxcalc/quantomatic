(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Matching treats graphs as undirected: IMPROVE: chnage graph rep to
   be undirected and have direction as edge data, w.r.t. order of
   names of unordered pair of vertices being connected.

Simple cases: those where number of vertices between edges are matched
exactly and no chasing of boundary vertices is needed.

Simple case: 
Ba -3- Xb = Na -3- Nb

Complex case needs B-Vertex tentacle chasing: 
Ba -1- Bb -1- Bc -1- Bd = Na -3- Nb

FIXME: Consider how a semi-local strictness condition can be
included: this is needed to avoid large (symmetric) failure
space. E.g. consider pattern of two disconected graphs (although the
same issue also comes up in connected grahs) : if one fails to match,
we will keep searching for match of the other one and only fail at the
end when we realise we have extra unmatched target edges. This can be
fixed by pull-back links annotated in pattern graph. The way
pull-backs work is that you know the unmatched boundary edges and
where they can reach to add extra edges. This gives a local-matching
criteria so that we can be locally strict. Local matching can then
have some non-local effects: causes a "ver-vertex" tentacle to become
folded back. This gives efficient failure.

*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature RG_PATGRAPH = 
sig
include RG_GRAPH
structure MCtxt : MATCH_CTXT 
sharing MCtxt.Vertex = IVertex
sharing MCtxt.Edge = Edge
(*
type match;
val match : T -> T -> match -> match Seq.seq
 val instantiate : match -> T -> T 
*)
end;

structure RGPatGraph : RG_PATGRAPH
= struct 
open RGGraph;
structure MCtxt = AngleMatchCtxt;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* 
functor PatGraphFun(
structure BGraph : BGRAPH
structure MCtxt : MATCH_CTXT
sharing Match.Vertex = BGraph.Vertex
sharing Match.Edge = BGraph.Edge)
: PATGRAPH
= struct

open BGraph;
structure MCtxt = MCtxt;
*)

structure Graph = RGPatGraph : RG_PATGRAPH;
structure Vertex = Graph.RGVertex;
structure Edge = Graph.Edge;
structure MCtxt = Graph.MCtxt;

structure Match = 
struct
(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype T = Match of 
         {(* pat to tgt - do we need the data? *)
          vmap: Vertex.name Vertex.NTab.T, 
          emap: Edge.name Edge.NTab.T, 
          (* tgt to pat for internal (1-1 matching) vertices *)
          (* [many pat boundary vertices can go to 1 tgt vertex] *)
          ivinvmap : Vertex.name Vertex.NTab.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T 
         };

val empty = Match {vmap = Vertex.NTab.empty,
                   emap = Edge.NTab.empty,
                   ivinvmap = Vertex.NTab.empty,
                   mctxt = MCtxt.empty
                  };

fun update_vmap f (Match rep) = 
    Match {vmap = f (#vmap rep), 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = f (#emap rep),
           ivinvmap = #ivinvmap rep,
           mctxt = #mctxt rep};
fun update_ivinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = f (#ivinvmap rep),
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           mctxt = f (#mctxt rep)};

fun get_vmap (Match rep) = #vmap rep;
fun get_emap (Match rep) = #emap rep;
fun get_ivinvmap (Match rep) = #ivinvmap rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;

(* implicit: m n *)
val tgt_vertex_is_unmatched = not oo Vertex.NTab.contains o get_ivinvmap;

(* try match two internal verteices data, using internal data matching
   function from MCxtx *)
fun match_vertex_idata (n,d) (tn,td) m = 
    (case MCtxt.vertex_match d td (get_mctxt m)
      of NONE => NONE (* vertices don't match *)
       | SOME mctxt2 => 
         SOME (m |> set_mctxt mctxt2
                 |> update_vmap (Vertex.NTab.ins (n,tn))
                 |> update_ivinvmap (Vertex.NTab.ins (tn,n))));

fun match_bvertex n1 n2 m = 
    update_vmap (Vertex.NTab.ins (n1,n2)) m;

fun match_edges pat_edges tgt_edges m = 
    Seq.map
      (fn ((_,ematch),mctxt2) => 
          (ematch, 
           m |> update_emap (Edge.NTab.merge_disjoint ematch)
             |> set_mctxt mctxt2))
      (MCtxt.multi_edge_match pat_edges tgt_edges (get_mctxt m));

fun pretty m = 
    Pretty.chunks 
      ([Pretty.block 
          [Pretty.str "VMatchings: ",
           Pretty.list "{" "}" 
                       (map (fn (pn,tn) => 
                                Pretty.block 
                                  [Pretty.str "(",
                                   Vertex.pretty_name pn,
                                   Pretty.str "->",
                                   Vertex.pretty_name tn, 
                                   Pretty.str ")"])
                            (Vertex.NTab.dest (get_vmap m)))]]
       @ [Pretty.str "EMatchings: ",
          Pretty.list "{" "}" 
                       (map (fn (pn,tn) => 
                                Pretty.block 
                                  [Pretty.str "(",
                                   Edge.pretty_name pn,
                                   Pretty.str "->",
                                   Edge.pretty_name tn, 
                                   Pretty.str ")"])
                            (Edge.NTab.dest (get_emap m)))]
       @ [Pretty.block [Pretty.str "MCtxt: ", MCtxt.pretty (get_mctxt m)]]);
    
val print = Pretty.writeln o pretty;

end; (* Match structure *)


structure MatchState 
= struct

datatype T = 
         MState of 
         {(* agenda = (tgt -> patt) = vert commited to matching each other *) 
          agenda : Vertex.name Vertex.NTab.T, (* and-agenda: pat -> tgt *)
          (* global vertex and edge matching info *)
          match : Match.T, 
          (* edges in tgt that still need to be matched *)
          needed_tgt_es : Edge.NSet.T,
          (* the pat graph with vertices removed as we match them *)
          unm_pat : Graph.T, 
          (* tgt with matched edges removed as we match them *)
          unm_tgt : Graph.T 
         };

(* initial matching state *)
fun init match pat tgt agenda =
    MState { agenda = agenda, match = match,
             needed_tgt_es = Edge.NSet.empty,
             unm_pat = pat, unm_tgt = tgt };

(* tools *)
fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_unm_pat (MState rep) = #unm_pat rep;
val get_unm_pat_vs = 
    Vertex.NTab.get_names o Graph.get_vertices o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_names o Graph.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    Edge.NTab.get_names o Graph.get_edges o get_unm_tgt;
fun get_needed_tgt_es (MState rep) = #needed_tgt_es rep;


(* *)
fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_unm_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = f (#unm_pat rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = f(#unm_tgt rep)
           };
fun update_needed_tgt_es f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = f (#needed_tgt_es rep),
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_ivinvmap = Match.get_ivinvmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

(* true at end of successful matching: this ensures the strictness
   condition for edge matching (only refers to strictly needed
   edges). *)
val no_extra_tgt_edges = Edge.NSet.is_empty o get_needed_tgt_es;

(* *)
fun get_adj_pat_vertices st edgens =
    Graph.adj_vnames' (get_unm_pat st) edgens;
fun get_adj_tgt_vertices st edgens =
    Graph.adj_vnames' (get_unm_tgt st) edgens;

(* implicit: st n *)
val tgt_vertex_is_unmatched = 
    not oo Match.tgt_vertex_is_unmatched o get_match;

(* implicit: tgt_edges st *)
val useup_tgt_edges = update_unm_tgt o Edge.NSet.fold Graph.delete_edge;
val useup_pat_edges = update_unm_pat o Edge.NSet.fold Graph.delete_edge;

(* unmatched but needed edges in the pattern *)
(* : Edge.NSet.T -> T -> T *)
val add_needed_edges' = update_needed_tgt_es o Edge.NSet.union_merge;
fun add_needed_edges (ins,outs) = 
    update_needed_tgt_es (Edge.NSet.union_merge ins 
                          o Edge.NSet.union_merge outs);
val useup_needed_edges = update_needed_tgt_es o Edge.NSet.remove_set;

(* implicit: v st *)
val useup1_pat_vertex = update_unm_pat o Graph.delete_vertex;
(* implicit: (patv*tgtv) st *)
fun add1_to_agenda (pat_n, tgt_n) st = 
    update_agenda (Vertex.NTab.ins (pat_n,tgt_n)) st;

(* pop first vertex pair from agenda *)
fun pop_agenda st = 
    (case Vertex.NTab.pull_elem (get_agenda st)
      of NONE => NONE 
       | SOME (h,agenda2) => SOME (h,set_agenda agenda2 st));

(* st = matching state; moretgtvns = more tgt vertices to be matched;
(ntgt,estgt) = target vertex and edges being considerd; (npat,espat) =
pattern vertex and edges being considered. *)
fun locally_match_2vs (from_pn, to_pn) from_tn to_tn st = 
    let 
      val pat_edges = Graph.edges_between (get_unm_pat st) from_pn to_pn
      val tgt_edges = Graph.edges_between (get_unm_tgt st) from_tn to_tn
    in
      (Match.match_edges pat_edges tgt_edges (get_match st))
        |> Seq.map 
        (fn (ematches,m2) => 
            let 
              val matched_tgt_edges = 
                  Edge.NTab.fold (Edge.NSet.add o snd) ematches Edge.NSet.empty
              val matched_pat_edges = 
                  Edge.NTab.get_names ematches
            in
              st |> add1_to_agenda (to_pn,to_tn)
                 |> useup_pat_edges matched_pat_edges
                 |> useup_tgt_edges matched_tgt_edges
                 |> set_match m2
                 (* edges were added as needed, we can remove the ones 
                    now matched; they are no longer needed. *)
                 |> (if Graph.is_boundary g from_pn 
                     then useup_needed_edges matched_tgt_edges
                     else I)
            end)
    end;


(* pick first pattern pattern node and then try to match it against
   every possible target node. *)
fun locally_match_verticies (from_n,from_d) (from_tn,from_td) pns tns st = 
    let 
      fun match1 pns (tns, st) = 
          (case Vertex.NSet.pull_local_bot pns
            of NONE => 
               if Vertex.NSet.is_empty tns then Seq.single st
               else Seq.empty
             | SOME (to_pn,pns2) => 
               (Seq.of_list (Vertex.NSet.pull_each tns))
                 |> Seq.maps (fn (to_tn, tns2) => 
                                 (Seq.map 
                                    (pair tns2)
                                    (locally_match_2vs 
                                       (from_n,to_pn) from_tn to_tn st)))
                 |> Seq.maps (match1 pns2))
    in (match1 pns (tns, st)) 
         |> Seq.map (useup1_pat_vertex from_n)
    end;

(* assumes patv are uninstantiated *)
(* raises Vertex.NTab...DUP exception, if n1 is already insantiated *)
(* Assumes each patvertex is only examined once; therefore assume not in vmap *)
fun match_vertex (n, Vertex.BVert _) (tn, _) st = 
    SOME (update_match (Match.match_bvertex n tn) st)
  | match_vertex (n, Vertex.IVert d) (tn, Vertex.IVert td) st = 
    (* map must be injective for internal pat vertices *)
    if tgt_vertex_is_unmatched st tn then 
      case Match.match_vertex_idata (n,d) (tn,td) (get_match st)
       of NONE => NONE
        | SOME m2 => SOME (st |> set_match m2)
    else NONE (* some internal vertex is already matched to the target *)
  | match_vertex (_, Vertex.IVert _) (_,Vertex.BVert _) m = 
    NONE; (* instantiated vertex cannot match a bvertex (half-edge) *)


(* assumes: pat_ns are all uninstantiated *)
(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
fun match_2vertices_with_edges 
      (n : Vertex.name) (* pattern vertex name *)
      (st : T) (* match state so far *)
      (tn : Vertex.name) = (* target vertex name *)
    let 
      val pat = get_unm_pat st
      val (d,edgens) = Graph.get_vertex pat n;
      val (td,tedgens) = Graph.get_vertex (get_unm_tgt st) tn;
    in case match_vertex (n,d) (tn,td) st of 
         NONE => Seq.empty (* vertex does not match *)
       | SOME st2 => (* continue matching *)
         (if Graph.is_boundary pat n then (* then strict matching *)
            add_needed_edges tedgens st2
         else st2) 
           |> (locally_match_verticies (n,d) (tn,td) 
                                       (get_adj_pat_vertices st2 edgens)
                                       (get_adj_tgt_vertices st2 tedgens))
    end;

(* datatype for steps of matching *)
datatype matchstep =
         FoundMatch of T
       | Progress of T Seq.seq (* or choices in matching *)

(* get tgt vertex names as a seq *)
val get_tgt_vnseq = 
    Seq.of_list o Vertex.NSet.dest 
    o Vertex.NTab.get_names 
    o Graph.get_vertices o get_unm_tgt;

(* consider the next and-point in the agenda *)
fun do_next_match_choices st = 
    (case pop_agenda st 
      of NONE => (* nothing on the prescribed agenda *)
         (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
            NONE => (* no pattern vertices left! *)
            if no_extra_tgt_edges st then 
              FoundMatch st (* because we finished! *)
            else (* matching failed with extra left over target edges *)
              Progress Seq.empty
          | SOME pvn => 
            Progress (* choices over picking a new vertex in tgt *)
              (Seq.maps
                 (match_2vertices_with_edges pvn st)
                 (get_tgt_vnseq st)))
       | SOME ((n,tn),st2) => 
         Progress (* deal with first prescribed and-choice *)
           (match_2vertices_with_edges n st2 tn));

(* given an or-choice agenda of states, apply progress steps to search
tree, until we get a match, then give that back as first elem of lazy
list. We can then rest until asked for the next thing. :P *)
fun match_search or_agenda () = 
    (case Seq.pull or_agenda of 
       NONE => NONE
     | SOME (st,more_or_choices) =>
       (case do_next_match_choices st of 
          FoundMatch st2 => 
          SOME (get_match st2, Seq.make (match_search more_or_choices))
        | Progress new_or_choices => 
          match_search (Seq.append new_or_choices more_or_choices) ()));
(* *)
fun match' initmatching pat tgt initagenda = 
    let val init_st = (init initmatching pat tgt initagenda)
    in Seq.make (match_search (Seq.single init_st)) end;

val match = match' Match.empty;

(* pretty printing *)
fun pretty_match_pair (a,b) = 
    Pretty.block [Pretty.str "(",
                  Vertex.pretty_name a,
                  Pretty.str "->",
                  Vertex.pretty_name b,
                  Pretty.str ")"];

fun pretty_next_pat_vertex st = 
    (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
       NONE => Pretty.str "NONE"
     | SOME n => Pretty.block [Pretty.str "SOME (",
                               Vertex.pretty_name n,
                               Pretty.str ")"]);

fun pretty_agenda st = 
    (case Vertex.NTab.pull_elem (get_agenda st) of 
       NONE => Pretty.str "Agenda is empty."
     | SOME ((nextn,_),rest) => 
       Pretty.block 
         [Pretty.str "Agenda: ",
          Vertex.pretty_name nextn,
          Pretty.list " then [" "]" 
                      (map pretty_match_pair (Vertex.NTab.dest rest)),
          Pretty.block [Pretty.str ";  needed tgt edges: ",
                        Edge.NSet.pretty (get_needed_tgt_es  st)]
         ]);

fun pretty (st as MState rep) = 
    Pretty.block 
      [Pretty.str "MatchState {",
       Pretty.chunks 
         [pretty_agenda st,
          Match.pretty (#match rep),
          Pretty.block [Pretty.str "Unmatched pat: ",
                        Graph.pretty (#unm_pat rep)],
          Pretty.block [Pretty.str "Unmatched tgt: ",
                        Graph.pretty (#unm_tgt rep)]],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;

end; (* MatchState structure *)

(* 
end;


structure DB_PatStrGraph = PatGraphFun(
  structure PatNE = PatStrNE
  structure Expr = StrExpr);
structure PatStrGraph :> PATTERN_GRAPH where 
type PatNE.node_pat = PatStrNE.node_pat 
 and type PatNE.edge_pat = PatStrNE.edge_pat 
= DB_PatStrGraph;
*)
