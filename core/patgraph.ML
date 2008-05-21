(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Matching treats graphs as undirected: IMPROVE: chnage graph rep to
be undirected and have direction as edge data, w.r.t. order of names
of unordered pair of vertices being connected. *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature PATGRAPH = 
sig

include BGRAPH
structure MCtxt : MATCH_CTXT 
sharing MCtxt.Vertex = Vertex
sharing MCtxt.Edge = Edge

type match;

val match : T -> T -> match -> match Seq.seq
(* val instantiate : match -> T -> T *)

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

structure Graph = RGGraph;
structure Vertex = RGGraph.Vertex;

(* BVertex = Vertex, but has boundary consturctors declared *)
structure BVertex = RGGraph.BVertex;

(* RGVertex = BVertex.BasicVert, but has RG consturctors *)
structure RGVertex = RGGraph.RGVertex; 
structure Edge = RGGraph.Edge;
structure MCtxt = AngleMatchCtxt;

structure Match = 
struct
(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype T = Match of 
         {(* pat to tgt - why do ne need the data? *)
          vmap: Vertex.name Vertex.NTab.T, 
          (* tgt to pat for internal (1-1 matching) vertices *)
          (* [many pat boundary vertices can go to 1 tgt vertex] *)
          ivinvmap : Vertex.name Vertex.NTab.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T 
         };

val empty = Match {vmap = Vertex.NTab.empty,
                   ivinvmap = Vertex.NTab.empty,
                   mctxt = MCtxt.empty
                  };

fun update_vmap f (Match rep) = 
    Match {vmap = f (#vmap rep), 
           ivinvmap = #ivinvmap rep,
           mctxt = #mctxt rep};
fun update_ivinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           ivinvmap = f (#ivinvmap rep),
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {vmap = #vmap rep, 
           ivinvmap = #ivinvmap rep,
           mctxt = f (#mctxt rep)};

fun get_vmap (Match rep) = #vmap rep;
fun get_ivinvmap (Match rep) = #ivinvmap rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;

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
       @ [Pretty.block [Pretty.str "MCtxt: ", MCtxt.pretty (get_mctxt m)]]);
    
val print = Pretty.writeln o pretty;

end; (* Match structure *)


structure MatchState 
= struct

datatype T = 
         MState of 
         {(* agenda = (tgt,patt) list = vertices commited to matching each other *) 
          agenda : (Vertex.name * Vertex.name) list, (* and-agenda: do all these *)
          match : Match.T, (* global vertex and edge matching info *)
          unm_pat_vs : Vertex.NSet.T, (* unmatched pat vertices *)
          unm_tgt_vs : Vertex.NSet.T, (* unmatched tgt vertices *)
          unm_tgt_es : Edge.NSet.T (* unmatched tgt edges *)
         };

fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_unm_pat_vs (MState rep) = #unm_pat_vs rep;
fun get_unm_tgt_vs (MState rep) = #unm_tgt_vs rep;
fun get_unm_tgt_es (MState rep) = #unm_tgt_es rep;

fun init match pat tgt agenda =
    MState { agenda = agenda, match = match,
             unm_pat_vs = (Vertex.NTab.get_names (Graph.get_vertices pat)),
             unm_tgt_vs = (Vertex.NTab.get_names (Graph.get_vertices tgt)),
             unm_tgt_es = (Edge.NTab.get_names (Graph.get_edges tgt))
           };

fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             unm_pat_vs = #unm_pat_vs rep,
             unm_tgt_vs = #unm_tgt_vs rep,
             unm_tgt_es = #unm_tgt_es rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             unm_pat_vs = #unm_pat_vs rep,
             unm_tgt_vs = #unm_tgt_vs rep,
             unm_tgt_es = #unm_tgt_es rep
           };
fun update_unm_pat_vs f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat_vs = f (#unm_pat_vs rep),
             unm_tgt_vs = #unm_tgt_vs rep,
             unm_tgt_es = #unm_tgt_es rep
           };
fun update_unm_tgt_vs f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat_vs = #unm_pat_vs rep,
             unm_tgt_vs = f(#unm_tgt_vs rep),
             unm_tgt_es = #unm_tgt_es rep
           };
fun update_unm_tgt_es f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat_vs = #unm_pat_vs rep,
             unm_tgt_vs = #unm_tgt_vs rep,
             unm_tgt_es = f(#unm_tgt_es rep)
           };

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_ivinvmap = Match.get_ivinvmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

fun tgt_edges_are_unmatched st edges = 
    Edge.NSet.sub_set edges (get_unm_tgt_es st);

(* implicit: st n *)
val tgt_vertex_is_unmatched = 
    Vertex.NSet.contains o get_unm_tgt_vs;

(* implicit: tgt_edges st *)
val useup_tgt_edges = 
    update_unm_tgt_es o (fn subtractthese => fn from_tgt_es => 
                         Edge.NSet.subtract from_tgt_es subtractthese);
(* implicit: v st *)
val useup1_tgt_vertex = update_unm_tgt_vs o Vertex.NSet.delete;
val useup1_pat_vertex = update_unm_pat_vs o Vertex.NSet.delete;
(* implicit: (patv*tgtv) st *)
val add1_to_agenda = update_agenda o (fn a1 => fn agenda => a1 :: agenda);
(* implicit: (patv*tgtv) st *)
val add1_match = update_match o Match.update_vmap o Vertex.NTab.ins;

fun pop_agenda st = 
    case get_agenda st 
     of [] => NONE 
      | (h::t) => SOME (h,set_agenda t st);

(* try match two internal verteices data, using internal data matching
   function from MCxtx *)
fun try_add_matched_ivdata (n,d) (tn,td) st = 
    (case MCtxt.vertex_match d td (get_match_mctxt st)
      of NONE => NONE (* vertices don't match *)
       | SOME mctxt2 => 
         SOME (st |> update_match (Match.set_mctxt mctxt2
                                   o Match.update_vmap (Vertex.NTab.ins (n,tn))
                                   o Match.update_ivinvmap (Vertex.NTab.ins (tn,n)))
                  |> useup1_tgt_vertex tn));

(* assumes patv are uninstantiated *)
(* raises Vertex.NTab...DUP exception, if n1 is already insantiated *)
(* Assumes each patvertex is only examined once; therefore assume not in vmap *)
fun match_vertex (n, BVertex.BVert _) (tn, _) st = 
    SOME (add1_match (n,tn) st)
  | match_vertex (n, BVertex.IVert d) (tn, BVertex.IVert td) st = 
    if tgt_vertex_is_unmatched st tn then 
      try_add_matched_ivdata (n,d) (tn,td) st
    else NONE (* some internal vertex is already matched to the target *)
  | match_vertex (_, BVertex.IVert _) (_,BVertex.BVert _) m = 
    NONE; (* instantiated vertex cannot match a bvertex (half-edge) *)

(* [a,b,c,d] => [(a, [b,c,d]), (b, [a,c,d]), (c,[a,b,d]), ... ] *)
fun pull_each [] = []
  | pull_each (h::t) = 
    (h,t)::(map (fn (h2,t2) => (h2,h::t2)) (pull_each t));

(* st = matching state; moretgtvns = more tgt vertices to be matched;
(ntgt,estgt) = target vertex and edges being considerd; (npat,espat) =
pattern vertex and edges being considered. *)
fun locally_match_2vs st ((npat,espat),((ntgt,estgt),moretgtvns))  = 
    let val these_tgt_edges = (Edge.NTab.get_names estgt) 
    in (* note: edge matching is alwasy strict, edges can never be matched many times *)
      if tgt_edges_are_unmatched st these_tgt_edges then 
        let 
          val st2 = st |> add1_to_agenda (npat,ntgt)
                       |> useup_tgt_edges these_tgt_edges
        in
          Seq.map (fn m2 => (moretgtvns, set_match_mctxt m2 st2))
                  (MCtxt.strict_multi_edge_match espat estgt (get_match_mctxt st2))
        end
      else Seq.empty
    end;

(* Find some way to match up the adjacent vertices which is strict on
edges, that is that for any given pair of vertices in the pattern every
edge between them is mapped to an edge between the corresponding ones
in the tgt.

assumes a given proposed vertex matching (proposed pat matches
proposed tgt), we consider vertices adjacent to proposed-tgt and those
adjacent to proposed-pat - these must match each other, but for
non-strict matching, there may be extra vertices (and thus edges) in
the tgt.

   1. patvns = list adjacent pattern vertices with connecting edges, 
      Vertex.name * (Edge.data Edge.NTab.T) list
   2. tgtvns (so far unmatch-edges) 
      = list adjacent target vertices with connecting edges
      Vertex.name * (Edge.data Edge.NTab.T) list
   3. and-agenda holding a matching up of patterns and adjacent vertices 
      (Vertex.name * Vertex.name) list
   4. match state so far for any match context vars
*)
(* nonstrict: extra adjacent vertices in tgt allows *)
fun locally_match_verticies [] (unmatchedges,st) = Seq.single st
  | locally_match_verticies (h::t) ([],st) = Seq.empty
  | locally_match_verticies (hpat::patvns) (tgtvns,st) = 
    (Seq.of_list (map (pair hpat) (pull_each tgtvns)))
      |> Seq.maps (locally_match_2vs st) (* or-choices for extending state *)
      |> Seq.maps (locally_match_verticies patvns); (* extend match for each *)

(* no extra adjacent vertices in tgt allowed *)
fun strict_locally_match_verticies [] ([],st) = Seq.single st
  | strict_locally_match_verticies [] ((h::t),st) = Seq.empty
  | strict_locally_match_verticies (h::t) ([],st) = Seq.empty
  | strict_locally_match_verticies (hpat::patvns) (tgtvns,st) = 
    (Seq.of_list (map (pair hpat) (pull_each tgtvns)))
      |> Seq.maps (locally_match_2vs st) (* or-choices for extending state *)
      |> Seq.maps (strict_locally_match_verticies patvns); (* extend match for each *)


(* assumes: pat_ns are all uninstantiated *)
fun match_2vertices_with_edges 
      pat tgt 
      n (* pattern vertex name *)
      st (* match state so far *)
      tn = (* target vertex name *)
    let 
      val (d,edgens) = Graph.get_vertex pat n;
      val (td,tedgens) = Graph.get_vertex tgt tn;
    in
      case match_vertex (n,d) (tn,td) st of 
        NONE => Seq.empty (* vertex does not match *)
      | SOME st2 => (* continue matching *)
        (case d 
          of (BVertex.BVert _) => (* allow extra edges in tgt *)
             locally_match_verticies
               (Vertex.NTab.dest (Graph.adj_vertices' pat edgens))
               ((Vertex.NTab.dest (Graph.adj_vertices' tgt tedgens)),
                useup1_pat_vertex n st2)
           | (BVertex.IVert _) => (* disallow extra edges in tgt *)
             strict_locally_match_verticies
               (Vertex.NTab.dest (Graph.adj_vertices' pat edgens))
               ((Vertex.NTab.dest (Graph.adj_vertices' tgt tedgens)),
                useup1_pat_vertex n st2))
    end;


exception badly_formed_pattern_exp of string * T;

datatype matchstep =
         FoundMatch of T
       | Progress of T list

fun do_next_match_choices pat tgt st = 
    (case pop_agenda st 
      of NONE => 
         (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
            NONE => FoundMatch st
          | SOME pvn => 
            Progress
              (Seq.list_of 
                 (Seq.maps
                    (match_2vertices_with_edges pat tgt pvn st)
                    (Seq.of_list (Vertex.NSet.dest (get_unm_tgt_vs st))))))
       | SOME ((n,tn),st2) => 
         Progress 
           (Seq.list_of (match_2vertices_with_edges pat tgt n st2 tn)));

fun match_search pat tgt [] () = NONE
  | match_search pat tgt (st::sts) () = 
    (case do_next_match_choices pat tgt st of 
       FoundMatch st2 => 
       SOME (get_match st2, Seq.make (match_search pat tgt sts))
     | Progress morests => 
       match_search pat tgt (morests@sts) ());

fun match' initmatching pat tgt initagenda = 
    let val init_st = (init initmatching pat tgt initagenda)
    in Seq.make (match_search pat tgt [init_st]) end;

val match = match' Match.empty;

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

fun pretty (st as MState rep) = 
    Pretty.block 
      [Pretty.str "MatchState {",
       Pretty.chunks 
         [Pretty.block [Pretty.str "Agenda: ",
                        Pretty.list 
                          "[" "]" (map pretty_match_pair (#agenda rep))],
          Match.pretty (#match rep),
          Pretty.block [Pretty.str "Unmatched pat vs: ",
                        Vertex.NSet.pretty (#unm_pat_vs rep)],
          Pretty.block [Pretty.str "Next pat vertex: ",
                        pretty_next_pat_vertex st],
          Pretty.block [Pretty.str "Unmatched tgt vs: ",
                        Vertex.NSet.pretty (#unm_tgt_vs rep)],
          Pretty.block [Pretty.str "Unmatched tgt edges: ",
                        Edge.NSet.pretty (#unm_tgt_es rep)]],
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
