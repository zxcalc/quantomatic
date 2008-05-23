(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Matching treats graphs as undirected: IMPROVE: chnage graph rep to
   be undirected and have direction as edge data, w.r.t. order of
   names of unordered pair of vertices being connected.

CHECK: is this a correct algorithm??? Simple cases: those where number
of vertices between edges are matched exactly and no chasing of
boundary vertices is needed.  

Complex case needs B-Vertex chasing: 
Ba -1- Bb -1- Bc -1- Bd = Na -3- Nb

Simple case: 
Ba -3- Xb = Na -3- Nb

*)

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
          match : Match.T, (* global vertex and edge matching info *)
          needed_tgt_es : Edge.NSet.T,
          unm_pat : Graph.T, (* unmatched part of the pat graph *)
          unm_tgt : Graph.T (* tgt with matched edges removed *)
         };

fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_umn_pat (MState rep) = #umn_pat rep;
val get_unm_pat_vs = Vertex.NTab.get_names o Graph.get_vertices o get_umn_pat;
fun get_umn_tgt (MState rep) = #umn_tgt rep;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_names o Graph.get_vertices o get_umn_tgt;
fun get_unm_tgt_es (MState rep) =
    Edge.NTab.get_names o Graph.get_edges o get_umn_tgt;
fun get_needed_tgt_es (MState rep) = #needed_tgt_es rep;

fun init match pat tgt agenda =
    MState { agenda = agenda, match = match,
             needed_tgt_es = Edge.NSet.empty,
             umn_pat = pat, unm_tgt = tgt };

fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             umn_pat = #umn_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             needed_tgt_es = #needed_tgt_es rep,
             umn_pat = #umn_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_umn_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             umn_pat = f (#umn_pat rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             umn_pat = #umn_pat rep,
             unm_tgt = f(#unm_tgt rep)
           };
fun update_needes_tgt_es f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = f (#needed_tgt_es rep),
             umn_pat = #umn_pat rep,
             unm_tgt = #unm_tgt rep
           };

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_ivinvmap = Match.get_ivinvmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

val no_extra_tgt_edges = Edge.NSet.is_empty o get_needes_tgt_es;

(* implicit: st n *)
val tgt_vertex_is_unmatched = 
    not oo Match.tgt_vertex_is_unmatched o get_match;

(* implicit: tgt_edges st *)
val useup1_tgt_edge = update_unm_tgt o Edge.NSet.fold Graph.delete_edge;

(* implicit: v st *)
val useup1_pat_vertex = update_unm_pat o Graph.delete_vertex;
(* implicit: (patv*tgtv) st *)
fun add1_to_agenda (pat_n, tgt_n) st = 
    update_agenda (Vertex.NTab.ins (pat_n,tgt_n)) st;
(* implicit: (patv*tgtv) st *)

val add_edge_matching = Edge.NSet.is_empty o get_needes_tgt_es;


(* pop first vertex pair from agenda *)
fun pop_agenda st = 
    (case Vertex.NTab.pull_elem (get_agenda st)
      of NONE => NONE 
       | SOME (h,agenda2) => SOME (h,set_agenda agenda2 st));

(* st = matching state; moretgtvns = more tgt vertices to be matched;
(ntgt,estgt) = target vertex and edges being considerd; (npat,espat) =
pattern vertex and edges being considered. *)
fun locally_match_2vs st (from_pn, to_pn) from_tn to_tn = 
    let 
      val st2 = st |> add1_to_agenda (npat,ntgt)
                   |> useup_tgt_edges these_tgt_edges
    in
      Seq.map (fn m2 => (moretgtvns, set_match_mctxt m2 st2))
                  (MCtxt.strict_multi_edge_match espat estgt (get_match_mctxt st2))
        end
      else Seq.empty
    end;

(* pick first pattern pattern node and then try to match it against
   every possible target node. *)
fun locally_match_verticies ((from_n,from_d),pns) ((from_tn,from_td),tns) st = 
    (case Vertex.NSet.pull_local_bot pns
      of NONE => if Vertex.NSet.is_empty tns then Seq.single st
                 else Seq.empty
       | SOME (to_pn,pns2) => 
         (Seq.list_of (Vertex.NSet.dest tns))
           |> Seq.maps (locally_match_2vs st (from_n,to_pn) from_tn))

(* assumes patv are uninstantiated *)
(* raises Vertex.NTab...DUP exception, if n1 is already insantiated *)
(* Assumes each patvertex is only examined once; therefore assume not in vmap *)
fun match_vertex (n, BVertex.BVert _) (tn, _) st = 
    SOME (update_match (Match.match_bvertex n tn) st)
  | match_vertex (n, BVertex.IVert d) (tn, BVertex.IVert td) st = 
    if tgt_vertex_is_unmatched st tn then 
      case Match.match_vertex_idata (n,d) (tn,td) (get_match st)
       of NONE => NONE
        | SOME m2 => set_match m2 st
    else NONE (* some internal vertex is already matched to the target *)
  | match_vertex (_, BVertex.IVert _) (_,BVertex.BVert _) m = 
    NONE; (* instantiated vertex cannot match a bvertex (half-edge) *)

(* [a,b,c,d] => [(a, [b,c,d]), (b, [a,c,d]), (c,[a,b,d]), ... ] *)
fun pull_each [] = []
  | pull_each (h::t) = 
    (h,t)::(map (fn (h2,t2) => (h2,h::t2)) (pull_each t));

fun get_adj_pat_vertices st edgens =
      Graph.adj_vertex_enames' (get_unm_pat_graph st) edgens;
fun get_adj_tgt_vertices st edgens =
      Graph.adj_vertex_enames' (get_unm_tgt_graph st) edgens;

(* assumes: pat_ns are all uninstantiated *)
(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
fun match_2vertices_with_edges 
      n (* pattern vertex name *)
      st (* match state so far *)
      tn = (* target vertex name *)
    let val (d,edgens) = Graph.get_vertex pat n;
        val (td,tedgens) = Graph.get_vertex tgt tn;
    in case match_vertex (n,d) (tn,td) st of 
         NONE => Seq.empty (* vertex does not match *)
       | SOME st2 => (* continue matching *)
         locally_match_verticies 
           ((n,d), get_adj_pat_vertices st2 )
           ((tn,td), get_adj_tgt_vertices st2)
           st2
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
