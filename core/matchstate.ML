(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  MatchState and Matching Algorithm *)
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
signature MATCH_STATE
= sig
  structure Match : MATCH 
  structure RGGraph : RG_GRAPH
  structure Edge : EDGE
  structure Vertex : RG_VERTEX

  (* sharing Vertex = RGGraph.RGVertex *)
  (* not clear why I need this this way round - but get type strange error the other way *)
  sharing RGGraph.SharingWithIData = Match.Sharing; 
  sharing Vertex.SharingWithIData = RGGraph.BVertex.SharingWithIData;
  sharing Edge.EdgeSharingWithData = RGGraph.Edge.EdgeSharingWithData;

  type T
  datatype matchstep =
           FoundMatch of T         | Progress of T Seq.seq

  val init : Match.T 
             -> RGGraph.T (* pat *)
             -> RGGraph.T (* tgt *)
             -> Vertex.name Vertex.NTab.T (* initial agenda *)
             -> T (* initial match state *)

  (* match two graphs *)
  val match : RGGraph.T -> RGGraph.T (* pat, tgt *)
              -> Vertex.name Vertex.NTab.T (* initial agenda *)
              -> Match.T Seq.seq
  val match' : Match.T (* initial matching *)
               -> RGGraph.T -> RGGraph.T (* pat, tgt *)
               -> Vertex.name Vertex.NTab.T (* agenda *)
               -> Match.T Seq.seq

  (* parts of the matching algorithm *)
  val do_next_match_choices : T -> matchstep

  val match_2vertices_with_edges : Vertex.name -> T -> Vertex.name -> T Seq.seq

  val match_vertex : Vertex.name * Vertex.data2 -> 
                     Vertex.name * Vertex.data2 ->
                     T -> T option

  val match_search : T Seq.seq -> unit -> (Match.T * Match.T Seq.seq) option


  (* modifying match state *)
  val pop_agenda : T -> ((Vertex.name * Vertex.name) * T) option

  val add1_to_agenda : Vertex.name * Vertex.name -> T -> T

  val del_pat_edges : Edge.NSet.T -> T -> T
  val del_tgt_edges : Edge.NSet.T -> T -> T

  (* internal get, update and set *)
  val get_agenda : T -> Vertex.name Vertex.NTab.T
  val get_match : T -> Match.T
  val get_match_mctxt : T -> Match.MCtxt.T
  val get_match_vmap : T -> Vertex.name Vertex.NTab.T
  val get_tgt_vnseq : T -> Vertex.name Seq.seq
  val get_tgt_vs : T -> T -> Vertex.NSet.T
  val get_unm_pat : T -> RGGraph.T
  val get_unm_pat_vs : T -> Vertex.NSet.T
  val get_unm_tgt : T -> RGGraph.T
  val get_unm_tgt_es : T -> Edge.NSet.T

  val update_agenda : (Vertex.name Vertex.NTab.T ->
                       Vertex.name Vertex.NTab.T) ->
                      T -> T
  val update_match : (Match.T -> Match.T) -> T -> T
  val update_unm_pat : (RGGraph.T -> RGGraph.T) -> T -> T
  val update_unm_tgt : (RGGraph.T -> RGGraph.T) -> T -> T

  val set_agenda : Vertex.name Vertex.NTab.T -> T -> T
  val set_match : Match.T -> T -> T
  val set_match_mctxt : Match.MCtxt.T -> T -> T

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val pretty_agenda : T -> Pretty.T
  val pretty_match_pair : Vertex.name * Vertex.name -> Pretty.T
  val pretty_next_pat_vertex : T -> Pretty.T
  val print : T -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchStateFun(
  structure RGGraph : RG_GRAPH 
  structure Match : MATCH
  sharing Match.Sharing = RGGraph.SharingWithIData
) (* : MATCH_STATE *)
= struct

(* structure DB_MatchState = struct *)

structure RGGraph = RGGraph;
structure Vertex = RGGraph.RGVertex;
structure Edge = RGGraph.Edge;
structure G = RGGraph;
structure V = Vertex;
structure E = Edge;

structure Match = Match;
structure MCtxt = Match.MCtxt;

datatype T = 
         MState of 
         {(* and-agenda = (pat -> tgt) = we are commited to matching these *) 
          agenda : Vertex.name Vertex.NTab.T, (* and-agenda: pat -> tgt *)
          (* global vertex and edge matching info *)
          match : Match.T, 
          (* the pat graph with vertices removed as we match them *)
          unm_pat : RGGraph.T, 
          pat_lfs : Vertex.NSet.T, (* unmatched leaves from init pat *)
          (* tgt with matched edges removed as we match them *)
          unm_tgt : RGGraph.T 
         };

(* initial matching state *)
fun init match pat tgt agenda =
    MState { agenda = agenda, match = match,
             pat_lfs = RGGraph.get_leaf_vnames pat,
             unm_pat = pat, unm_tgt = tgt };

(* tools *)
fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_unm_pat (MState rep) = #unm_pat rep;
fun get_pat_lfs (MState rep) = #pat_lfs rep;
val get_unm_pat_vs = 
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_pat;
val get_unm_unboxed_pat_vs = RGGraph.get_unboxed o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    Edge.NTab.get_nameset o RGGraph.get_edges o get_unm_tgt;

(* *)
fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             unm_pat = #unm_pat rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             unm_pat = #unm_pat rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_unm_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = f (#unm_pat rep),
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_pat_lfs f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep,
             pat_lfs = f (#pat_lfs rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = f(#unm_tgt rep)
           };

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

(* *)
(* fun get_adj_pat_vertices st edgens =
    RGGraph.adj_vnames' (get_unm_pat st) edgens;
fun get_adj_tgt_vertices st edgens =
    RGGraph.adj_vnames' (get_unm_tgt st) edgens;
*)
(* implicit: tgt_edges st *)
val del_tgt_edge = update_unm_tgt o RGGraph.delete_edge;
val del_pat_edge = update_unm_pat o RGGraph.delete_edge;
val del_tgt_edges = update_unm_tgt o Edge.NSet.fold RGGraph.delete_edge;
val del_pat_edges = update_unm_pat o Edge.NSet.fold RGGraph.delete_edge;

(* implicit: v st *)
val del_pat_vertex = update_unm_pat o RGGraph.delete_vertex;
fun del_pat_lfvertex pn st = 
    st |> del_pat_vertex pn
       |> update_pat_lfs (Vertex.NSet.delete pn);

(* implicit: (patv*tgtv) st *)
fun add1_to_agenda (pat_n, tgt_n) st = 
    update_agenda (Vertex.NTab.ins (pat_n,tgt_n)) st;

(* add vertex-matching to agenda if consistent (consistent = not in
   agenda, or already in the agenda but matched to same thing - in
   which case do nothing) *)
fun try_add1_to_agenda (pat_n,tgt_n) st = 
    let val agenda = get_agenda st
    in case Vertex.NTab.lookup agenda pat_n of 
        NONE => SOME (set_agenda (Vertex.NTab.ins (pat_n,tgt_n) agenda) st)
      | SOME tgt_n2 => 
        (if Vertex.name_eq (tgt_n,tgt_n2) 
         then SOME st (* already in agenda *)
         else NONE (* pat-vertex can only be mapped one tgt-vertex. *))
    end;

(* pop first vertex pair from agenda *)
fun pop_agenda st = 
    (case Vertex.NTab.pull_elem (get_agenda st)
      of NONE => NONE 
       | SOME (h,agenda2) => SOME (h,set_agenda agenda2 st));

(* match the edges between two pattern edges with those between two
   target edges. If both pat edges are non-boundary, then match
   strictly: all tgt edges must be matched, else only require all
   pattern edges to be matched. *)
fun multi_edge_matches from_pd to_pn pat_edges tgt_edges st = 
    (* choose edge matching function *)
    ((case from_pd of (* non-strict edge matching: maybe extra edges in tgt *)
        V.BVert => Match.multi_edge_match
      | V.IVert d => 
        (case fst (RGGraph.get_vertex (get_unm_pat st) to_pn)
          of V.BVert => Match.multi_edge_match
           | V.IVert d => Match.strict_multi_edge_match))
       pat_edges tgt_edges (get_match st))
    (* apply edge matching: all pat egdes are always matched,
       mtes is matched tgt edges *)
    |> Seq.map (fn (_, (mtes, m)) => 
                   st |> del_pat_edges (Edge.NTab.get_nameset pat_edges)
                      |> del_tgt_edges mtes
                      |> set_match m);

(* match edges between two vertices and add new matching to the agenda *)
fun edgematch_adj_2vs (from_pv as (from_pn, from_pd)) to_pn
                      (from_tn, _) to_tn st =
    let 
      val unm_pat = (get_unm_pat st);
      val pat_edges = RGGraph.edges_between unm_pat from_pn to_pn;
      val tgt_edges = RGGraph.edges_between (get_unm_tgt st) from_tn to_tn;
    in
      st |> multi_edge_matches from_pd to_pn pat_edges tgt_edges
         |> Seq.map_filter (try_add1_to_agenda (to_pn,to_tn))
         |> (Seq.of_list o Seq.list_of) : T Seq.seq (* debug: force eval here and now *)
    end;

(* NOTE: we treat all self-loops as equivalent; this assumes no edge
   data, but saves on many otherwise symmetric matches for self-loops *)
fun edgematch_adj_leaf_to_loop (lfpvn,(lfpeh,ped)) (from_tn,from_td) (* tgt vertex *)
                               (ltes, st) = 
    (case Edge.NSet.get_local_bot (Edge.NTab.get_nameset ltes)
      of NONE => NONE
       | SOME lte => 
         let 
           val m2 = 
               (get_match st)
                 |> (Match.add_leaf_ehn2edge_emap 
                       (lfpeh,ped) (lte, Edge.NTab.get ltes lte))
                 |> Match.match_lfvertex lfpvn (from_tn,from_td)
           val st2 = st |> set_match m2
                        |> del_pat_lfvertex lfpvn
                        |> del_pat_edge (Edge.Ehn.get_ename lfpeh)
         in if Match.tgt_edge_is_fully_matched m2 lte then 
              SOME (Edge.NTab.delete lte ltes, 
                    st2 |> del_tgt_edge lte)
            else 
              SOME (ltes, st2)
         end);


(* match up an leafedge from a pv against *)
(* NOTE: we treat all self-loops as equivalent; this assumes no edge
   data, but saves on many otherwise symmetric matches for self-loops;
 *)
(* IMPROVE: respect edge data might fail - deal with options/turn into seqs *)
fun edgematch_adj_leaf (from_tn,from_td) (lfpne as (lfpvn,(lfpeh,ped)))
                       (left_over_tehs, (ltes, st)) = 
    (case Edge.Ehn.NSet.pull_local_bot left_over_tehs 
      of NONE => (* all extra target edges already matched, so match 
                    the extra leaf pattern edge/vertex to self-loops *)
         (case edgematch_adj_leaf_to_loop lfpne (from_tn,from_td) (ltes, st)
           of SOME (ltes2, st2) => Seq.single (left_over_tehs, (ltes2, st2))
            | NONE => Seq.empty (* THINK: can this actually happen? *) )
       | SOME (teh, more_tehs) => 
         let val tgt = get_unm_tgt st
             val tn = RGGraph.get_ehn_vname tgt (Edge.Ehn.other teh)
             val td = RGGraph.get_vertex_data tgt tn
             val ted = RGGraph.get_ehn_data tgt teh
             val m2 = (get_match st) 
                        |> Match.match_lfvertex lfpvn (tn,td)
                        |> (the o Match.add_full_leaf_ehn2ehn_emap 
                                    (lfpeh,ped) (teh,ted))
             val te = Edge.Ehn.get_ename teh
             val pe = Edge.Ehn.get_ename lfpeh
             val st2 = st |> set_match m2
                          |> del_pat_lfvertex lfpvn 
                          |> del_tgt_edge te
         in
           Seq.cons  (* edgematch against this teh *)
             (more_tehs, (ltes, st2))
             (Seq.map (* edgematch against other incident tehs *)
                (fn (tehs, x) => (Edge.Ehn.NSet.add teh tehs, x))
                (edgematch_adj_leaf (from_tn,from_td) lfpne (more_tehs, (ltes, st))))
         end
         handle Option.Option => 
                (Seq.map (* edgematch against other incident tehs *)
                   (fn (tehs, x) => (Edge.Ehn.NSet.add teh tehs, x))
                   (edgematch_adj_leaf (from_tn,from_td) lfpne (more_tehs, (ltes, st))))
    );


(* match extra pat leaves against remaining target edges *)
(* IMPROVE?: really match leaf to every possible location; currently
   we just match leaves to whatever they can be matched to first. *)
fun edgematch_extra_pat_leaves from_pd (from_tn, from_td)
      ltes (* unmatched loopy edges in tgt *)
      plfs (* left over pattern boundary leaves *)
      st = 
    let 
      val left_over_tehs = 
          RGGraph.get_vname_ehns (get_unm_tgt st) from_tn
      val leaf_match_seq =
          Vertex.NTab.fold
            (Seq.maps o edgematch_adj_leaf (from_tn, from_td))
            plfs
            (Seq.single (left_over_tehs, (ltes, st)))
    in (* allow extra tgts only if from_d is boundary *)
      Seq.maps
        (fn (tehs2, (ltes2, st2)) =>
            (case from_pd 
              of Vertex.BVert => Seq.single st2 (* nonstrict *)
               | Vertex.IVert _ => (* strict *)
                 if Edge.Ehn.NSet.is_empty tehs2 
                    andalso Edge.NTab.is_name_empty ltes2
                 then Seq.single st2 else Seq.empty))
        leaf_match_seq
    end;

(* match up all adjavent edges of chosen pattern and target, we do
   this by going through each adjacent vertex of the pattern and
   finding it and adjacent vertex in the target. *)
(* IMPROVE: use incident number to quickly rule out cases where
   pattern has more incident edges that tgt -> they will always fail?
   (i.e. do counting in advance, avoid seraching permutations of
   counting) *)
fun edgematch_adj_verticies 
      (from_pv as (from_pn,from_pd)) (* pat vertex *)
      (from_tv as (from_tn,from_td)) (* tgt vertex *)
      (pns, (* adjacent to pat, non-leaves *)
       tns) (* adjacent to tgt *) 
      (ltes, (* unmatched loopy edges in tgt *)
       (plfs, (* left over pattern boundary leaves *)
       st)) = (* state of matching so far *)
    (* *)
    (Seq.single (tns, st))
    (* match all adj non-leaf pattern vertices *)
    |> (Vertex.NSet.fold 
          (Seq.maps o 
          (fn to_pn => 
           fn (tns,st) => 
              (* for each tn, try to_pn against it *)
              (Seq.maps
                 (fn (to_tn, tns2) => 
                     Seq.map (pair tns2)
                             (edgematch_adj_2vs 
                                from_pv to_pn from_tv to_tn st))
                 (Seq.of_list (Vertex.NSet.pull_each tns)))))
          pns)
    (* match remaining pat leaves; note: we ignore tns as we
       re-compute it in terms of half edges. IMPROVE: avoid this? *)
    |> (Seq.maps (edgematch_extra_pat_leaves from_pd from_tv ltes plfs o snd));


(* if pn is internal and tgt has loops, match tgt loops against leaf
   boundary nodes adjacent to pn; this will remove adjacent matched
   pattern nodes from the those needing to be matched against further
   tgt nodes. (This is when we do edge loop cutting in tgt, from our
   matching semantics)
 *)

(* IMPROVE: avoid exception to stop fold loop - have a try_fold *)
(*  Match all loopy edges in tgt against 2 leaf boundary nodes in pat *)
fun match_all_tgt_loops_by_leaves (tn,td) (ltes, (plfs,st)) = 
    let val (plfs2, st2) = 
      Edge.NTab.fold 
      (fn (te as (ten,ted)) => fn (plfs,st) => 
       let (* pull out two lef boundary nodes *)
         val (p1 as (pn1,pe1),plfs2) = the (V.NTab.pull_elem plfs)
         val (p2 as (pn2,pe2),plfs3) = the (V.NTab.pull_elem plfs2)
          (* IMPROVE: to support edge data, be careful - 
             matching may need to change another edges data *)
         val st2 = 
             st |> del_pat_lfvertex pn1 
                |> del_pat_lfvertex pn2
                |> del_tgt_edge ten
                |> (update_match 
                      (Match.match_lfvertex pn1 (tn,td)
                       o Match.match_lfvertex pn2 (tn,td)
                       o Match.add_leaf_ehn2edge_emap pe1 te
                       o Match.add_leaf_ehn2edge_emap pe2 te))
       in (plfs3,st2) end)
      ltes (plfs,st)
    in Seq.single (Edge.NTab.empty, (plfs2, st2)) end
    (* IMPROVE: I don't like imperitive loop exit!!! *)
    (* Option exception, indicating failure, if there is not enough pattern
       boundary nodes to match the loops, or if matching fails for
       some reason. *)
    handle Option => Seq.empty; 

(* match all self-loops in pattern by self-loops in tgt *)
fun match_all_pat_self_loops lpes (ltes, st) = 
    (Match.multi_edge_match lpes ltes (get_match st))
      |> Seq.map (fn (ltes2,(mtes,m)) => 
                     (ltes2, 
                      st |> del_pat_edges (Edge.NTab.get_nameset lpes)
                         |> del_tgt_edges mtes
                         |> set_match m));

(* 1. every loop in pat must have a loop in tgt,
   2. if pat is internal, then all extra loops in tgt 
   must be matched by leaf boundaries
*)
fun match_self_loops pd tn lpes (* loopy pat edges *)
                     (ltes, (* loopy tgt edges *)
                      (plfs, (* pat leaves *)
                       st)) = 
    let val ltes2st2seq = match_all_pat_self_loops lpes (ltes, st) in
      case pd 
       of V.BVert => 
          Seq.map (fn (ltes,st) => (ltes,(plfs,st))) ltes2st2seq
        | V.IVert _ => 
          Seq.maps (* must have all tgt self loops matched by leaves *)
            (fn (ltes,st) => match_all_tgt_loops_by_leaves 
                               tn (ltes, (plfs, st)))
            ltes2st2seq
    end;
          

(* st pn *)
val is_leaf_of_init_pat = Vertex.NSet.contains o get_pat_lfs;

(* assumes pat vs are uninstantiated.
   Raises Vertex.NTab...DUP exception, if n1 is already instantiated.
   Assumes each pat-vertex is only examined once; 
   therefore assume not in vmap *)
fun match_vertex pnv tnv st = 
    case Match.match_vertex pnv tnv (get_match st)
     of NONE => NONE
      | SOME m2 => SOME (st |> set_match m2);

(* implicit: pnv tnv st *)
val match_lfvertex = update_match oo Match.match_lfvertex;

(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
fun match_2vertices_with_edges 
      (pn : Vertex.name) (* pattern vertex name *)
      (st : T) (* match state so far *)
      (tn : Vertex.name) = (* target vertex name *)
    let 
      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      (* ltes = loopy tgt edge half name set, 
         tns = other tgt nodes set *) 
      val (td, (ltes, tns)) = RGGraph.adj_tgt_info tgt tn;
      (* plfs = tab leaves adj to pn map to edge halfs touching pn, 
         lpes = loopy pattern ename set
         pns = all other adj vertices set *)
      val (pd, (plfs, lpes, pns)) = RGGraph.adj_pat_info pat pn;
    in 
      if is_leaf_of_init_pat st pn then 
        st |> match_lfvertex pn (tn,td)
           |> (fn st2 => (ltes,(plfs,st2)))
           |> edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns)
           |> Seq.map (del_pat_lfvertex pn)
      else 
        case match_vertex (pn,pd) (tn,td) st of 
          NONE => Seq.empty (* vertex does not match *)
        | SOME st2 => (* continue matching *)
          (ltes,(plfs,st2))
            |> match_self_loops pd (tn,td) lpes 
            |> Seq.maps (edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns))
            |> Seq.map (del_pat_vertex pn) (* finished matching pn *)
    end;

(* datatype for steps of matching *)
datatype matchstep =
         FoundMatch of T
       | Progress of T Seq.seq (* or choices in matching *)

(* get tgt vertex names as a seq *)
val get_tgt_vnseq = 
    Seq.of_list o Vertex.NSet.list_of 
    o Vertex.NTab.get_nameset 
    o RGGraph.get_vertices o get_unm_tgt;

datatype pulled_vertex_kind = 
         BBoxed of Vertex.name 
       | UnBBoxed of Vertex.name;

fun get_next_pat_vertex st = 
    (case (Vertex.NSet.get_local_bot (get_unm_unboxed_pat_vs st)) of
      SOME n => SOME (UnBBoxed n)
    | NONE => 
      (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
         NONE => NONE
       | SOME n => SOME (BBoxed n)));

(* consider the next and-point in the agenda *)
fun do_next_match_choices st = 
    (case pop_agenda st 
      of NONE => (* nothing on the prescribed agenda *)
         (* get next pat vertex to match, non-boxed first *)
         (case get_next_pat_vertex st of 
            NONE => (* no pattern vertices left! *)
              FoundMatch st (* because we finished! *)
          | SOME (UnBBoxed pvn) => 
            Progress (* choices over picking a new vertex in tgt *)
              (Seq.maps
                 (match_2vertices_with_edges pvn st)
                 (get_tgt_vnseq st))
          | SOME (BBoxed pvn) => 
            (* IMPROVE: find all box-unfoldings also as matches? *)
            (* all other things are in boxes, 
               so killing them all gives a valid matching... *)
            FoundMatch st) 
       | SOME ((pn,tn),st2) => 
         Progress (* deal with first prescribed and-choice *)
           (match_2vertices_with_edges pn st2 tn));

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

(* 
exception match_bug_exp of Match.T * RGGraph.T * RGGraph.T ->
               Vertex.name Vertex.NTab.T -> Match.T Seq.seq
*)
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
    Pretty.block 
      ((case Vertex.NTab.pull_elem (get_agenda st) of 
          NONE => [Pretty.str "Agenda: []"]
        | SOME ((nextfrom,nextto),rest) => 
          [Pretty.str "Agenda: (",
           Vertex.pretty_name nextfrom,
           Pretty.str "->",
           Vertex.pretty_name nextto,
           Pretty.list ") then [" "]" 
                       (map pretty_match_pair (Vertex.NTab.list_of rest))])
      );

fun pretty (st as MState rep) = 
    Pretty.block 
      [Pretty.str "MatchState {",
       Pretty.chunks 
         [Match.pretty (#match rep),
          pretty_agenda st,
          Pretty.block [Pretty.str "Unmatched pat: "],
          RGGraph.pretty (#unm_pat rep),
          Pretty.block [Pretty.str "Unmatched tgt: "],
          RGGraph.pretty (#unm_tgt rep)
         ],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;

end; (* MatchState structure *)


structure DB_MatchState = MatchStateFun(structure RGGraph = RGGraph 
                                           and Match = Match);

structure MatchState : MATCH_STATE = DB_MatchState;
