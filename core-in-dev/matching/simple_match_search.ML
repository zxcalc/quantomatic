(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  MatchSearch and Matching Algorithm *)
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
signature SIMPLE_MATCH_SEARCH
= sig
  structure Match : MATCH 
  structure BangGraph : BANG_GRAPH
  structure Edge : EDGE
  structure Vertex : OVERTEX

  (* sharing Vertex = BangGraph.RGVertex *)
  (* not clear why I need this this way round - but get type strange error the other way *)
  sharing BangGraph.SharingBangGraph = Match.Sharing.G;
  sharing Vertex.SharingWithIData = BangGraph.OVertex.SharingWithIData;
  sharing Edge.EdgeSharingWithData = BangGraph.Edge.EdgeSharingWithData;

  type T
  datatype matchstep =
           FoundMatch of T         | Progress of T Seq.seq

  val init : Match.T 
             -> BangGraph.T (* pat *)
             -> BangGraph.T (* tgt *)
             -> Vertex.name Vertex.NTab.T (* initial agenda *)
             -> T (* initial match state *)

  (* match two graphs *)
  val match : (BangGraph.Vertex.NSet.T * BangGraph.Edge.NSet.T 
               * BangGraph.BBox.NSet.T) (* other names to avoid when matching *)
              -> BangGraph.T -> BangGraph.T (* pat, tgt *)
              -> Vertex.name Vertex.NTab.T (* initial agenda *)
              -> Match.T Seq.seq
  val match' : Match.T (* initial matching *)
               -> BangGraph.T -> BangGraph.T (* pat, tgt *)
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
  val get_match_viso : T -> Vertex.name Vertex.NTab.T
  val get_tgt_vs : T -> T -> Vertex.NSet.T
  val get_unm_pat : T -> BangGraph.T
  val get_unm_pat_vs : T -> Vertex.NSet.T
  val get_unm_tgt : T -> BangGraph.T
  val get_unm_tgt_es : T -> Edge.NSet.T

  val update_agenda : (Vertex.name Vertex.NTab.T ->
                       Vertex.name Vertex.NTab.T) ->
                      T -> T
  val update_match : (Match.T -> Match.T) -> T -> T
  val update_unm_pat : (BangGraph.T -> BangGraph.T) -> T -> T
  val update_unm_tgt : (BangGraph.T -> BangGraph.T) -> T -> T

  val set_agenda : Vertex.name Vertex.NTab.T -> T -> T
  val set_match : Match.T -> T -> T

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val pretty_agenda : T -> Pretty.T
  val pretty_match_pair : Vertex.name * Vertex.name -> Pretty.T
  val pretty_next_pat_vertex : T -> Pretty.T
  val print : T -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor SimpleMatchSearchFun(
  structure BangGraph : BANG_GRAPH 
  structure Match : MATCH
  sharing Match.Sharing.G = BangGraph.SharingBangGraph
) (* : MATCH_SEARCH *)
= struct

(* structure DB_MatchSearch = struct *)
structure BangGraph = BangGraph;
structure Vertex = BangGraph.OVertex;
structure Edge = BangGraph.Edge;
structure G = BangGraph;
structure V = Vertex;
structure E = Edge;

structure Match = Match;

structure BBMatch = Match.BBMatch;
structure BBox = G.BBox;
structure BBMap = BBMatch.BBMap;

datatype T = 
         MState of 
         {(* and-agenda = (pat -> tgt) = we are commited to matching these *) 
          agenda : V.name V.NTab.T, 
          (* global vertex and edge matching info *)
          match : Match.T, 
          (* the pat graph with vertices removed as we match them *)
          unm_pat : G.T, 
          (* unmatched leaves from init pat = 
             needed because they can otherwise be introduced in the pat! *)
          pat_boundary : V.NSet.T, 
          (* tgt with matched edges removed as we match them *)
          unm_tgt : G.T 
         };

(* initial matching state *)
fun init match pat tgt agenda =
    MState { (* initially used names are those of the pattern graph *)
             agenda = agenda, 
             match = match, 
             pat_boundary = G.get_boundary pat,
             unm_pat = pat, unm_tgt = tgt };

(* tools *)
fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
val get_bbmatch = Match.get_bbmatch o get_match;
val get_names = Match.get_names o get_match;

fun get_unm_pat (MState rep) = #unm_pat rep;
fun get_pat_boundary (MState rep) = #pat_boundary rep;
(* implicit: st pn *)
val is_leaf_of_init_pat = Vertex.NSet.contains o get_pat_boundary;

val get_unm_pat_vs = 
    Vertex.NTab.get_nameset o BangGraph.get_vertices o get_unm_pat;
val get_unm_unboxed_pat_vs = BangGraph.get_unboxed o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
val get_unm_unboxed_tgt_vs = BangGraph.get_unboxed o get_unm_tgt;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_nameset o BangGraph.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    Edge.NTab.get_nameset o BangGraph.get_edges o get_unm_tgt;

(* *)
fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             unm_pat = #unm_pat rep, 
             pat_boundary = #pat_boundary rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             unm_pat = #unm_pat rep, 
             pat_boundary = #pat_boundary rep,
             unm_tgt = #unm_tgt rep
           };
fun update_unm_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = f (#unm_pat rep), 
             pat_boundary = #pat_boundary rep,
             unm_tgt = #unm_tgt rep
           };
fun update_pat_boundary f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, 
             pat_boundary = f (#pat_boundary rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, 
             pat_boundary = #pat_boundary rep,
             unm_tgt = f(#unm_tgt rep)
           };
fun update_names f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, 
             pat_boundary = #pat_boundary rep,
             unm_tgt = #unm_tgt rep
           };

val set_names = update_match o Match.set_names;
(* val update_names = update_match oo Match.update_names; *)

val set_unm_pat = update_unm_pat o K;

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_viso = Match.get_viso o get_match;

(* the map part of bang box matching *)
val get_bbmap =  Match.get_bbmap o get_match;

(* *)
(* fun get_adj_pat_vertices st edgens =
    BangGraph.adj_vnames' (get_unm_pat st) edgens;
fun get_adj_tgt_vertices st edgens =
    BangGraph.adj_vnames' (get_unm_tgt st) edgens;
*)
(* implicit: tgt_edges st *)
val del_tgt_edge = update_unm_tgt o BangGraph.delete_edge;
val del_pat_edge = update_unm_pat o BangGraph.delete_edge;
val del_tgt_edges = update_unm_tgt o Edge.NSet.fold BangGraph.delete_edge;
val del_pat_edges = update_unm_pat o Edge.NSet.fold BangGraph.delete_edge;

(* implicit st *)
fun del_tgt_n_edges n es = 
    update_unm_tgt 
      (fold BangGraph.delete_edge (List.take (Edge.NSet.list_of es, n)));

(* implicit: v st *)
val del_pat_vertex = update_unm_pat o BangGraph.delete_vertex;

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
         else NONE (* a pat-vertex can only be mapped one tgt-vertex. *))
    end;

(* pop first vertex pair (pat,tgt) from agenda; get internal vertices before
   external ones - required by edge-matching - and probably more efficient too. 
   IMPROVE: store a maximally internal traversal of the pattern (as the
   pre-agenda) which can then connected to the tgt vertices as the agenda. This 
   will save repeated search for non-boundary vertices - also the basis of more 
   efficient algorithm. (see also get_next_pat_vertex) *)
fun pop_agenda st = 
    let val agenda = (get_agenda st);
        val pat = get_unm_pat st;
    in 
      case Vertex.NTab.find (fn (n1,n2) => not (G.is_boundary pat n1)) agenda of
        NONE => (* no internal vertices in the agenda *) 
          (case Vertex.NTab.pull_elem agenda
            of NONE => NONE 
             | SOME (n1n2,agenda2) => SOME (n1n2,set_agenda agenda2 st))
       | SOME (n1n2 as (n1,n2)) => 
         SOME (n1n2,set_agenda (Vertex.NTab.delete n1 agenda) st)
    end;

(* match the edges between two pattern vertives with those between two
   target vertices. *)
fun multi_edge_matches pes tes st = Seq.empty
(* FIXME:    
 WAS: Match.multi_edge_match pes tes (get_match st)
      |> Seq.map (fn (unmatches_tes, (mtes, m)) => 
                     (unmatches_tes, (mtes,
                      st |> del_pat_edges (Edge.NTab.get_nameset pes)
                         |> del_tgt_edges mtes
                         |> set_match m)));
*)

(* *)

fun half_edge_match (pn,(tn,td)) (peh,ped) tes st = Seq.empty
(* FIXME:    
 WAS:    (((tes,(E.NSet.empty,get_match st))
      |> Match.match_another_halfedge (peh,ped))
      |> Seq.single
      |> Seq.map (fn (unmatches_tes, (mtes, m)) => 
                     (unmatches_tes, (mtes,
                      st |> del_pat_edge (E.Ehn.get_ename peh)
                         |> del_tgt_edges mtes
                         |> set_match (Match.match_boundary pn tn m)
                         |> del_pat_vertex pn
                         ))))
    (* IMPROVE: make sensible error handling *) 
    handle Option.Option => Seq.empty; 
*)


(* try to match the bang-box's of the given pattern and tgt node:
   mapping must be injective, and tgt *)
(* IMPROVE?: maybe can do lazy bbox unfolding, and have bbox copying happen
   here? probbaly need generic notion of symmetry and search *)
fun match_bboxes pnv tnv st = 
    let val pat = get_unm_pat st;
        val tgt = get_unm_tgt st;
    in
      (case BangGraph.lookup_box_of tgt tnv of 
         NONE => 
         (case BangGraph.lookup_box_of pat pnv of 
            NONE => SOME st (* no bboxes; continue as normal *)
          | SOME pbbn => (* some pat bbox, no tgt bbox *)
            (case BBMap.lookup_domf (get_bbmap st) pbbn of
               NONE => SOME (st |> update_unm_pat (BangGraph.drop_box pbbn)
                                |> update_match (Match.add_bbox_drop pbbn))
             | SOME _ => NONE)) (* pat bbox already mapped; can't be dropped  *)
       | SOME tbbn => 
         (case BangGraph.lookup_box_of pat pnv of 
            NONE => NONE (* target bbox, but no pat bbox => fail *)
          | SOME pbbn => (* both in bboxes *)
            (case BBMap.lookup_domf (get_bbmap st) pbbn of
               NONE => (* pattern bbox not yet mapped - make new mapping *)
               SOME (st |> update_match (Match.add_bbox_map pbbn tbbn))
             | SOME tbbn2 => (* already mapped to a tgt bang box *)
               if (BBox.name_eq (tbbn,tbbn2)) 
                 then SOME st (* already mapped to this bbox *)
               else NONE))) (* already mapped elsewhere *)
    end;

(* bang boxes of pnv and tnv are set to be matching *)
fun have_matching_bboxes pnv tnv st = 
    let val pat = get_unm_pat st;
        val tgt = get_unm_tgt st;
    in
      case (BangGraph.lookup_box_of tgt tnv, BangGraph.lookup_box_of pat pnv) 
      of (NONE, NONE) => true
       | (SOME bb1, SOME bb2) => (BBMap.contains (get_bbmap st) (bb1,bb2))
       | (_,_) => false
    end;

(* move to library? *)
fun and_try f1 f2 x = (case f1 x of NONE => NONE | SOME x2 => f2 x2);

(* should be in ograph *)
local open BangGraph in 
(* try to get the name of the half-edge going to the boundary point 
   (i.e. the name of the half-edge attached to the internal vertex) 
*)
fun try_get_endpoint_ehn g v = 
    let val (vd,(ins,outs)) = get_vertex g v in
      case vd of OVertex.EVert => 
        (case (Edge.NSet.tryget_singleton ins,Edge.NSet.tryget_singleton outs)
         of (NONE, SOME e) => SOME (Edge.Ehn.mk_end e)
          | (SOME e, NONE) => SOME (Edge.Ehn.mk_start e)
          | (NONE, NONE) => NONE
          | (SOME _, SOME _) => NONE)
      | OVertex.IVert _ => NONE
    end;
end;

(* match edges between two vertices and add new matching to the agenda *)
(* match up edges between target verticess with pattern vs. 
   Target may be matched also to other pattern vs (when the pattern is a 
   half-edge/boundary). *)
fun edgematch_adj_2vs (p as ((from_pv as (from_pn, from_pd))))
                      to_pn
                      (t as ((from_tn, _)))
                      to_tn st =
    (case match_bboxes to_pn to_tn st 
      of SOME st2 => 
         let 
           val pat = get_unm_pat st2;
         in (* check if pattern vertex is endpoint/boundary *)
           case try_get_endpoint_ehn pat to_pn 
           of NONE =>  (* pattern vertex is internal *)
             (case try_add1_to_agenda (to_pn,to_tn) st2 of NONE => Seq.empty
              | SOME st3 => 
                let
                  val tes = BangGraph.halfedges_between (get_unm_tgt st3) 
                              from_tn to_tn;
                  val pes = BangGraph.edges_between pat from_pn to_pn; 
                in multi_edge_matches pes tes st3 end)
                  (* |> (Seq.of_list o Seq.list_of) : T Seq.seq 
                      -- to debug: force eval here and now *)
           | SOME ehn => (* pattern vertex is leaf-boundary *)
             let 
               val tgt = get_unm_tgt st2 
               val tes = BangGraph.halfedges_between (get_unm_tgt st2) 
                              from_tn to_tn;
             in
               half_edge_match 
                 (to_pn,(to_tn,BangGraph.get_vertex_data tgt to_tn))
                 (ehn, BangGraph.get_edge_data pat (Edge.Ehn.get_ename ehn)) 
                 tes st2
             end
         end
      | NONE => Seq.empty);

(* IMPROVE: should this be part of match? or maybe the unmatched edges should be 
   a param passed between functions, so we can use NSet.is_empty directly? *)
fun tgt_has_unmapped_edges_between st from_tn to_tn = 
    let val (lr,rl) = BangGraph.dir_enames_between (get_unm_tgt st) from_tn to_tn;
        val m = get_match st;
    in 
      (* FIXME: WAS: 
        E.NSet.exists   
        (fn e => Match.tgt_edgehalf_is_matched m (E.Ehn.mk_start e)) 
        lr
      orelse 
      E.NSet.exists   
        (fn e => Match.tgt_edgehalf_is_matched m (E.Ehn.mk_end e)) 
        rl *) 
     true
    end;

(* IMPROVE: can do lazy enumeration of bboxes here? *)
(* match as many pattern nodes as needed to get all the target edges. *)
(* Note optimisation: consider each pattern vertex only once, either it's used
   or it isn't. This assumes that edges have no data/have only local data. And
   that we arn't interested in permutations of edge-matches. *)
fun strict_edgematch_adj_tgt_vertex 
      (from_pv as (from_pn,from_pd)) 
      (from_tv as (from_tn,from_td))
      to_tn
      (pns, st) = 
    if not(tgt_has_unmapped_edges_between st from_tn to_tn)
    then Seq.single (pns, st)
    else 
      case Vertex.NSet.pull_local_bot pns
      of SOME (to_pn, pns2) => 
         Seq.append (* all ways to use adj pat vs to match given tgt vs *)
           (* don't use this vertex now, look for others pat vs, 
              but note it wasn't used by adding it to pns at the end. *)
           (* IMPROVE: make tail rec? *)
           (Seq.map (fn (pns,st) => (Vertex.NSet.add to_pn pns,st))
             (strict_edgematch_adj_tgt_vertex from_pv from_tv to_tn 
                (pns2,st)))
           (* use this vertex, and then look to see if we need more *)
           ((edgematch_adj_2vs from_pv to_pn from_tv to_tn st)
             |> Seq.map (pair pns2 o snd o snd)
             |> Seq.maps (strict_edgematch_adj_tgt_vertex from_pv from_tv
                          to_tn))
       | NONE => Seq.empty (* not enough pattern vertices to 
                              match extra edges *)
    ;

(* 
        (Seq.of_list (Vertex.NSet.pull_each pns))
        |> Seq.maps (* try each pattern node here *)
          (fn (to_pn, pns2) => 
              (edgematch_adj_2vs from_pv to_pn from_tv to_tn st)
              |> Seq.map (pair pns2 o snd o snd))
*)

(* match up all adjavent edges of chosen pattern and target, we do
   this by going through each adjacent vertex of the pattern and
   finding it and adjacent vertex in the target. *)
(* IMPROVE: use incident number to quickly rule out cases where
   pattern has more incident edges that tgt -> they will always fail?
   (i.e. do counting in advance, avoid seraching permutations of
   counting) *)
(* Invarent: ignore_arity >= 0 *)
fun edgematch_adj_verticies 
      (from_pv as (from_pn,from_pd)) (* pat vertex *)
      (from_tv as (from_tn,from_td)) (* tgt vertex *)
      (ignore_arity,st) = (* state of matching so far *)
    let 
      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      
      (* adjacent to tgt/pat, non-leaves *)
      val pns = BangGraph.adj_vnames pat from_pn;
      val tns = BangGraph.adj_vertices_with_edges tgt from_tn;
    in 
    (* *)
    (Seq.single (ignore_arity,(pns,st)))
    (* match all adj tgt vertices *)
    |> (tns |> Vertex.NTab.fold 
          (fn (to_tn,tes) => 
           let val teset = Edge.NTab.get_nameset tes;
               val to_tn_nedges = Edge.NSet.cardinality teset;
           in 
             Seq.maps (fn (ignore_arity,(pns,st)) => 
              Seq.append
                (* ignore all target edges *)
                (if (ignore_arity >= to_tn_nedges) (* always false ? *)
                 (* ISSUE? deleting edges connected to internal vertices will 
                    cause bad matchings: need to make sure all inernal vertices 
                    have been matched first. A: This should always be false: edge-points from_pn is never a boundary vertex*)
                 then Seq.single 
                        (ignore_arity - to_tn_nedges,
                         (pns, del_tgt_edges teset st))
                 else Seq.empty)
                (* or-choice: ignore every number of ignoreable target edges 
                   includes 0; for don't ignore any *)
                ((Seq.of_list (0 upto (Int.max (0, 
                   Int.min (to_tn_nedges, ignore_arity - 1)))))
                 |> Seq.maps 
                    (fn ignore_now => 
                    let val st2 = del_tgt_n_edges ignore_now teset st
                    in (* after ignore, match strictly *)
                      Seq.map (pair (ignore_arity - ignore_now))
                              (strict_edgematch_adj_tgt_vertex 
                                from_pv from_tv to_tn (pns,st2))
                    end)))
            end))
    end;

(* FIXME: using choose assumes no edge data *)

(* choose i elements from a list of l possibilities - order independent. *)
(* limit is about: choose 13 out of list of 26. 
   IMPROVE: use seq to lazily generate possibilities and avoid memory death
   (using depth first search!)
*)
fun list_choose i l = 
    let val lsize = length l;
        val skip_this_many = lsize - i;

        (* assumes (skip_this_many <= lsize) & (lsize = length (h::t)) *)
        fun choose _ _ [] = [] (* nothing to choose from *)
          | choose 0 lsize l = [l] (* skip none *)
          | choose skip_this_many lsize (h::t) = 
            if skip_this_many = lsize then [[]] (* skip all the rest *)
            else 
              (map (fn l => h::l) (* choose h *)
                   (choose skip_this_many (lsize - 1) t))
              @ (* skip h *)
              (choose (skip_this_many - 1) (lsize - 1) t);
    in 
      if skip_this_many < 0 orelse i < 0 then []
      else (choose skip_this_many lsize l) 
    end;

(* all unordered-pairs of leaf vertices -- all ways to match a self-loop; only
   unbboxed leaf vertices can be used (bboxed ones must be unboxed first) *)
fun pull_adj_pat_leaf_pairs (* from_patbbox *) pn st = 
    let val pat = get_unm_pat st;

        val leafs = 
            BangGraph.fold_adj 
              (fn e => fn n => fn L => 
                if not (BangGraph.is_boxed pat n) 
                   andalso is_leaf_of_init_pat st n
                   andalso BangGraph.is_boundary pat n 
                then (e,n)::L else L)
              pat pn [];

        fun all_leaf_pairs [] = []
          | all_leaf_pairs (n::ns) = 
            (* (if (in_fresh_bbox from_patbbox (BangGraph.lookup_box_of pat n))
             then [(n,n)] else [])
            @ *) 
            (map (pair n) ns)
            @ (all_leaf_pairs ns);

    in all_leaf_pairs leafs end;


(* if pn is internal and tgt has loops, match tgt loops against leaf
   boundary nodes adjacent to pn; this will remove adjacent matched
   pattern nodes from the those needing to be matched against further
   tgt nodes. (This is when we do edge loop cutting in tgt, from our
   matching semantics)
 *)
(* IMPROVE: to support edge data, be careful - 
   matching may need to change another edges data *)
exception bug_exp of string;
(*  Match all loopy edges in tgt against 2 leaf boundary nodes in pat *)
fun match_all_tgt_loops_by_leaves (pn,pd) (tn,td) (ltes,st) = 
    let 
      val ltes_size = Edge.Ehn.NSet.cardinality 
                        (Edge.Ehn.NTab.get_nameset ltes);
    in 
      if ltes_size = 0 then Seq.single st else 
      let
        val pat_leaf_pairs = pull_adj_pat_leaf_pairs pn st;
        val leaf_pairs = Seq.of_list (list_choose ltes_size pat_leaf_pairs);
      in 
        Seq.map fst
        (Edge.Ehn.NTab.fold 
        (fn (te as (tehn,ted)) => 
        let val ten = Edge.Ehn.get_ename tehn in
        Seq.map 
         (fn (st, []) => raise bug_exp "match_all_tgt_loops_by_leaves"
           | (st, (((pe1,pn1),(pe2,pn2))::plfps)) => 
            (* pull out two leaf boundary nodes *)
            (st |> del_pat_vertex pn1 
                |> del_pat_vertex pn2
                |> del_tgt_edge ten
                |> (update_match 
                      (Match.match_lfvertex pn1 (tn,td)
                       o Match.match_lfvertex pn2 (tn,td)
                       o Match.add_leaf_ehn2edge_emap pe1 (ten,ted)
                       o Match.add_leaf_ehn2edge_emap pe2 (ten,ted))), 
             plfps))
         end)
         ltes (Seq.map (pair st) leaf_pairs))
       end
    end;

(* we can match any number of extra target loops by leaves, leaving however many 
   extra target loops behind. 
   We are assuming all loopy edges have the same data - we cut symmetries 
   in permutations of unmatched self-loop edges. 
   ignore_arity = total edges that will be ignored in the target
*)
fun match_tgt_loops_by_leaves pnpd tntd ignore_arity (ltes,st) = 
    let 
      fun match_loops ltes (ignore_arity,st) = 
          Seq.append 
            (Seq.map (pair ignore_arity) (* ignore none *)
                     (match_all_tgt_loops_by_leaves pnpd tntd (ltes,st)))
            (if ignore_arity <= 0 then Seq.empty 
             else (* ignore: 1 more edge in target *)
             case Edge.Ehn.NTab.pull_smallest ltes of NONE => Seq.empty
             | SOME (_,ltes2) => (* ignore more edges in tgt *) 
               match_loops ltes2 (ignore_arity - 2,st));
    in (match_loops ltes (ignore_arity, st)) end;

(* match all self-loops in pattern by self-loops in tgt *)
fun match_all_pat_self_loops lpes ltes st = 
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
fun match_self_loops (pnpd as (pn,pd)) (tntd as (tn,td))
       (ast as (ignore_arity,st)) = 
    let 
      (* loopy tgt edges *)
      val ltes = BangGraph.get_self_loop_halfedges (get_unm_tgt st) tn;
      (* loopy pat edges *)
      val lpes = BangGraph.get_self_loop_edges (get_unm_pat st) pn;
    in 
        st |> match_all_pat_self_loops lpes ltes
           |> Seq.maps (match_tgt_loops_by_leaves pnpd tntd ignore_arity)
    end;

(* second bboxopt is a fresh bbox from the first - i.e. can be copied 
   independently *)
fun in_fresh_bbox _ NONE = false
  | in_fresh_bbox NONE (SOME _) = true
  | in_fresh_bbox (SOME bbn) (SOME bbn2) = 
    not (BBox.name_eq (bbn, bbn2));

fun nfold 0 f a = a
  | nfold n f a = nfold (n - 1) f (f a);

(* copy a single bang-box *)
fun copy1_pat_bbox bbn st = 
    let val names = get_names st
        val ((bbn2,(vrn,ern)),((vs2,es2,bs2),pat2)) = 
            G.copy1_box names bbn (get_unm_pat st)
        val names2 = (V.Rnm.get_avoids vrn, E.Rnm.get_avoids ern, bs2)
    in
      st |> set_unm_pat pat2
         |> set_names names2
         |> update_match (Match.add_bbox_copy 
              (bbn,(V.Rnm.get_nmap vrn,E.Rnm.get_nmap ern),bbn2))
    end;

(* !-box ops *)
fun copy_pat_bbox bbn k st = 
    if k = 0 then st |> update_unm_pat (G.kill_box bbn) 
                     |> update_match (Match.add_bbox_kills [bbn])
    else if k = 1 then st
    else if k > 1 then 
      nfold (k - 1) (copy1_pat_bbox bbn) st
    else raise bug_exp "copy_pat_bbox: negative copy value";

(* 
structure Arity = struct
  val zero = 0;
  fun is_zero a = (a = 0);
  fun is_negative a = a < 0;
  fun add a b = a + b;
  fun subtract_by a b = a - b;
  fun mult_by_int a i = a * i;
  fun divide a b = Int.div(a,b);
end;
*)

fun compute_scope_arities pn tn st = 
    let 
      val (pntab,pbboxntab) = 
          G.get_adj_arities_and_bang_arities (get_unm_pat st) pn;
      val (tntab,tbboxntab) = 
          G.get_adj_arities_and_bang_arities (get_unm_tgt st) tn;

      (* implicit: ntab k *)
      val ntab_sum = Vertex.NTab.fold (fn (n,i) => Arity.add i);
      val sum_in_bboxtab = 
          BBox.NTab.map_all (fn ntab => ntab_sum ntab Arity.zero);
      val summed_bbox_sum = BBox.NTab.fold (fn (n,i) => Arity.add i);

      val pbboxsums = sum_in_bboxtab pbboxntab;
      val tbboxsums = sum_in_bboxtab tbboxntab;

      val tarity = Arity.zero |> ntab_sum tntab |> summed_bbox_sum tbboxsums;
      val p_min_arity = Arity.zero |> ntab_sum pntab;

      val scope_arity =  Arity.subtract tarity p_min_arity;
    in
      (scope_arity, pbboxsums)
    end;

(*  *)

(* find all expansions of bang boxes so that matching might be possible *)
(* IMPROVE: integrate into lookahead to do maximal early pruning? 
   delay actual copying of bang box until we get an exact match to the number of 
   edges? *)
(* returns seq of possible bang-box expanded graphs paired with the target 
   number of edges that need to be ignored. 

   invarient: tarity > ignore_arity;
   tarity is total arity of target; ignore_arity is amount that can be removed to make tarity = parity. 
*)
fun expand_adj_pat_bboxes pn tn st = 
    let 
       val (scope_arity,pbboxsums) = compute_scope_arities pn tn st;
    in 
      if Arity.has_negative_incidence scope_arity 
      then Seq.empty (* Fail: need at least pn edges *)
      else (* go through each bang box considering all expansions of it, 
              upto size limit, combined with each previous possibility *)
        BBox.NTab.fold
          (fn (bbn,bbarity) => Seq.maps (* i edges to this bang box *)
              (fn (scope_arity,st) => 
                Seq.of_list
                (map (fn k => ((* remove this number of edges *)
                               Arity.subtract scope_arity 
                                              (Arity.mult_by_int k bbarity),
                               (* copy bang box this number of times *)
                               copy_pat_bbox bbn k st))
                     (* # of copies *)
                     (0 upto 
                      (Arity.max_incidence (Arity.divide scope_arity bbarity)))
                     )))
          pbboxsums
          (Seq.single (scope_arity,st))
    end;

(* assumes pat vs are uninstantiated.
   Raises Vertex.NTab...DUP exception, if n1 is already instantiated.
   Assumes each pat-vertex is only examined once; 
   therefore assume not in vmap *)
fun match_vertex pnv tnv st = 
    (case Match.match_vertex pnv tnv (get_match st)
     of NONE => NONE
      | SOME m2 => SOME (st |> set_match m2));

(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
(* assumes: 
    - called only on internal vertices 
    - bang boxes of pn and tn are already matching - we only need to consider vertices adj to these and update thing corespondingly. 
   guarentees: 
    - all adjacent bounary vertices get matched, not added to agenda
    - removes pattern vertex
*)
fun match_2vertices_with_edges 
      (pn : Vertex.name) (* pattern vertex name *)
      (st : T) (* match state so far *)
      (tn : Vertex.name) = (* target vertex name *)
    let 
      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      val pd = BangGraph.get_vertex_data pat pn;
      val td = BangGraph.get_vertex_data tgt tn;
    in (* check data matches *)
      case match_vertex (pn,pd) (tn,td) st of 
        NONE => Seq.empty (* data doesn't match *)
      | SOME st2 => (* continue matching all adjacent things *)
        st2 |> expand_adj_pat_bboxes pn tn
            |> Seq.filter (Arity.is_zero o fst)
            |> Seq.maps (match_self_loops (pn,pd) (tn,td))
            |> Seq.maps (edgematch_adj_verticies (pn,pd) (tn,td))
            (* remove pattern vertex, ignore now redudnent info *)
            |> Seq.map (del_pat_vertex pn o snd o snd)
    end;


(* IMPROVE: currently bang box expansion and to fit tgt size and non-strict matching are separate. Combine to avoid such an inefficient generate and test approach. 
*)


(* datatype for steps of matching *)
datatype matchstep =
         FoundMatch of T
       | Progress of T Seq.seq (* or choices in matching *)

(* get tgt vertex names as a seq *)
val get_unboxed_tgt_vnseq = 
    Seq.of_list o Vertex.NSet.list_of o get_unm_unboxed_tgt_vs;

datatype pulled_vertex_kind = 
         BBoxed of Vertex.name 
       | UnBBoxed of Vertex.name;

(* try to get next unboxed pattern vertex, give back vertex in bbox if none 
   outside it. *)
(* IMPROVE: store a maximally internal traversal of the pattern (as the
   pre-agenda) which can then connected to the tgt vertices as the agenda. This 
   will save repeated search for non-boundary vertices - also the basis of more 
   efficient algorithm. (see also pop_agenda) *)
fun get_next_pat_vertex st = 
    let val unboxed_p_vs = (get_unm_unboxed_pat_vs st)
        val pat = get_unm_pat st
    in
      case Vertex.NSet.get_exists 
             (fn n => not (G.is_boundary pat n)) unboxed_p_vs 
      of SOME n => SOME (UnBBoxed n)
       | NONE => 
          (case (Vertex.NSet.get_local_bot unboxed_p_vs) of
             SOME n => SOME (UnBBoxed n)
           | NONE => 
              (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
                 NONE => NONE
               | SOME n => SOME (BBoxed n)))
    end;

(* FIXME: this is used as a hack to avoid searching expansions of bang boxes which are disjoint form the rest of the pattern graph. *)
fun drop_all_bboxes_in_pat st = 
    let val pat = get_unm_pat st;
        val bboxlist = BangGraph.get_bbox_list pat;
    in 
      st |> update_unm_pat (fold BangGraph.kill_box bboxlist)
         |> update_match (Match.add_bbox_kills bboxlist)
    end;

(* consider the next and-point in the agenda *)
fun do_next_match_choices st = 
    (case pop_agenda st 
      of NONE => (* nothing connected to previously matched parts *)
         (* get next pat vertex to match, non-boxed first *)
         (case get_next_pat_vertex st of 
            NONE => (* no pattern vertices left! *)
              FoundMatch st (* because we finished! *)
          | SOME (UnBBoxed pn) => (* have a pat vertex outside bangboxes... *)
            Progress 
              (Seq.maps (* choices over picking a new vertex in tgt *)
                 (match_2vertices_with_edges pn st)
                 (get_unboxed_tgt_vnseq st))
          | SOME (BBoxed pn) => (* FIXME: add initial bbox match *)
            (* all other things are in boxes, 
               so killing them all gives a valid matching... *)
            (* FIXME: find all box-unfoldings and macthes to target !-boxes *)
            FoundMatch (drop_all_bboxes_in_pat st)) 
       | SOME ((pn,tn),st2) => (* match these things, only question is how ...*)
         Progress (* deal with first prescribed and-choice *)
           (match_2vertices_with_edges pn st2 tn));

(* given an or-choice agenda of states (seq), apply progress steps to search
tree, until we get a match, then give that back as first elem of lazy
list. We can then rest until asked for the next thing! *)
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
exception match_bug_exp of Match.T * BangGraph.T * BangGraph.T ->
               Vertex.name Vertex.NTab.T -> Match.T Seq.seq
*)
(* *)
fun match' m pat tgt initagenda = 
    let 
      val init_st = (init m pat tgt initagenda)
    in Seq.make (match_search (Seq.single init_st)) end;

fun match names pat tgt = match' (Match.init names pat tgt) pat tgt;

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

fun get_agenda_list st = 
    (case pop_agenda st of NONE => []
       | SOME (x,st2) => x :: get_agenda_list st2);

fun pretty_agenda st = 
    Pretty.list "[" "]" (map pretty_match_pair (get_agenda_list st));

fun pretty (st as MState rep) = 
    Pretty.block 
      [Pretty.str "MatchSearch {",
       Pretty.chunks 
         [Match.pretty (#match rep),
          pretty_agenda st,
          Pretty.block [Pretty.str "Unmatched pat: "],
          BangGraph.pretty (#unm_pat rep),
          Pretty.block [Pretty.str "Unmatched tgt: "],
          BangGraph.pretty (#unm_tgt rep)
         ],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;

end; (* MatchSearch structure *)


structure DB_MatchSearch = MatchSearchFun(structure BangGraph = BangGraph 
                                           and Match = Match);

structure MatchSearch : MATCH_SEARCH = DB_MatchSearch;
