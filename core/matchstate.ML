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

structure BBoxMatch = RGGraph.BBoxMatch;
(* structure BBox = RGGraph.BBoxMatch.BBox;
structure BBMap = RGGraph.BBoxMatch.BBMap; *)
structure BBox = RGGraph.BBox;
structure BBMap = RGGraph.BBoxMatch.BBMap;

datatype T = 
         MState of 
         {(* and-agenda = (pat -> tgt) = we are commited to matching these *) 
          agenda : Vertex.name Vertex.NTab.T, 
          (* global vertex and edge matching info *)
          match : Match.T, 
          (* the pat graph with vertices removed as we match them *)
          unm_pat : RGGraph.T, 
          (* bang box expansion of pattern *)
          bbmatch : BBoxMatch.T,
          (* unmatched leaves from init pat = 
             needed because they can otherwise be introduced in the pat! *)
          pat_lfs : Vertex.NSet.T, 
          (* tgt with matched edges removed as we match them *)
          unm_tgt : RGGraph.T 
         };

(* initial matching state *)
fun init match pat tgt agenda =
    MState { agenda = agenda, match = match, bbmatch = BBoxMatch.empty,
             pat_lfs = RGGraph.get_leaf_vnames pat,
             unm_pat = pat, unm_tgt = tgt };

(* tools *)
fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_bbmatch (MState rep) = #bbmatch rep;
fun get_unm_pat (MState rep) = #unm_pat rep;
fun get_pat_lfs (MState rep) = #pat_lfs rep;
(* implicit: st pn *)
val is_leaf_of_init_pat = Vertex.NSet.contains o get_pat_lfs;

val get_unm_pat_vs = 
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_pat;
val get_unm_unboxed_pat_vs = RGGraph.get_unboxed o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
val get_unm_unboxed_tgt_vs = RGGraph.get_unboxed o get_unm_tgt;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    Edge.NTab.get_nameset o RGGraph.get_edges o get_unm_tgt;

(* *)
fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             unm_pat = #unm_pat rep, bbmatch = #bbmatch rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             unm_pat = #unm_pat rep, bbmatch = #bbmatch rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_unm_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = f (#unm_pat rep), bbmatch = #bbmatch rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };
fun update_pat_lfs f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, bbmatch = #bbmatch rep,
             pat_lfs = f (#pat_lfs rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, bbmatch = #bbmatch rep,
             pat_lfs = #pat_lfs rep,
             unm_tgt = f(#unm_tgt rep)
           };
fun update_bbmatch f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             unm_pat = #unm_pat rep, bbmatch = f (#bbmatch rep),
             pat_lfs = #pat_lfs rep,
             unm_tgt = #unm_tgt rep
           };


val set_unm_pat = update_unm_pat o K;

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

(* the map part of bang box matching *)
val update_bbmap = update_bbmatch o BBoxMatch.update_bbmap;
val get_bbmap = BBoxMatch.get_bbmap o get_bbmatch;

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

(* implicit st *)
fun del_tgt_n_edges n es = 
    update_unm_tgt 
     (fold RGGraph.delete_edge (List.take (Edge.NSet.list_of es, n)));

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
fun multi_edge_matches from_pd to_pd pat_edges tgt_edges st = 
    (* choose edge matching function *)
    ((case from_pd of (* non-strict edge matching: maybe extra edges in tgt *)
        V.BVert => Match.multi_edge_match
      | V.IVert d => 
        (case to_pd
          of V.BVert => Match.multi_edge_match
           | V.IVert d => Match.strict_multi_edge_match))
       pat_edges tgt_edges (get_match st))
    (* apply edge matching: all pat egdes are always matched,
       mtes is matched tgt edges *)
    |> Seq.map (fn (unmatches_tes, (mtes, m)) => 
                   (unmatches_tes, (mtes,
                    st |> del_pat_edges (Edge.NTab.get_nameset pat_edges)
                       |> del_tgt_edges mtes
                       |> set_match m)));

(* try to match the bang-box's of the given pattern and tgt node:
   mapping must be injective, and tgt *)
fun match_bboxes pnv tnv st = 
    let val pat = get_unm_pat st;
        val tgt = get_unm_tgt st;
    in
      (case RGGraph.lookup_box_of tgt tnv of 
         NONE => 
         (case RGGraph.lookup_box_of pat pnv of 
            NONE => SOME st
          | SOME pbbn => 
            (case BBMap.lookup_domf (get_bbmap st) pbbn of
               NONE => SOME (st |> update_unm_pat (RGGraph.drop_box pbbn))
             | SOME _ => NONE)) (* already mapped to tgt bang box *)
       | SOME tbbn => 
         (case RGGraph.lookup_box_of pat pnv of 
            NONE => NONE
          | SOME pbbn => 
            (case BBMap.lookup_domf (get_bbmap st) pbbn of
               NONE => 
               SOME (st |> update_bbmap (BBMap.add_to_dom pbbn tbbn))
             | SOME tbbn2 => (* already mapped to a tgt bang box *)
               if (BBox.name_eq (tbbn,tbbn2)) then SOME st
               else NONE)))
    end;

(* bang boxes of pnv and tnv are set to be matching *)
fun have_matching_bboxes pnv tnv st = 
    let val pat = get_unm_pat st;
        val tgt = get_unm_tgt st;
    in
      case (RGGraph.lookup_box_of tgt tnv, RGGraph.lookup_box_of pat pnv) 
      of (NONE, NONE) => true
       | (SOME bb1, SOME bb2) => (BBMap.contains (get_bbmap st) (bb1,bb2))
       | (_,_) => false
    end;

(* move to library? *)
fun and_try f1 f2 x = (case f1 x of NONE => NONE | SOME x2 => f2 x2);

(* match edges between two vertices and add new matching to the agenda *)
fun edgematch_adj_2vs (p as ((from_pv as (from_pn, from_pd))))
                      to_pn
                      (t as ((from_tn, _)))
                      to_tn st =
    (case st |> and_try (match_bboxes to_pn to_tn)
                        (try_add1_to_agenda (to_pn,to_tn))
      of SOME st2 => 
         let 
           val pat = get_unm_pat st2
           val to_pd = RGGraph.get_vertex_data pat to_pn
           val pes = RGGraph.edges_between pat from_pn to_pn
           val tes = RGGraph.edges_between (get_unm_tgt st2) from_tn to_tn
         in multi_edge_matches from_pd to_pd pes tes st2 end
            (* |> (Seq.of_list o Seq.list_of) : T Seq.seq 
                  -- to debug: force eval here and now *)
      | NONE => Seq.empty);

(* IMPROVE: can do lazy enumeration of bboxes here? *)
(* match as many pattern nodes as needed to get all the target edges. *)
fun strict_edgematch_adj_tgt_vertex 
      (from_pv as (from_pn,from_pd)) 
      (from_tv as (from_tn,from_td))
      to_tn
      (pns, st) = 
    let 
      val tes = RGGraph.edges_between (get_unm_tgt st) from_tn to_tn
    in
      if Edge.NTab.is_name_empty tes then Seq.single (pns, st)
      else 
        (Seq.of_list (Vertex.NSet.pull_each pns))
        |> Seq.maps (* try each pattern node here *)
        (fn (to_pn, pns2) => 
            (edgematch_adj_2vs from_pv to_pn from_tv to_tn st)
            |> Seq.map (pair pns2 o snd o snd))
    end;

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
      val pns = RGGraph.adj_vnames pat from_pn;
      val tns = RGGraph.adj_vertices_with_edges tgt from_tn;
    in 
    (* *)
    (Seq.single (ignore_arity,(pns,st)))
    (* match all adj tgt vertices *)
    |> (tns |> Vertex.NTab.fold 
          (fn (to_tn,tes) => 
           let val teset = Edge.NTab.get_nameset tes
               val to_tn_nedges = Edge.NSet.cardinality teset 
           in 
             Seq.maps (fn (ignore_arity,(pns,st)) => 
              Seq.append
                (* ignore all target edges *)
                (if (ignore_arity >= to_tn_nedges)
                 then Seq.single 
                        (ignore_arity - to_tn_nedges,
                         (pns, del_tgt_edges teset st))
                 else Seq.empty)
                (* or-choice: ignore every number of ignoreable target edges *)
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
   unboxed leaf vertices can be used (bboxed ones must be unboxed first) *)
fun pull_adj_pat_leaf_pairs (* from_patbbox *) pn st = 
    let val pat = get_unm_pat st;

        val leafs = 
            RGGraph.fold_adj 
              (fn e => fn n => fn L => 
                if not (RGGraph.is_boxed pat n) 
                   andalso is_leaf_of_init_pat st n
                   andalso RGGraph.is_leaf_vertex pat n 
                then (e,n)::L else L)
              pat pn [];

        fun all_leaf_pairs [] = []
          | all_leaf_pairs (n::ns) = 
            (* (if (in_fresh_bbox from_patbbox (RGGraph.lookup_box_of pat n))
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
      val ltes_size = Edge.NSet.cardinality (Edge.NTab.get_nameset ltes);
    in 
      if ltes_size = 0 then Seq.single st else 
      let
        val pat_leaf_pairs = pull_adj_pat_leaf_pairs pn st;
        val leaf_pairs = Seq.of_list (list_choose ltes_size pat_leaf_pairs);
      in 
        Seq.map fst
        (Edge.NTab.fold 
        (fn (te as (ten,ted)) => Seq.map 
         (fn (st, []) => raise bug_exp "match_all_tgt_loops_by_leaves"
           | (st, (((pe1,pn1),(pe2,pn2))::plfps)) => 
         (* pull out two leaf boundary nodes *)
            (st |> del_pat_lfvertex pn1 
                |> del_pat_lfvertex pn2
                |> del_tgt_edge ten
                |> (update_match 
                      (Match.match_lfvertex pn1 (tn,td)
                       o Match.match_lfvertex pn2 (tn,td)
                       o Match.add_leaf_ehn2edge_emap pe1 te
                       o Match.add_leaf_ehn2edge_emap pe2 te)), 
             plfps)))
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
             case Edge.NTab.pull_smallest ltes of NONE => Seq.empty
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
   IMPROVE: derive k dynamically. 
   k = number of edges to ignore
*)
fun match_self_loops (pnpd as (pn,pd)) (tntd as (tn,td))
       (ast as (ignore_arity,st)) = 
    let 
      (* loopy tgt edges *)
      val ltes = RGGraph.get_self_loops (get_unm_tgt st) tn;
      (* loopy pat edges *)
      val lpes = RGGraph.get_self_loops (get_unm_pat st) pn;
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

(* !-box ops *)
fun copy_pat_bbox bbn k st = 
    if k = 0 then update_unm_pat (RGGraph.kill_box bbn) st
    else if k = 1 then st
    else if k > 1 then 
      update_unm_pat (nfold (k - 1) (snd o RGGraph.copy1_box bbn)) st
    else raise bug_exp "copy_pat_bbox: negative copy";

(* unused? *)
fun copy_then_drop_pat_bbox bbn k st = 
    if k = 0 then update_unm_pat (RGGraph.kill_box bbn) st
    else if k = 1 then update_unm_pat (RGGraph.drop_box bbn) st
    else if k > 1 then 
      update_unm_pat (RGGraph.drop_box bbn 
                      o nfold (k - 1) (snd o RGGraph.copy1_box bbn)) st
    else raise bug_exp "copy_then_drop_pat_bbox: negative copy";


fun compute_scope_arities pn tn st = 
    let 
      val (pntab,pbboxntab) = RGGraph.get_adj_arities (get_unm_pat st) pn;
      val (tntab,tbboxntab) = RGGraph.get_adj_arities (get_unm_tgt st) tn;

      (* implicit: ntab k *)
      val ntab_sum = Vertex.NTab.fold (fn (n,i) => fn j => i + j);
      val sum_in_bboxtab = BBox.NTab.map_all (fn ntab => ntab_sum ntab 0)
      val summed_bbox_sum = BBox.NTab.fold (fn (n,i) => fn j => i +j);

      val pbboxsums = sum_in_bboxtab pbboxntab;
      val tbboxsums = sum_in_bboxtab tbboxntab;

      val tarity = 0 |> ntab_sum tntab |> summed_bbox_sum tbboxsums;
      val p_min_arity = 0 |> ntab_sum pntab;

      val scope_arity =  tarity - p_min_arity;
    in
      (scope_arity, pbboxsums)
    end;

(*  *)

(* find all expansions of bang boxes so that matching might be possible *)
(* IMPROVE: this be integrated into lookahead to do maximal early pruning? *)
(* returns seq of possible bang-box expanded graphs paired with the target 
   number of edges that need to be ignored. 

   invarient: tarity > ignore_arity;
   tarity is total arity of target; ignore_arity is amount that can be removed to make tarity = parity. 
*)
fun expand_pat_bboxes pn tn st = 
    let 
       val (scope_arity,pbboxsums) = compute_scope_arities pn tn st;
    in 
      if scope_arity < 0 then Seq.empty (* need at least pn edges *)
      else (* go through each bang box considering all expansions of it, 
              upto size limit, combined with each previous possibility *)
        BBox.NTab.fold
          (fn (bbn,i) => Seq.maps 
              (fn (ignore_arity,st) => 
                Seq.of_list
                (map (fn k => (ignore_arity - (k * i), copy_pat_bbox bbn k st))
                     (0 upto (ignore_arity div i)))))
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

(* implicit: pnv tnv st *)
val match_lfvertex = update_match oo Match.match_lfvertex;

(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
(* assumes called only during traversal: bang boxes of pn and tn are already 
   matching - we only need to consider vertices adj to these and update thing corespondingly. *)
fun match_2vertices_with_edges 
      (pn : Vertex.name) (* pattern vertex name *)
      (st : T) (* match state so far *)
      (tn : Vertex.name) = (* target vertex name *)
    let 
      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      val pd = RGGraph.get_vertex_data pat pn;
      val td = RGGraph.get_vertex_data tgt tn;
    in 
      (* happens when when a new vertex has just been chosen to start 
         matching from, and in traversal when there are no adj vertices *)
      if is_leaf_of_init_pat st pn then (* CHECK: !-boxed, map to edge *)
         (fst (compute_scope_arities pn tn st), 
          match_lfvertex pn (tn,td) st)
           |> edgematch_adj_verticies (pn,pd) (tn,td)
           |> Seq.map (del_pat_lfvertex pn o snd o snd)
      else 
        case match_vertex (pn,pd) (tn,td) st of 
          NONE => Seq.empty (* vertex does not match *)
        | SOME st2 => (* continue matching *)
          st2 |> expand_pat_bboxes pn tn
              |> (if Vertex.is_internal_data pd then (* strict match filter *)
                  Seq.map_filter (fn (x as (ignore_arity, st)) => 
                    if ignore_arity = 0 then SOME x else NONE)
                  else I)
              |> Seq.maps (match_self_loops (pn,pd) (tn,td))
              |> Seq.maps (edgematch_adj_verticies (pn,pd) (tn,td))
              |> Seq.map (del_pat_vertex pn o snd o snd) (* finished matching pn *)
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
fun get_next_pat_vertex st = 
    (case (Vertex.NSet.get_local_bot (get_unm_unboxed_pat_vs st)) of
      SOME n => SOME (UnBBoxed n)
    | NONE => 
      (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
         NONE => NONE
       | SOME n => SOME (BBoxed n)));

(* FIXME: this is used as a hack to avoid searching expansions of bang boxes which are disjoint form the rest of the pattern graph. *)
fun drop_all_bboxes_in_pat st = 
    let val pat = get_unm_pat st;
        val bboxlist = RGGraph.get_bbox_list pat;
    in 
      st |> update_unm_pat (fold RGGraph.kill_box bboxlist)
         |> update_bbmatch (BBoxMatch.add_kills bboxlist)
    end;

(* consider the next and-point in the agenda *)
fun do_next_match_choices st = 
    (case pop_agenda st 
      of NONE => (* nothing connected to previously matched parts *)
         (* get next pat vertex to match, non-boxed first *)
         (case get_next_pat_vertex st of 
            NONE => (* no pattern vertices left! *)
              FoundMatch st (* because we finished! *)
          | SOME (UnBBoxed pn) => 
            Progress (* choices over picking a new vertex in tgt *)
              (Seq.maps 
                 (match_2vertices_with_edges pn st)
                 (get_unboxed_tgt_vnseq st))
          | SOME (BBoxed pn) => (* FIXME: add initial bbox match *)
            (* all other things are in boxes, 
               so killing them all gives a valid matching... *)
            (* FIXME: find all box-unfoldings and macthes to target !-boxes *)
            FoundMatch (drop_all_bboxes_in_pat st)) 
       | SOME ((pn,tn),st2) => 
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
          Pretty.block [Pretty.str "BBMatch: "],
          BBoxMatch.pretty (#bbmatch rep),
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
