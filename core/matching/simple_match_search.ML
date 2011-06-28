(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Matching Algorithm Bang open-graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*
    This is a simple algorithm that traverses the pat in a fixed order, searching for locations in the target that it can match to. The algorithm tries to match up edges and vertices in all ways possible. This results in many symmertic matches. The algorithm is also rather inefficient in the way it explores expansions of bang-boxes: it tries all expansions next to the vertex being examined such that it has the right arity w.r.t. to the considered vertex in the target.

    FIXME: The algorithm is complete w.r.t. bangboxes: if all vertices in a component are in !-boxes, then the algorithm simply drop all bang-boxes.  
*)
signature SIMPLE_MATCH_SEARCH_SHARING
= sig
  structure Match : MATCH_SHARING 
  type T
end;


signature SIMPLE_MATCH_SEARCH
= sig
  structure Match : MATCH 
  structure Edge : EDGE
  structure Vertex : OVERTEX
  sharing Vertex.SharingWithIData = Match.G.OVertex.SharingWithIData;
  sharing Edge.EdgeSharingWithData = Match.G.Edge.EdgeSharingWithData;

  type T

  structure Sharing : SIMPLE_MATCH_SEARCH_SHARING
  sharing type Sharing.T = T 
  sharing Sharing.Match = Match.Sharing

  val init : Match.T 
             -> Match.G.T (* pat *)
             -> Match.G.T (* tgt *)
             -> Vertex.name Vertex.NTab.T (* initial agenda *)
             -> T (* initial match state *)

  (* match two graphs *)
  val match : (Match.G.Vertex.NSet.T * Match.G.Edge.NSet.T 
               * Match.G.BBox.NSet.T) (* other names to avoid when matching *)
              -> Match.G.T -> Match.G.T (* pat, tgt *)
              -> Match.T Seq.seq
  val match' : Match.T (* initial matching *)
               -> Match.G.T -> Match.G.T (* pat, tgt *)
               -> Vertex.name Vertex.NTab.T (* agenda *)
               -> Match.T Seq.seq

  (* parts of the matching algorithm *)
  datatype matchstep = FoundMatch of T | Progress of T Seq.seq
  val do_next_match_choices : T -> matchstep
  val match_2vertices_with_edges : 
      Vertex.name (* pat vertex *) 
      -> T (* state of matching  *)
      -> Vertex.name (* tgt vertex *)
      -> T Seq.seq (* possible new choices of state *)

  (* modifying match state *)
  val pop_agenda : T -> ((Vertex.name * Vertex.name) * T) option

  (* raises Match.match_exp () *)
  val add1_to_agenda : Vertex.name * Vertex.name -> T -> T
  
  (* internal get, update and set *)
  val get_agenda : T -> Vertex.name Vertex.NTab.T
  val get_match : T -> Match.T
  val get_tgt_vs : T -> T -> Vertex.NSet.T
  val get_unm_pat : T -> Match.G.T
  val get_unm_pat_vs : T -> Vertex.NSet.T
  val get_unm_tgt : T -> Match.G.T
  val get_unm_tgt_es : T -> Edge.NSet.T

  val update_agenda : (Vertex.name Vertex.NTab.T ->
                       Vertex.name Vertex.NTab.T) ->
                      T -> T
  val update_match : (Match.T -> Match.T) -> T -> T
  val update_unm_pat : (Match.G.T -> Match.G.T) -> T -> T
  val update_unm_tgt : (Match.G.T -> Match.G.T) -> T -> T

  val del_pat_edges : Edge.NSet.T -> T -> T

  val set_agenda : Vertex.name Vertex.NTab.T -> T -> T
  val set_match : Match.T -> T -> T

  (* shouldn't happen *)
  exception bug_exp of string; 

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val pretty_agenda : T -> Pretty.T
  val pretty_match_pair : Vertex.name * Vertex.name -> Pretty.T
  val pretty_next_pat_vertex : T -> Pretty.T
  val print : T -> unit
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor SimpleMatchSearch(Match : MATCH)
(*  : SIMPLE_MATCH_SEARCH *)
= struct

(* structure DB_MatchSearch = struct *)
structure BangGraph = Match.G;
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
fun get_unm_pat (MState rep) = #unm_pat rep;
fun get_pat_boundary (MState rep) = #pat_boundary rep;

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

val get_bbmatch = Match.get_bbmatch o get_match;
val get_names = Match.get_names o get_match;
val get_match_viso = Match.get_viso o get_match;
val get_bbmap =  Match.get_bbmap o get_match;

val get_unm_pat_vs = 
    V.NTab.get_nameset o G.get_vertices o get_unm_pat;
val get_unm_unboxed_pat_vs = G.get_unboxed o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
val get_unm_unboxed_tgt_vs = G.get_unboxed o get_unm_tgt;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    V.NTab.get_nameset o G.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    E.NTab.get_nameset o G.get_edges o get_unm_tgt;

val update_names = update_match o Match.update_names;

val set_names = update_match o Match.set_names;
val set_unm_pat = update_unm_pat o K;
val set_agenda = update_agenda o K;
val set_match = update_match o K;

(* implicit: e st *)
val del_pat_edge = update_unm_pat o G.delete_edge;
(* implicit: es st *)
val del_pat_edges = update_unm_pat o E.NSet.fold G.delete_edge;
(* implicit: v st *)
val del_pat_vertex = update_unm_pat o G.delete_vertex;

(* deletion of tgt edges as we proceed *)
val del_tgt_edge = update_unm_tgt o G.delete_edge;
val del_tgt_edges = update_unm_tgt o E.NSet.fold G.delete_edge;


fun ins1_to_agenda (pat_n, tgt_n) st = 
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

fun add1_to_agenda (pat_n,tgt_n) st = 
    (case try_add1_to_agenda (pat_n,tgt_n) st of SOME st' => st'
     | NONE => raise Match.match_exp ()); 

(* try to match the bang-box's of the given pattern and tgt node:
   mapping must be injective, and tgt *)
(* IMPROVE?: maybe can do lazy bbox unfolding, and have bbox copying happen
   here? probbaly need generic notion of symmetry and search *)
fun try_match_bboxes (pvn1,tvn1) (pvn2,tvn2) st = 
    let val pat = get_unm_pat st;
        val tgt = get_unm_tgt st;
    in
      (case BangGraph.lookup_box_of tgt tvn2 of 
         NONE => 
         (case BangGraph.lookup_box_of pat pvn2 of 
            NONE => SOME st (* no bboxes; continue as normal *)
          | SOME pbbn2 => (* some pat bbox, no tgt bbox *)
            (case BBMap.lookup_domf (get_bbmap st) pbbn2 of
               NONE => SOME (st |> update_unm_pat (BangGraph.drop_box pbbn2)
                                |> update_match (Match.add_bbox_drop pbbn2))
             | SOME _ => NONE)) (* pat bbox already mapped; can't be dropped  *)
       | SOME tbbn2 => 
         (case BangGraph.lookup_box_of pat pvn2 of 
            NONE => NONE (* target bbox, but no pat bbox => fail *)
          | SOME pbbn2 => (* both in bboxes *)
            (case BBMap.lookup_domf (get_bbmap st) pbbn2 of
               NONE => (* pattern bbox not yet mapped - make new mapping *)
               (case BangGraph.lookup_box_of tgt tvn1 of
                  NONE => SOME (st |> update_match (Match.add_bbox_map pbbn2 tbbn2))
                | SOME tbbn1 =>
                  (* two adj pat !-boxes can't match single adj target one *)
                  if (BBox.name_eq (tbbn1,tbbn2)) andalso 
                     not (BBox.name_eq (BangGraph.get_box_of pat pvn1,pbbn2))
                  then NONE
                  else 
                    SOME (st |> update_match (Match.add_bbox_map pbbn2 tbbn2)))
             | SOME tbbn2' => (* already mapped to a tgt bang box *)
               if (BBox.name_eq (tbbn2,tbbn2'))
               then SOME st (* already mapped to this bbox *)
               else NONE))) (* already mapped elsewhere *)
    end;

(* AK: bbox_match_exp shouldn't be raised under normal failed matches. in the mean time, raising match_exp *)
exception bbox_match_exp of unit;
fun match_bboxes pnv tnv st = 
    (case try_match_bboxes pnv tnv st of NONE => raise bbox_match_exp () (*Match.match_exp ()*)
     | SOME st' => st');


(* raises Match.match_exp on failure *)
fun edgematch1 (pt as (pvn1,tvn1)) (pe as (peh1,(pe_dir,pe_d)),pvn2)
                                   (te as (teh1,(te_dir,te_d)),tvn2) st =
    (* for self-loops, which will already be fully mapped, skip (the target
       vertex will have already been removed too) *)
    if Match.emap_contains (get_match st) (peh1,teh1) then st 
    else
    let 
      val peh2 = G.Edge.Ehn.other peh1;
      val teh2 = G.Edge.Ehn.other teh1;
      val pat = get_unm_pat st;
    in  
      st |> update_match (Match.match_edge_data pe_d te_d)
         |> (if G.is_boundary pat pvn2
             then (* pvn2 is a boundary vertex *)
              del_pat_vertex pvn2
              o update_match 
                  ((case (pe_dir,te_dir) 
                      of (G.Directed, G.Directed) => 
                         (Match.dir_match_edgehalf_name' peh1 teh1)
                       | (G.UnDirected, G.UnDirected) => 
                         (Match.match_edgehalf_name' peh1 teh1)
                       | _ => raise Match.match_exp ())
                    o Match.match_boundary pvn2 tvn2) 
             else (* pvn2 is an internal vertex *)
               del_tgt_edge (E.Ehn.get_ename teh1)
               o update_match 
                  ((case (pe_dir,te_dir) 
                      of (G.Directed, G.Directed) => 
                          (Match.dir_match_edgehalf_name' peh1 teh1
                           o Match.dir_match_edgehalf_name' peh2 teh2)
                       | (G.UnDirected, G.UnDirected) => 
                         (Match.match_edgehalf_name' peh1 teh1
                           o Match.match_edgehalf_name' peh2 teh2)
                       | _ => raise Match.match_exp ())
                   o Match.match_vertex_name pvn2 tvn2)
                (* don't add to the agenda if self-loop edge *)
                o (if Vertex.name_eq (pvn2,pvn1) then I 
                    else add1_to_agenda (pvn2,tvn2)))
         |> match_bboxes pt (pvn2,tvn2)
    end;

(* THINK: make a version that builds in edge data always being symmetric: 
   we can always take the first edge that matches *)
(* THINK: use difference pivoted lists for faster/more efficient storing of 
   unmatched stuff? *)
(* Treats each edge as unique, matches all permutations of matching data *)
fun try_edgematch1 pt pe_pvn2 st te_tvn2 (unmatched_tes,matches) =
    let 
      (* add that this target edge can be used by all matching of 
         further pattern edges *) 
      val matches' = map (fn (unmatched_es,st) => 
                         (te_tvn2 :: unmatched_tes, st)) matches
    in
      (te_tvn2::unmatched_tes,
       (((unmatched_tes, edgematch1 pt pe_pvn2 te_tvn2 st) :: matches') 
        handle Match.match_exp _ => matches'))
    end;

(* all possible target edge matches, if none then fail *)
(* raises Match.match_exp on failure *)
fun edge_match_adj_to_pedge (pt as (pvn1,tvn1))
                            (pe as (peh,(pe_dir,pe_d))) pvn2 
                            agenda_and_st_choices = 
    (case maps (fn (unmatched_tes, st) => 
                snd (fold (try_edgematch1 pt (pe,pvn2) st) 
                          unmatched_tes 
                          ([], [])) (* start with no matches, no unused tes *)
               ) agenda_and_st_choices
     of [] => raise Match.match_exp () (* no matches found for this pat e *)
      | agenda_and_st_choices' => agenda_and_st_choices');

(* match up all adjacent edges of chosen pattern and target vertex *)
(* raises Match.match_exp on failure *)
fun edgematch_all_adj (pt as (pvn1,tvn1)) st = 
    let val adj_list = 
        G.fold_adj (fn te as (ehn,_) => fn tvn2 => fn l => (te,tvn2)::l)
                   (get_unm_tgt st) tvn1 [];
        (* TODO: undir edges which are self-loops will always have two matches, one for each directon. Simple-dup removal doesn't work: all half-edges much be matched.  *)
    in 
      G.fold_adj (edge_match_adj_to_pedge pt)
                 (get_unm_pat st) pvn1 [(adj_list,st)]
        |> map snd
        |> Seq.of_list
    end;
(* simple dup-removal for above does not work! Simple-dup removal doesn't work: all half-edges much be matched. 
(fn te as (ehn,_) => fn tvn2 => fn (l,eset) => 
                    let val e = E.Ehn.get_ename ehn in 
                      if (E.NSet.contains eset e) 
                      then (l,eset) ** avoid counting self-loop-edges twice **
                      else ((te,tvn2)::l, E.NSet.add e eset) end)
                   (get_unm_tgt st) tvn1 ([],E.NSet.empty);
*)



(* copy a single bang-box *)
fun copy1_pat_bbox bbn st = 
    let val (names as vs,es,bs) = get_names st
        val (bbn2,((vrn',ern',brn'),pat2)) = 
          G.copy1_box bbn 
            (( G.Vertex.Rnm.mk_from_avoids vs, 
               G.Edge.Rnm.mk_from_avoids es,
               G.BBox.Rnm.mk_from_avoids bs), 
             get_unm_pat st);
    in
      st |> set_unm_pat pat2
         |> set_names (Vertex.Rnm.get_avoids vrn', 
                       Edge.Rnm.get_avoids ern', 
                       BBox.Rnm.get_avoids brn')
         |> update_match (Match.add_bbox_copy 
              (bbn,(V.Rnm.get_nmap vrn',E.Rnm.get_nmap ern'),bbn2))
    end;

exception bug_exp of string; 
(* !-box ops *)
fun nfold 0 f a = a
  | nfold n f a = nfold (n - 1) f (f a);
fun copy_pat_bbox bbn k st = 
    if k = 0 then st |> update_unm_pat (G.kill_box bbn) 
                     |> update_match (Match.add_bbox_kills [bbn])
    else if k = 1 then st
    else if k > 1 then 
      nfold (k - 1) (copy1_pat_bbox bbn) st
    else raise bug_exp "copy_pat_bbox: negative copy value";

fun compute_scope_arities pn tn st = 
    let 
      val (pntab,pbboxntab) = 
          G.get_adj_arities_and_bang_arities (get_unm_pat st) pn;
      val (tntab,tbboxntab) = 
          G.get_adj_arities_and_bang_arities (get_unm_tgt st) tn;

      (* implicit: ntab k *)
      val ntab_sum = Vertex.NTab.fold (fn (n,i) => Arity.add i);
      val sum_in_bboxtab = 
          BBox.NTab.map_all (fn n => fn ntab => ntab_sum ntab Arity.zero);
      val summed_bbox_sum = BBox.NTab.fold (fn (n,i) => Arity.add i);

      val pbboxsums = sum_in_bboxtab pbboxntab;
      val tbboxsums = sum_in_bboxtab tbboxntab;

      val tarity = Arity.zero |> ntab_sum tntab |> summed_bbox_sum tbboxsums;
      val p_min_arity = Arity.zero |> ntab_sum pntab;

      val scope_arity =  Arity.subtract tarity p_min_arity;
    in
      (scope_arity, pbboxsums)
    end;

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
        |> Seq.filter (Arity.is_zero o fst)
        |> Seq.map snd
    end;

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
      st |> update_match (Match.match_vertex (pn,pd) (tn,td))
         |> expand_adj_pat_bboxes pn tn
         |> Seq.maps (edgematch_all_adj (pn,tn))
         (* force evaluation so as to catch exception *) 
         (* FIXME: fix this hack *)
         |> Seq.list_of |> Seq.of_list 
         (* remove pattern vertex, ignore now redudnent info *)
         |> Seq.map (del_pat_vertex pn)
    end handle Match.match_exp _ => Seq.empty;


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

(* *)
fun match' m pat tgt initagenda = 
    let 
      (* given an or-choice agenda of states (seq), apply progress steps 
         to search tree, until we get a match, then give that back as first 
         elem of lazy list. We can then rest until asked for the next thing! *)
      fun match_search or_agenda () = 
          (case Seq.pull or_agenda of 
             NONE => NONE
           | SOME (st,more_or_choices) =>
             (case do_next_match_choices st of 
                FoundMatch st2 => 
                SOME (get_match st2, Seq.make (match_search more_or_choices))
              | Progress new_or_choices => 
                match_search (Seq.append new_or_choices more_or_choices) ()));

      val init_st = (init m pat tgt initagenda)
    in Seq.make (match_search (Seq.single init_st)) end;

fun match names pat tgt = 
    match' (Match.init names pat tgt) pat tgt Vertex.NTab.empty;

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


structure Sharing : SIMPLE_MATCH_SEARCH_SHARING =
struct
  type T = T; 
  structure Match = Match.Sharing;
end;



end; (* MatchSearch structure *)


