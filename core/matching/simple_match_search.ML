(* Simple graph matching
 *
 * BASIC_MATCH_SEARCH, BASIC_MATCH_SEARCH_SHARING,
 * SimpleMatchSearch(Match: MATCH): BASIC_MATCH_SEARCH
 *
 * Searches for matchings of a pattern graph on a target graph in a
 * fairly naive way.
 *)

signature BASIC_MATCH_SEARCH_SHARING
= sig
  structure Match : MATCH_SHARING 
end;

signature BASIC_MATCH_SEARCH
= sig
  structure Log : LOG;

  (* A description of a match (depends on graph type) *)
  structure Match : MATCH
  
  structure Sharing : BASIC_MATCH_SEARCH_SHARING 
  sharing Sharing.Match = Match.Sharing

  (* Searches for matches *)
  val match : (V.NSet.T * E.NSet.T * B.NSet.T) (* names to avoid *)
              -> Match.G.T (* pattern graph *)
	      -> Match.G.T (* target graph *)
              -> Match.T Seq.seq (* (lazy) list of matches *)
  
end;

functor SimpleMatchSearch(Match : MATCH)
= struct

  structure Log : LOG = Log(val init_level = 0);

  structure G = Match.G;

  structure Match = Match;

  structure BBMatch = Match.BBMatch;

  datatype vertex_kind_name = 
           BBoxed of V.name 
         | UnBBoxed of V.name;

  datatype T = 
           MState of 
           {(* and-agenda = (pat -> tgt) = we are commited to matching these *) 
            agenda : V.name V.NTab.T, 
            (* global vertex and edge matching info *)
            match : Match.T, 
            (* the full pattern graph, with its expansions so far *)
            pat : G.T, 
            (* the unmatched pattern vertex names, with a flag for being 
               unboxed *)
            unm_pat_vs : vertex_kind_name Seq.seq, 
            (* tgt with matched edges removed as we match them *)
            unm_tgt : G.T 
           };


  fun name_of_vertex_kind_name (BBoxed n) = n
    | name_of_vertex_kind_name (UnBBoxed n) = n;

  (* Note w.r.t. evaluation thing: we generally rely on pat boundary and unboxed being monotomic (not affecting existing vertices during the matching process.) Gives back seq of unboxed internal names, then boxed internal names. *)
  fun pat_vertex_agenda_seq' pat vs = 
      let val unboxed = (G.get_unbboxed pat);
          val boxed = (G.get_bboxed pat);
      in
        Seq.map_filter 
          (fn n => if G.is_boundary pat (name_of_vertex_kind_name n) then NONE 
           else SOME n)
          (Seq.append (Seq.map UnBBoxed (V.NSet.seq_of unboxed)) 
                      (Seq.map BBoxed (V.NSet.seq_of boxed)))
      end;
  fun pat_vertex_agenda_seq pat = 
      pat_vertex_agenda_seq' pat (G.get_vnames pat);

  (* initial matching state *)
  fun init match pat tgt agenda =
      MState { (* initially used names are those of the pattern graph *)
               agenda = agenda, 
               match = match, 
               pat = pat,
               unm_pat_vs = pat_vertex_agenda_seq pat,
               unm_tgt = tgt };

  (* tools *)
  fun get_agenda (MState rep) = #agenda rep;
  fun get_match (MState rep) = #match rep;
  fun get_pat (MState rep) = #pat rep;
  fun get_unm_pat_vs (MState rep) = #unm_pat_vs rep;

  val get_pat_boundary = G.get_boundary o get_pat;

  (* *)
  fun update_agenda f (MState rep) = 
      MState { agenda = f (#agenda rep), match = #match rep,
               pat = #pat rep,
               unm_pat_vs = #unm_pat_vs rep, 
               unm_tgt = #unm_tgt rep
             };
  fun update_match f (MState rep) = 
      MState { agenda = #agenda rep, match = f (#match rep),
               pat = #pat rep,
               unm_pat_vs = #unm_pat_vs rep, 
               unm_tgt = #unm_tgt rep
             };
  fun update_unm_pat_vs f (MState rep) = 
      MState { agenda = #agenda rep, match = #match rep,
               pat = #pat rep,
               unm_pat_vs = f (#unm_pat_vs rep), 
               unm_tgt = #unm_tgt rep
             };
  fun update_pat f (MState rep) = 
      MState { agenda = #agenda rep, match = #match rep,
               pat = f (#pat rep),
               unm_pat_vs = #unm_pat_vs rep, 
               unm_tgt = #unm_tgt rep
             };
  fun update_unm_tgt f (MState rep) = 
      MState { agenda = #agenda rep, match = #match rep,
               pat = #pat rep,
               unm_pat_vs = #unm_pat_vs rep, 
               unm_tgt = f(#unm_tgt rep)
             };

  val get_bbmatch = Match.get_bbmatch o get_match;
  val get_names = Match.get_names o get_match;
  val get_match_viso = Match.get_viso o get_match;
  val get_bbmap =  Match.get_bbmap o get_match;

  fun get_unm_tgt (MState rep) = #unm_tgt rep;
  val get_unm_unboxed_tgt_vs = G.get_unbboxed o get_unm_tgt;
  fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
      V.NTab.get_nameset o G.get_vertices o get_unm_tgt;
  val get_unm_tgt_es =
      E.NTab.get_nameset o G.get_edges o get_unm_tgt;

  val update_names = update_match o Match.update_names;

  val set_names = update_match o Match.set_names;
  val set_unm_pat_vs = update_unm_pat_vs o K;
  val set_pat = update_pat o K;
  val set_agenda = update_agenda o K;
  val set_match = update_match o K;

  (* deletion of tgt edges as we proceed *)
  val del_tgt_edge = update_unm_tgt o G.delete_edge;
  val del_tgt_edges = update_unm_tgt o E.NSet.fold G.delete_edge;

  (* pop first vertex pair (pat,tgt) from agenda; get internal vertices before external ones - required by edge-matching - and probably more efficient too. IMPROVE: store a maximally internal traversal of the pattern (as the pre-agenda) which can then connected to the tgt vertices as the agenda. This will save repeated search for non-boundary vertices - also the basis of more efficient algorithm. (see also get_next_pat_vertex) *)
  fun pop_agenda st = 
      let val agenda = get_agenda st;
          val pat = get_pat st;
      in 
        case V.NTab.find (fn (n1,n2) => not (G.is_boundary pat n1)) agenda of
          NONE => (* no internal vertices in the agenda *) 
            (case V.NTab.pull_elem agenda
              of NONE => NONE 
               | SOME (n1n2,agenda2) => SOME (n1n2,set_agenda agenda2 st))
         | SOME (n1n2 as (n1,n2)) => 
           SOME (n1n2,set_agenda (V.NTab.delete n1 agenda) st)
      end;

  (* *)
  (* pretty printing *)
  fun pretty_match_pair (a,b) = 
      Pretty.block [Pretty.str "(",
                    V.pretty_name a,
                    Pretty.str "->",
                    V.pretty_name b,
                    Pretty.str ")"];

  fun pretty_get_unm_pat_vs st = 
      Pretty.list "unmatched_pat_vs: [" "]" 
        (map (V.pretty_name o name_of_vertex_kind_name) 
             (Seq.list_of (get_unm_pat_vs st)));

  fun get_agenda_list st = 
      (case pop_agenda st of NONE => []
         | SOME (x,st2) => x :: get_agenda_list st2);

  fun pretty_agenda st = 
      Pretty.list "agenda: [" "]" (map pretty_match_pair (get_agenda_list st));

  fun pretty (st as MState rep) = 
      Pretty.block 
        [Pretty.str "MatchSearch {",
         Pretty.chunks 
           [Match.pretty (#match rep),
            pretty_get_unm_pat_vs st,
            pretty_agenda st,
            Pretty.block [Pretty.str "pat: "],
            G.pretty (#pat rep),
            Pretty.block [Pretty.str "Unmatched tgt: "],
            G.pretty (#unm_tgt rep)
           ],
         Pretty.str "}"];

  val print = Pretty.writeln o pretty;

  (* *)
  fun ins1_to_agenda (pat_n, tgt_n) st = 
      update_agenda (V.NTab.ins (pat_n,tgt_n)) st;

  (* add vertex-matching to agenda if consistent (consistent = not in
     agenda, or already in the agenda but matched to same thing - in
     which case do nothing) *)
  fun try_add1_to_agenda (pat_n,tgt_n) st = 
      let val agenda = get_agenda st
      in case V.NTab.lookup agenda pat_n of 
          NONE => SOME (set_agenda (V.NTab.ins (pat_n,tgt_n) agenda) st)
        | SOME tgt_n2 => 
          (if V.name_eq (tgt_n,tgt_n2) 
           then SOME st (* already in agenda *)
           else NONE (* a pat-vertex can only be mapped one tgt-vertex. *))
      end;

  fun add1_to_agenda (pat_n,tgt_n) st = 
      (case try_add1_to_agenda (pat_n,tgt_n) st of SOME st' => st'
       | NONE => raise Match.match_exp ()); 

  (* try to match the bang-boxes of the given pattern and tgt node:
     mapping must be injective, and tgt *)
  (* IMPROVE?: maybe can do lazy bbox unfolding, and have bbox copying happen
     here? probbaly need generic notion of symmetry and search *)
  fun try_match_bboxes (pvn1,tvn1) (pvn2,tvn2) st = 
      let val pat = get_pat st;
          val tgt = get_unm_tgt st;
      in
        (case G.lookup_bbox_of tgt tvn2 of 
           NONE => 
           (case G.lookup_bbox_of pat pvn2 of 
              NONE => SOME st (* no bboxes; continue as normal *)
            | SOME pbbn2 => (* some pat bbox, no tgt bbox *)
              (case BMap.lookup_domf (get_bbmap st) pbbn2 of
                 NONE => SOME (st |> update_pat (G.drop_bbox pbbn2)
                                  |> update_match (Match.add_bbox_drop pbbn2))
               | SOME _ => NONE)) (* pat bbox already mapped; can't be dropped  *)
         | SOME tbbn2 => 
           (case G.lookup_bbox_of pat pvn2 of 
              NONE => NONE (* target bbox, but no pat bbox => fail *)
            | SOME pbbn2 => (* both in bboxes *)
              (case BMap.lookup_domf (get_bbmap st) pbbn2 of
                 NONE => (* pattern bbox not yet mapped - make new mapping *)
                 (case G.lookup_bbox_of tgt tvn1 of
                    NONE => SOME (st |> update_match (Match.add_bbox_map pbbn2 tbbn2))
                  | SOME tbbn1 =>
                    (* two adj pat !-boxes can't match single adj target one *)
                    if (B.name_eq (tbbn1,tbbn2)) andalso 
                       not (B.name_eq (G.get_bbox_of pat pvn1,pbbn2))
                    then NONE
                    else 
                      SOME (st |> update_match (Match.add_bbox_map pbbn2 tbbn2)))
               | SOME tbbn2' => (* already mapped to a tgt bang box *)
                 if (B.name_eq (tbbn2,tbbn2'))
                 then SOME st (* already mapped to this bbox *)
                 else NONE))) (* already mapped elsewhere *)
      end;

  (* AK: bbox_match_exp shouldn't be raised under normal failed matches. in the mean time, raising match_exp *)
  exception bbox_match_exp of unit;
  fun match_bboxes ptv1 ptv2 st = 
      (case try_match_bboxes ptv1 ptv2 st of NONE => raise Match.match_exp ()
       | SOME st' => st');


  (* raises Match.match_exp on failure *)
  fun edgematch1 (pt as (pvn1,tvn1)) (pe as (peh1,(pe_dir,pe_d)),pvn2)
                                     (te as (teh1,(te_dir,te_d)),tvn2) st =
      (* for self-loops, which will already be fully mapped, skip (the target
         vertex will have already been removed too) *)
      if Match.emap_contains (get_match st) (peh1,teh1) then st 
      else
      let 
        val _ = Log.log 3 ("edgematch1: " 
                ^ "pvn1=" ^ (V.string_of_name pvn1) 
                ^ "; tvn1=" ^ (V.string_of_name tvn1) 
                ^ "; peh1=" ^ (H.string_of_name peh1) 
                ^ "; teh1=" ^ (H.string_of_name teh1) 
                ^ "\n" ^ (Pretty.string_of (pretty st)));
        (* get the ends close to the matched vertex; peh1 and teh1 are ends
           touching the adjacent vertex. *)
        val peh2 = H.other peh1; 
        val teh2 = H.other teh1;
        val pat = get_pat st; 
      in  
        st |> update_match (Match.match_edge_data pe_d te_d)
           (* must try to match bboxes before deleting vertex from pat. *)
           |> match_bboxes pt (pvn2,tvn2)
           (* set vertex and edge match and delete vertex from pat*)
           |> (if G.is_boundary pat pvn2
               then (* pvn2 is a boundary vertex *)
                update_match 
                  ((fn m => case Match.other_plugging_half_of_same_edge 
                                  m peh2 teh2 
                            of NONE => m
                             | SOME peh2_plug => 
                              (Match.add_plugging 
                                (G.get_ehn_vname pat peh1)
                                (G.get_ehn_vname pat 
                                    (H.other peh2_plug))
                                m))
                   o (case (pe_dir,te_dir) 
                      of (G.Directed, G.Directed) => 
                         (Match.dir_match_edgehalf_name' peh2 teh2)
                       | (G.UnDirected, G.UnDirected) => 
                         (Match.match_edgehalf_name' peh2 teh2)
                       | _ => raise Match.match_exp ())
                    o Match.match_boundary pvn2 tvn2)
               else (* pvn2 is an internal vertex *)
                 del_tgt_edge (H.get_ename teh1)
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
                  o (if V.name_eq (pvn2,pvn1) then I 
                      else add1_to_agenda (pvn2,tvn2)))
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
  (* peh is the edge-half closest to pvn2 *)
  (* raises Match.match_exp on failure *)
  fun edge_match_adj_to_pedge (pt as (pvn1,tvn1))
                              (pe as (peh,(pe_dir,pe_d))) pvn2 
                              agenda_and_st_choices = 
      (Log.log 3 ("edge_match_adj_to_pedge:" 
        ^ " pvn1=" ^ (V.string_of_name pvn1) 
        ^ "; tvn1=" ^ (V.string_of_name tvn1) 
        ^ "; pe=" ^ (H.string_of_name peh) 
        ^ "; pvn2=" ^ (V.string_of_name pvn2));
      case maps (fn (unmatched_tes, st) =>
                  (* if pattern edge is already matched to something, skip it *)
                  if (Match.emap_contains_pat_ehn (get_match st) 
                        (H.other peh)) then
                    (Log.log 3 ("edge_match_adj_to_pedge: skipped."); 
                     [(unmatched_tes, st)])
                  else
                  (* try to match the pattern edge to each target edge *)
                    snd (fold (try_edgematch1 pt (pe,pvn2) st) 
                            unmatched_tes 
                            ([], [])) (* start with no matches, no unused tes *)
                 ) agenda_and_st_choices
       of [] => (Log.log 3 ("edge_match_adj_to_pedge: failed."); 
                 raise Match.match_exp ()) (* no matches found for this pat e *)
        | agenda_and_st_choices' => agenda_and_st_choices');

  (* match up all adjacent edges of chosen pattern and target vertex *)
  (* raises Match.match_exp on failure (for early exit of loop) *)
  fun edgematch_all_adj (pt as (pvn1,tvn1)) st = 
      let 
        val adj_list = 
          G.fold_adj_halfedges (fn te as (ehn,_) => fn tvn2 => fn l => (te,tvn2)::l)
                     (get_unm_tgt st) tvn1 [];
        fun pretty_adj_list g v =
            let 
              val adj_list = 
              G.fold_adj_halfedges (fn te => fn v2 => fn l => (te,v2)::l)
                         g v [];
            in 
              (Pretty.list "[" "]" (map
                (fn ((eh,_),vn) => Pretty.block [Pretty.str "(e=", H.pretty_name eh, Pretty.str ",v=", V.pretty_name vn, Pretty.str ")"]) adj_list))
            end;
 
        val _ = Log.log 3 ("edgematch_all_adj:" 
          ^ " pvn1=" ^ (V.string_of_name pvn1)
          ^ "(adj=" ^ (Pretty.string_of (pretty_adj_list (get_pat st) pvn1))
          ^ "); tvn1=" ^ (V.string_of_name tvn1)
          ^ "(adj=" ^ (Pretty.string_of (pretty_adj_list (get_unm_tgt st) tvn1))
          );

          (* TODO: undir edges which are self-loops will always have two matches, one for each directon. Simple-dup removal doesn't work: all half-edges must be matched.  *)
      in 
        G.fold_adj_halfedges (edge_match_adj_to_pedge pt)
                   (get_pat st) pvn1 
                   [(adj_list,st)] (* target agenda of things to be matched, 
                                      current state *)
          |> map snd
          |> Seq.of_list
      end;
  (* Simple-dup removal doesn't work: all half-edges much be matched...
  (fn te as (ehn,_) => fn tvn2 => fn (l,eset) => 
                      let val e = H.get_ename ehn in 
                        if (E.NSet.contains eset e) 
                        then (l,eset) ** avoid counting self-loop-edges twice **
                        else ((te,tvn2)::l, E.NSet.add e eset) end)
                     (get_unm_tgt st) tvn1 ([],E.NSet.empty);
  *)

  (* copy a single bang-box *)
  fun copy1_pat_bbox bbn st = 
      let val (names as vs,es,bs) = get_names st
          val (bbn2,((vrn',ern',brn'),pat2)) = 
            G.copy1_bbox bbn 
              (( V.Rnm.mk_from_avoids vs, 
                 E.Rnm.mk_from_avoids es,
                 B.Rnm.mk_from_avoids bs), 
               get_pat st);
      in
        st |> set_pat pat2
           |> set_names (V.Rnm.get_avoids vrn', 
                         E.Rnm.get_avoids ern', 
                         B.Rnm.get_avoids brn')
           |> update_match (Match.add_bbox_copy 
                (bbn,(V.Rnm.get_nmap vrn',E.Rnm.get_nmap ern'),bbn2))
      end;

  exception bug_exp of string; 
  (* !-box ops *)
  fun nfold 0 f a = a
    | nfold n f a = nfold (n - 1) f (f a);
  fun copy_pat_bbox bbn k st = 
      if k = 0 then st |> update_pat (G.kill_bbox bbn) 
                       |> update_match (Match.add_bbox_kills [bbn])
      else if k = 1 then st
      else if k > 1 then 
        nfold (k - 1) (copy1_pat_bbox bbn) st
      else raise bug_exp "copy_pat_bbox: negative copy value";

  fun compute_scope_arities pn tn st = 
      let 
        val (pntab,pbboxntab) = 
            G.get_filtered_adj_arities_and_bang_arities
              (fn ((ehn,(edir,ed)),vn) => 
                  Match.emap_contains_pat_ehn (get_match st) ehn)
              (get_pat st) pn;
        val (tntab,tbboxntab) = 
            G.get_adj_arities_and_bang_arities (get_unm_tgt st) tn;

        (* implicit: ntab k *)
        val ntab_sum = V.NTab.fold (fn (n,i) => Arity.add i);
        val sum_in_bboxtab = 
            B.NTab.map_all (fn n => fn ntab => ntab_sum ntab Arity.zero);
        val summed_bbox_sum = B.NTab.fold (fn (n,i) => Arity.add i);

        val pbboxsums = sum_in_bboxtab pbboxntab;
        val tbboxsums = sum_in_bboxtab tbboxntab;

        val tarity = Arity.zero |> ntab_sum tntab |> summed_bbox_sum tbboxsums;
        val p_min_arity = Arity.zero |> ntab_sum pntab;

        val scope_arity =  Arity.subtract tarity p_min_arity;
      in
        (scope_arity, pbboxsums)
      end;

  (* find all expansions of bang boxes so that matching might be possible *)
  (* IMPROVE: integrate into lookahead to do maximal early pruning? delay actual copying of bang box until we get an exact match to the number of edges? 
  FIXME: only decide a bang-box expansion once, on first entry: this fixes a bang-boxes expansion: we must not allow a second vertex incident to the bang-box to change the expansion. 
  *)
  (* returns seq of possible bang-box expanded graphs paired with the target 
     number of edges that need to be ignored. 

     invarient: tarity > ignore_arity;
     tarity is total arity of target; ignore_arity is amount that can be removed to make tarity = parity. 
  *)
  fun expand_adj_pat_bboxes pn tn st = 
      let 
         val (scope_arity,pbboxsums) = compute_scope_arities pn tn st;
         val _ = Log.log 3 ("expand_adj_pat_bboxes: "
           ^ "pn=" ^ (V.string_of_name pn) 
           ^ "; tn=" ^ (V.string_of_name tn)
           ^ "; scope_arity=" ^ (Pretty.string_of (Arity.pretty scope_arity))
           ^ "; pbboxsums=" ^ (Pretty.string_of (Arity.pretty scope_arity)));
      in 
        if Arity.has_negative_incidence scope_arity 
        then Seq.empty (* Fail: need at least pn edges *)
        else (* go through each bang box considering all expansions of it, 
                upto size limit, combined with each previous possibility *)
          B.NTab.fold
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
        (pn : V.name) (* pattern vertex name *)
        (st : T) (* match state so far *)
        (tn : V.name) = (* target vertex name *)
      let 
        val pat = get_pat st;
        val tgt = get_unm_tgt st;
        val pd = G.get_vertex_data pat pn;
        val td = G.get_vertex_data tgt tn;
        val _ = Log.log 3 ("match_2vertices_with_edges: " 
          ^ "pn=" ^ (V.string_of_name pn) ^ "; tn=" ^ (V.string_of_name tn));
      in (* check data matches *)
        st |> update_match (Match.match_vertex (pn,pd) (tn,td))
           |> expand_adj_pat_bboxes pn tn
           |> Seq.maps (edgematch_all_adj (pn,tn))
           (* force evaluation so as to catch exception *) 
           (* FIXME: fix this hack *)
           |> Seq.list_of |> Seq.of_list
      end handle Match.match_exp _ => Seq.empty;


  (* datatype for steps of matching *)
  datatype matchstep =
           FoundMatch of T
         | Progress of T Seq.seq (* or choices in matching *)

  (* get tgt vertex names as a seq *)
  val get_unbboxed_tgt_vnseq = 
      Seq.of_list o V.NSet.list_of o get_unm_unboxed_tgt_vs;

  (* *)
  fun pull_next_pat_vertex st = 
      case Seq.pull (get_unm_pat_vs st) of NONE => NONE
      | SOME (n : vertex_kind_name, more) =>
        SOME (n, set_unm_pat_vs more st);

  (* FIXME: this is used as a hack to avoid searching expansions of bang boxes which are disjoint form the rest of the pattern graph. *)
  fun drop_all_bboxes_in_pat st = 
      let val pat = get_pat st;
          val bboxlist = G.get_bbox_list pat;
      in 
        st |> update_pat (fold G.kill_bbox bboxlist)
           |> update_match (Match.add_bbox_kills bboxlist)
      end;

  (* consider the next and-point in the agenda *)
  fun do_next_match_choices st = 
      (case pop_agenda st 
        of NONE => (* nothing connected to previously matched parts *)
           (* get next pat vertex to match, non-boxed first *)
           (case pull_next_pat_vertex st of 
              NONE => (* no pattern vertices left! *)
                (Log.log 3 ("FoundMatch " 
                  ^ "\n" ^ (Pretty.string_of (pretty st)));
                FoundMatch st) (* because we finished! *)
            (* have a pat vertex outside bangboxes... and we've pulled it 
               from the unmatched pattern list. *)
            | SOME (UnBBoxed pn, st) => 
              (Log.log 3 ("Progress: (UnBBoxed " ^ (V.string_of_name pn) ^ ")"
                ^ "\n" ^ (Pretty.string_of (pretty st)));
               Progress 
                (Seq.maps (* choices over picking a new vertex in tgt *)
                   (match_2vertices_with_edges pn st)
                   (get_unbboxed_tgt_vnseq st)))
            | SOME (BBoxed pn, st) => (* FIXME: add initial bbox match *)
              (* all other things are in boxes, 
                 so killing them all gives a valid matching... *)
              (* FIXME: find all box-unfoldings and macthes to target !-boxes *)
              (Log.log 3 ("Progress: (BBoxed " ^ (V.string_of_name pn) ^ ")"
                ^ "\n" ^ (Pretty.string_of (pretty st)));
              FoundMatch (drop_all_bboxes_in_pat st)))
         | SOME ((pn,tn),st2) => (* match these things, only question is how ...*)
           (Log.log 3 ("Progress: (UnBBoxed " ^ (V.string_of_name pn) ^ ")"
                ^ "\n" ^ (Pretty.string_of (pretty st)));
           Progress (* deal with first prescribed and-choice *)
             (match_2vertices_with_edges pn st2 tn)));

  (* *)
  fun match' m pat tgt initagenda = 
      let 
        (* given an or-choice agenda of states (seq), apply progress steps 
           to search tree, until we get a match, then give that back as first 
           elem of lazy list. We can then rest until asked for the next thing! *)
        fun match_search or_agenda () = 
            (case Seq.pull or_agenda of 
               NONE => (Log.log 3 ("Seq.pull or_agenda = NONE"); NONE)
             | SOME (st,more_or_choices) =>
               (case do_next_match_choices st of 
                  FoundMatch st2 => 
                  SOME (get_match st2, Seq.make (match_search more_or_choices))
                | Progress new_or_choices => 
                  match_search (Seq.append new_or_choices more_or_choices) ()));

        val init_st = (init m pat tgt initagenda)
      in Seq.make (match_search (Seq.single init_st)) end;

  (* val match : (V.NSet.T * E.NSet.T * B.NSet.T)
               -> G.T -> G.T -> Match.T Seq.seq  *)
  fun match names pat tgt = let
    in match' (Match.init names pat tgt) pat tgt V.NTab.empty end

  structure Sharing : BASIC_MATCH_SEARCH_SHARING =
  struct
    structure Match = Match.Sharing;
  end;

end;
