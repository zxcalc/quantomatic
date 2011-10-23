
  (* opposites *)
  val tgt_edge_has_unmatched_half : T -> E.name -> bool
  val tgt_edge_is_fully_matched : T -> E.name -> bool

  (* *)
  val tgt_edgehalf_is_unmapped : T -> H.name -> bool
  val tgt_vertex_is_matched : T -> V.name -> bool

  (* for incremental matching of edges *)
  (* raise option exception on failure *)
  val match_another_halfedge : H.name * G.EData.data (* pattern hedge *) 
      -> (G.EData.data H.NTab.T (* left over unmatched *)
          * (E.NSet.T (* matches edges *)
             * T)) 
      -> (G.EData.data H.NTab.T (* left over unmatched *)
          * (E.NSet.T (* matches edges *)
             * T)) 
  val match_another_fulledge : E.name * G.EData.data (* pattern edge *)
      -> (G.EData.data H.NTab.T (* left over unmatched *)
          * (E.NSet.T (* matches edges *)
             * T)) 
      -> (G.EData.data H.NTab.T (* left over unmatched *)
          * (E.NSet.T (* matches edges *)
             * T))

  (* match many edges *)
  val multi_edge_match : 
         (G.dir_or_undir * G.EData.data) E.NTab.T  (* pattern edges *)
      -> (G.dir_or_undir * G.EData.data) H.NTab.T (* target half-edges *)
      -> T 
      -> ((G.dir_or_undir * G.EData.data) H.NTab.T (* left over unmatched *)
          * (E.NSet.T (* matches edges *)
             * T)) Seq.seq


  (* FIXME: change to seq/option types, respect failure! don't use exeptions *)
  (* adds given half to a free half of edge *)
(* 
  val add_leaf_ehn2edge_emap 
       : H.name * (G.dir_or_undir * G.EData.data) (* pat *)
      -> E.name * (G.dir_or_undir * G.EData.data) (* tgt *)
      -> T -> T
  (* only adds this half *)
  val add_leaf_ehn2ehn_emap : H.name * (G.dir_or_undir * G.EData.data) 
                              -> H.name * (G.dir_or_undir * G.EData.data) 
                              -> T -> T

  (* adds both halfs as matching *)
  val add_full_leaf_ehn2ehn_emap : H.name * G.EData.data 
                              -> H.name * G.EData.data 
                              -> T -> T option 
*)




(* try to add pe -> te, where pe is an edge doing to a leaf, so it
   will only map half of a tgt edge. *)
fun try_add_undir_emap (peh,ped) (te,td) m = 
    let val emap = get_emap m
        val teh_start = H.mk_start te
    in case EMap.lookup_codf emap teh_start
        of NONE => (* map to start of te *) 
           add_eh_match (peh,ped) (teh_start,td) m
         | SOME _ => 
           let val teh_end = H.mk_end te in
             (case EMap.lookup_codf emap teh_end
               of NONE => (* map to end of te *)
                  add_eh_match (peh,ped) (teh_end,td) m
                (* tgt egde already fully mapped *)
                | SOME _ => NONE) end
    end;


exception emap_exp of string;

fun add_leaf_ehn2edge_emap ph t m = 
    case try_add_leaf_ehn2edge_emap ph t m 
     of NONE => raise emap_exp "add_leaf_edge_emap"
      | SOME x => x;

(* IMPROVE: don't use exceptions? *)
fun add_full_leaf_ehn2ehn_emap (peh,ped) (teh,ted) m = 
    SOME (m |> update_emap (EMap.add peh teh 
                            o EMap.add (H.other peh) 
                                       (H.other teh))
            |> update_mctxt (the o MCtxt.edge_match ped ted))
    handle Option.Option => NONE
         | EMap.add_exp _ => NONE;

fun add_leaf_ehn2ehn_emap (peh,ped) (teh,ted) m = 
    let val emap = get_emap m
    in case EMap.lookup_codf emap teh
        of NONE => (* map to start of te *) 
           (case EMap.lookup_domf emap peh
             of NONE => 
                (case add_eh_match (peh,ped) (teh,ted) m 
                  of NONE => raise emap_exp "add_leaf_ehn2ehn_emap1"
                   | SOME m2 => m2)
              | SOME _ => raise emap_exp "add_leaf_ehn2ehn_emap2")
         | SOME _ => raise emap_exp "add_leaf_ehn2ehn_emap3"
    end;


fun match_another_halfedge
      (peh,ped) (* patten edge name and data *)
      (unmtes2, (* unmatched edges *)
        (mtes2,m2)) (* matched set of enames, match so far *)
    = let (* pull and match pat edge against next target edge. For now we treat all vertices as being symmetric on edge; IMPROVE: should be a parameter of vertex data? *)
        val ((teh1,(tdir_or_undir,ted)),unmtes3) = 
            the (H.NTab.pull_elem unmtes2);
        val te1 = H.get_ename teh1;
        (* match edge data *)
        val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
        (* update match context, and add non-loopy edge map *)
        val m3 = m2 |> the o (add_eh_match (peh, ped) (teh1, ted))
                    |> set_mctxt mctxt2;
        (* te1 is now matched *)
        val mtes3 = if tgt_edge_has_unmatched_half m3 te1 then mtes2 
                    else E.NSet.add te1 mtes2;
      in (unmtes3, (mtes3,m3)) end;

fun match_another_fulledge
      (pe1,ped) (* patten edge name and data *)
      (unmtes2, (* unmatched edges *)
        (mtes2,m2)) (* matched set of enames, match so far *)
    = let (* pull and match pat edge against next target edge  *)
        val ((teh1,ted),unmtes3) = the (H.NTab.pull_elem unmtes2);
        val te1 = H.get_ename teh1;
        (* match edge data *)
        val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
        (* update match context, and add non-loopy edge map *)
        val m3 = m2 |> (the o try_add_emap (pe1, ped) (te1, ted))
                    |> set_mctxt mctxt2;
        (* te1 is now matched *)
        val mtes3 = E.NSet.add te1 mtes2;
      in (unmtes3, (mtes3,m3)) end;


(* assumes that only one of p_from or p_to is a leaf vertex *)
fun multi_edge_match pes tes m = 
    ((tes, (* unmatched tgt edges *) 
      (E.NSet.empty,m)) (* matched enames so far and the match to update *)
         |> E.NTab.fold match_another_fulledge pes
         |> Seq.single)
    handle Option.Option => Seq.empty; (* ran out of target egdes *)


(* tries to match new pattern edge with (first) one in the target set,
raises exception if target set is empty - we handle this give NONE *)
(* given:
     pes: a set of edges between two vertices in the pat, 
     tes: a set of edges between two vertices in the tgt 
   return: 
     option of matches using up all edges in pes, 
     tells us which left-over tes, and which are used. 
     (in future this might be a seq - if edge matching gave back a sequence)
*)
(* IMPROVE: avoid exception handling for failure - use fold_opt *)
(* assumes edge matching order is not relevant/matching makes it
insensitive: I think this is true for AC data matching *)
(* for non-leaf edges/left over leaf edges pes = non-loop tes *)
fun multi_fulledge_match pes tes m = 
    let 
      fun match_more (pe1,ped) (* patten edge name and data *)
                     (unmtes2, (* unmatched tgt edges *)
                      (mtes2, (* matched tgt edges *)
                      m2)) (* match so far *)
        = let (* pull and match pat edge against next target edge  *)
            val ((te1,ted),unmtes3) = the (E.NTab.pull_elem unmtes2);
            (* match edge data *)
            val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
            (* update match context, and add non-loopy edge map *)
            val m3 = m2 |> (the o try_add_emap (pe1, ped) (te1, ted))
                        |> set_mctxt mctxt2;
            (* te1 is now matched *)
            val mtes3 = E.NSet.add te1 mtes2;
          in (unmtes3, (mtes3, m3)) end;
      val (tes_result, m2) = 
          E.NTab.fold match_more pes (tes, (E.NSet.empty, m))
    in Seq.single (tes_result, m2)
    end handle Option.Option => Seq.empty;



(* unused? *)
(* 
fun copy_then_drop_pat_bbox bbn k st = 
    if k = 0 then update_unm_pat (BangGraph.kill_box bbn) st
    else if k = 1 then update_unm_pat (BangGraph.drop_box bbn) st
    else if k > 1 then 
      update_unm_pat (BangGraph.drop_box bbn 
                      o nfold (k - 1) (snd o BangGraph.copy1_box bbn)) st
    else raise bug_exp "copy_then_drop_pat_bbox: negative copy";
*)
