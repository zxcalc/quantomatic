functor SimpleInnerMatchSearch(
    structure G : OGRAPH
) : INNER_MATCH_SEARCH
= struct
  structure G = G
  structure MatchState = OGraphMatchState(structure G = G)
  
  
  exception InnerMatchError of string
  
  
  (*****************************************)
  (************ CIRCLE MATCHING ************)
  (*****************************************)
  
  (* FIXME: circle matching ignores edge data *)
  
  fun u_circles_in_tgt ms = let
    val tgt = MatchState.get_tgt ms
  in fold (fn (s,t,_,_) =>
       if G.is_wire_vertex tgt s andalso V.name_eq (s,t) andalso (* is a circle *)
       not (VInjEndo.dom_contains (MatchState.get_vmap ms) s)    (* not matched *)
       then V.NSet.add s else I) (G.get_wire_list tgt) V.NSet.empty
  end
  
  fun match_circles ms = let
    fun m_circ cp (SOME (ms', cts)) =
         (case V.NSet.get_min cts
            of SOME ct => SOME (ms' |> MatchState.add_circle_to_match (cp,ct)
                                    |> MatchState.update_u_circles (V.NSet.delete cp),
                                cts |> V.NSet.delete ct)
             | NONE => NONE)
      | m_circ _ NONE = NONE
  in case V.NSet.fold m_circ (MatchState.get_u_circles ms) (SOME (ms, u_circles_in_tgt ms))
       of SOME (ms',_) => SOME ms' | NONE => NONE
  end
  
  
  (*****************************************)
  (************* MAIN MATCHING *************)
  (*****************************************)
  
  fun match_main ms = let
    
    (* remove pnv from u_nodeverts, and schedule at all sites where it matches *)
    fun match_and_schedule_new nv = let
      val ms' = ms |> MatchState.update_u_nodeverts  (V.NSet.delete nv)
                   |> MatchState.update_p_nodeverts  (V.NSet.add nv)
                   |> MatchState.update_ps_nodeverts (V.NSet.add nv)
      val unm_nv_tgt = V.NSet.subtract
        (G.get_node_vertices (MatchState.get_tgt ms'))
        (VInjEndo.get_codset (MatchState.get_vmap ms'))
      val nv_data = G.get_vertex_data (MatchState.get_pat ms') nv
      fun m_nodevert tnv =
        case G.VData.match_data (nv_data, G.get_vertex_data (MatchState.get_tgt ms') tnv)
                                (MatchState.get_match_subst ms)
          of SOME subst => match_main (ms' |> MatchState.set_match_subst subst
                                           |> MatchState.add_vertex_to_match (nv, tnv))
           | NONE => Seq.empty
    in V.NSet.fold (Seq.append o m_nodevert) unm_nv_tgt Seq.empty
    end
    
    (* continue matching from the given partially-matched node-vertex *)
    (* STUB: just unschedule given vertex *)
    fun continue_matching_from nv =
      match_main (ms |> MatchState.update_ps_nodeverts (V.NSet.delete nv))
    
  in case V.NSet.get_min (MatchState.get_ps_nodeverts ms)
       of SOME ps => continue_matching_from ps
        | NONE => (
            case V.NSet.get_min (MatchState.get_u_nodeverts ms)
              of SOME un => match_and_schedule_new un
               | NONE => Seq.single ms) (* nothing left to do *)
  end
  
  (* match all circles, then enter main matching routine *)
  fun match_concrete ms = case match_circles ms
    of SOME ms' => match_main ms'
     | NONE     => Seq.empty (* return nothing if circles can't be matched *)
  
  
  
  (*****************************************)
  (********** BARE WIRE MATCHING ***********)
  (*****************************************)
  
  (* grow the single, given edge into:
      - a circle consisting of two edges for circles,
      - otherwise a chain of three edges, return the middle edge *)
  fun grow_edge g e = let
    val (dd, (s,t)) = G.get_edge g e
  in if V.name_eq (s,t) then
      let
        val (b1,g) = g |> G.add_vertex G.OVData.EVert
        val g = g |> G.doadd_edge dd s b1
        val (mid_e, g) = g |> G.add_edge dd b1 t
        val g = g |> G.delete_edge e
      in (mid_e,g)
      end
    else
      let 
        val (b1,g) = g |> G.add_vertex G.OVData.EVert
        val (b2,g) = g |> G.add_vertex G.OVData.EVert
        val g = g |> G.doadd_edge dd s b1
        val (mid_e,g) = g |> G.add_edge dd b1 b2
        val g = g |> G.doadd_edge dd b2 t
        val g = g |> G.delete_edge e
      in (mid_e,g)
      end
  end
  
  
  (* match the given set of bare wires, expanding wire-vertices where necessary *)
  fun match_bare_wires ms = 
    if E.NSet.is_empty (MatchState.get_u_bare_wires ms) then Seq.single ms
    else let
      val pat = MatchState.get_pat ms
      val tgt = MatchState.get_tgt ms
      (* for a normalised graph, this will always be the source end *)
      val bw = (the o E.NSet.get_min) (MatchState.get_u_bare_wires ms)
      val ((bw_dir, bw_data), (bw_s, bw_t)) = G.get_edge pat bw
      
      (* candidate edges for matching are:
           - the unique edge in a circle, bare wire, or boundary wire
           - the middle edge in an interior wire *)
      fun is_candidate_for_bw e = let
        val ((d,_),(s,t)) = G.get_edge tgt e
      in not (EInjEndo.cod_contains (MatchState.get_emap ms) e) andalso
         d = bw_dir andalso
         (G.is_boundary tgt s orelse G.is_boundary tgt t orelse    (* bare and boundary wires *)
          (G.is_wire_vertex tgt s andalso G.is_wire_vertex tgt t)) (* circles + middle edges *)
      end
      
      (* return all the edges of the right kind with matching dir_or_undir *)
      val edges = E.NSet.filter (is_candidate_for_bw) (G.get_enames tgt)
      fun match_bw e =
        case G.EData.match_data (bw_data, G.get_edge_data tgt e) (MatchState.get_match_subst ms)
          of SOME _ => let
               val (mid_e, tgt') = grow_edge tgt e
               val new_subst = case G.EData.match_data (bw_data, G.get_edge_data tgt' mid_e) (MatchState.get_match_subst ms)
                                 of SOME sub => sub | NONE => raise InnerMatchError "Newly-grown edge should match"
               val ms' = ms
                 |> MatchState.set_tgt tgt' (* save the new, edge-homeo equivalent target *)
                 |> MatchState.set_match_subst new_subst
                 |> MatchState.add_edge_to_match (bw,mid_e)
                 |> MatchState.update_u_bare_wires (E.NSet.delete bw)
             in match_bare_wires ms'
             end
           | NONE => Seq.empty
    in E.NSet.fold (Seq.append o match_bw) edges Seq.empty
    end
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
end