functor SimpleInnerMatchSearch(
    structure G : OGRAPH
) : INNER_MATCH_SEARCH
= struct
  structure G = G
  structure MatchState = OGraphMatchState(structure G = G)
    
  (* do as much concrete matching as possible and return a lazy list of updated match states *)
  fun match_concrete m = Seq.empty
  
  
  (* candidate edges for matching are:
           - the unique edge in a circle, bare wire, or boundary wire
           - the middle edge in an interior wire *)
  fun is_candidate_for_bw tgt dir_or_undir e = let
    val ((d,_),(s,t)) = G.get_edge tgt e
  in d = dir_or_undir andalso
     (G.is_boundary tgt s orelse G.is_boundary tgt t orelse   (* bare and boundary wires *)
      (G.is_wire_vertex tgt s andalso G.is_wire_vertex tgt t)) (* circles + middle edges *)
  end
  
  
  (* grow the single, given edge into a chain of three edges, return the middle edge,
     which will be added to the matching *)
  fun grow_edge g e = let
    val (b1,g) = g |> G.add_vertex G.OVData.EVert
    val (b2,g) = g |> G.add_vertex G.OVData.EVert
    val (dd, (s,t)) = G.get_edge g e
    val g = g |> G.doadd_edge dd s b1
    val (mid_e,g) = g |> G.add_edge dd b1 b2
    val g = g |> G.doadd_edge dd b2 t
    val g = g |> G.delete_edge e
  in (mid_e,g)
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
      (* return all the edges of the right kind with matching dir_or_undir *)
      val edges = E.NSet.filter (is_candidate_for_bw tgt bw_dir) (G.get_enames tgt)
      fun match_bw e =
        case G.EData.match_data (bw_data, G.get_edge_data tgt e) (MatchState.get_match_subst ms)
          of SOME new_subst => let
               val (middle_e, tgt') = grow_edge tgt e
               val ms' = ms
                 |> MatchState.set_tgt tgt' (* save the new, edge-homeo equivalent target *)
                 |> MatchState.set_match_subst new_subst
                 |> MatchState.add_edge_to_match (bw,middle_e)
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