functor SimpleOuterMatchSearch(
    structure InnerMatchSearch : INNER_MATCH_SEARCH
) : OUTER_MATCH_SEARCH =
struct
  structure InnerMatchSearch = InnerMatchSearch
  structure MatchState = InnerMatchSearch.MatchState
  structure G = InnerMatchSearch.G
  
  (* TODO: make this cope with undirected edges *)
  fun match_avoiding_names names pat tgt = let
    val pat = G.normalise pat
    val tgt = G.normalise tgt
    val wv = G.get_wire_vertices pat
    (* find all circles and remove them from wv *)
    val circles = V.NSet.filter 
      (fn v => case V.NSet.tryget_singleton (G.succ_vnames pat v)
                 of SOME v' => V.name_eq(v,v') | NONE => false) wv
    val wv = wv |> V.NSet.subtract circles
    (* find all bare wires and remove them from wv *)
    val (wire_sources, wire_targets) = V.NSet.fold
      (fn v => fn (ss,tt) => (
        if G.is_input_vertex pat v then
          (* use of 'the' because normalised graphs should not contain isolated points *)
          let val v' = (the o V.NSet.tryget_singleton) (G.succ_vnames pat v)
          in if G.is_wire_vertex pat v'
             then (ss|>V.NSet.add v, tt|>V.NSet.add v')
             else (ss,tt)
          end
        else (ss,tt))) wv (V.NSet.empty, V.NSet.empty)
    val wv = wv |> V.NSet.subtract wire_sources |> V.NSet.subtract wire_targets
    val ms = MatchState.empty
      |> MatchState.set_names names
      |> MatchState.set_pat pat
      |> MatchState.set_tgt tgt
      |> MatchState.set_u_circles circles
      |> MatchState.set_u_nodeverts (G.get_node_vertices pat)
      |> MatchState.set_u_wireverts wv
  in InnerMatchSearch.match_concrete ms
  end
  
  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end