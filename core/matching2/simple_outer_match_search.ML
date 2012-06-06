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
    val wires = G.get_wire_list pat
    fun delete_wvs_in_wire (s,t,wv,_) set =
      set |> V.NSet.union_merge wv
          |> (if G.is_wire_vertex pat s then V.NSet.add s else I)
          |> (if G.is_wire_vertex pat t then V.NSet.add t else I)
    
    val circles = filter (fn (s,t,_,_) => V.name_eq (s,t) andalso G.is_wire_vertex pat s) wires
    val bare_wires = filter (fn (s,t,_,_) => G.is_wire_vertex pat s andalso G.is_wire_vertex pat t) wires
    
    val wvs = fold delete_wvs_in_wire (circles @ bare_wires) (G.get_wire_vertices pat)
    val circle_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    val bare_wire_wvs = fold (fn (s,_,_,_) => fn set => set |> V.NSet.add s) circles V.NSet.empty
    
    val ms = MatchState.empty
      |> MatchState.set_names names
      |> MatchState.set_pat pat
      |> MatchState.set_tgt tgt
      |> MatchState.set_u_circles circle_wvs
      |> MatchState.set_u_nodeverts (G.get_node_vertices pat)
      |> MatchState.set_u_wireverts wvs
  in Seq.maps InnerMatchSearch.match_bare_wires (InnerMatchSearch.match_concrete ms)
  end
  
  val match = match_avoiding_names (V.NSet.empty, E.NSet.empty)
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure InnerMatchSearch = InnerMatchSearch.Sharing
    structure MatchState = MatchState.Sharing
  end
end