signature TENSOR_DATA =
sig
  structure Graph : BANG_GRAPH
  structure Tensor : TENSOR
  
  val dimension : int (* all generators are assumed to be of fixed dimension *)
  (* given vertex data and arities, return a tensor *)
  val tensor_for_vertex_data : Graph.VData.data -> (int * int) -> Tensor.T
end


functor TensorEquiv(
  structure TData : TENSOR_DATA
) : GRAPH_EQUIV =
struct
  type T = TData.Tensor.T option
  structure Graph = TData.Graph
  
  (* offset tables assign a vertex name to the rightmost tensor index that connects
     to that name. *)
  
  (* shifts all of the offsets >= the offset of vertex "nm" down by 1. if "nm" has
     no space left, it is removed from the offset table. *)
  fun contract_offsets_for_name nm offsets = let
    val current_offset = V.NTab.get offsets nm
    fun dec (nm, offset) (tab, rm) =
      if offset >= current_offset
      then (tab |> V.NTab.doadd (nm, offset-1), rm)
      else (tab |> V.NTab.doadd (nm, offset), rm orelse current_offset-1 = offset)
    val (tab, rm) = V.NTab.fold dec offsets (V.NTab.empty, current_offset = 0)
  in if rm then tab |> V.NTab.delete nm else tab
  end
  
  fun contract_edge graph edge (i_offsets, o_offsets, tensor) = let
    val (_,(src,tgt)) = TData.Graph.get_edge graph edge
    val upper = V.NTab.get o_offsets src
    val lower = V.NTab.get i_offsets tgt
  in (contract_offsets_for_name tgt i_offsets,
      contract_offsets_for_name src o_offsets,
      TData.Tensor.contract (lower,upper) tensor)
  end
  
  fun boundary_list offsets = map fst (sort (fn ((_,o1),(_,o2)) => int_ord (o1, o2)) (V.NTab.list_of offsets))
  
  fun append_vertex graph vert (i_offsets, o_offsets, verts, tensor) = let
    val (data, (in_edges, out_edges)) = TData.Graph.get_vertex graph vert
    val (num_in, num_out) =
      case data of TData.Graph.OVData.NVert _ => (E.NSet.cardinality in_edges, E.NSet.cardinality out_edges)
                 | TData.Graph.OVData.WVert => (1,1)
    val (current_in, current_out) = (TData.Tensor.lower_index_count tensor, TData.Tensor.upper_index_count tensor)
    val new_tensor = TData.Tensor.product tensor (TData.tensor_for_vertex_data data (num_in, num_out))
    val new_i_offsets = if num_in = 0 then i_offsets
                        else i_offsets |> V.NTab.doadd (vert, (TData.Tensor.lower_index_count tensor) + num_in - 1)
    val new_o_offsets = if num_out = 0 then o_offsets
                        else o_offsets |> V.NTab.doadd (vert, (TData.Tensor.upper_index_count tensor) + num_out - 1)
    val new_verts = verts |> V.NSet.add vert
    val edges_to_contract = V.NSet.fold
                              (E.NSet.union_merge o (TData.Graph.enames_between graph vert))
                              new_verts E.NSet.empty
    val (new_i_offsets, new_o_offsets, new_tensor) =
      E.NSet.fold (contract_edge graph) edges_to_contract (new_i_offsets,new_o_offsets,new_tensor)
  in (new_i_offsets, new_o_offsets, new_verts, new_tensor)
  end
  
  fun tensor_for_graph graph = let
    val (i_offsets,o_offsets,_,t) =
      V.NSet.fold_rev (append_vertex graph)
                      (TData.Graph.get_vnames graph)
                      (V.NTab.empty,V.NTab.empty,V.NSet.empty,
                       TData.Tensor.id TData.dimension 0)
  in (boundary_list i_offsets, boundary_list o_offsets,t)
  end
  
  fun compute_equiv_data gr = let
    val (ins,outs,tens) = tensor_for_graph gr
    val ((scalar,perm_outs,perm_ins),tens') = TData.Tensor.normalise tens
    val norm_ins = map (fn n => V.mk ("i_"^(Int.toString n))) perm_ins
    val norm_outs = map (fn n => V.mk ("o_"^(Int.toString n))) perm_outs
    val gr' = fold2 Graph.rename_vname ins norm_ins (fold2 Graph.rename_vname outs norm_outs gr)
  in
    (gr', SOME tens)
  end
  
  fun eq (NONE, _) = false
    | eq (_, NONE) = false
    | eq (SOME t1, SOME t2) = TData.Tensor.eq(t1,t2)
end


