signature TENSOR_DATA =
sig
  structure Graph : BANG_GRAPH
  
  val dimension : int (* all generators are assumed to be of fixed dimension *)
  (* given vertex data and arities, return a tensor *)
  val tensor_for_vertex_data : Graph.Vertex.data -> (int * int) -> Tensor.T
end


functor TensorEval(
  structure TData : TENSOR_DATA
) =
struct
  structure Graph = TData.Graph;
  structure OVertex = Graph.OVertex;
  structure Edge = Graph.Edge;
  
  (* offset tables assign a vertex name to the rightmost tensor index that connects
     to that name. *)
  
  (* shifts all of the offsets >= the offset of vertex "nm" down by 1. if "nm" has
     no space left, it is removed from the offset table. *)
  fun contract_offsets_for_name nm offsets = let
    val current_offset = OVertex.NTab.get offsets nm
    fun dec (nm, offset) (tab, rm) =
      if offset >= current_offset
      then (tab |> OVertex.NTab.doadd (nm, offset-1), rm)
      else (tab |> OVertex.NTab.doadd (nm, offset), rm orelse current_offset-1 = offset)
    val (tab, rm) = OVertex.NTab.fold dec offsets (OVertex.NTab.empty, current_offset = 0)
  in if rm then tab |> OVertex.NTab.delete nm else tab
  end
  
  fun contract_edge graph edge (i_offsets, o_offsets, tensor) = let
    val (_,(src,tgt)) = Graph.get_edge graph edge
    val upper = OVertex.NTab.get o_offsets src
    val lower = OVertex.NTab.get i_offsets tgt
  in (contract_offsets_for_name tgt i_offsets,
      contract_offsets_for_name src o_offsets,
      Tensor.contract (lower,upper) tensor)
  end
  
  fun boundary_list offsets = map fst (sort (fn ((_,o1),(_,o2)) => int_ord (o1, o2)) (OVertex.NTab.list_of offsets))
  
  fun append_vertex graph vert (i_offsets, o_offsets, verts, tensor) = let
    val (data, (in_edges, out_edges)) = Graph.get_vertex graph vert
    val (num_in, num_out) =
      case data of OVertex.IVert _ => (Edge.NSet.cardinality in_edges, Edge.NSet.cardinality out_edges)
                 | OVertex.EVert => (1,1)
    val (current_in, current_out) = (Tensor.lower_index_count tensor, Tensor.upper_index_count tensor)
    val new_tensor = Tensor.product tensor (TData.tensor_for_vertex_data data (num_in, num_out))
    val new_i_offsets = if num_in = 0 then i_offsets
                        else i_offsets |> OVertex.NTab.doadd (vert, (Tensor.lower_index_count tensor) + num_in - 1)
    val new_o_offsets = if num_out = 0 then o_offsets
                        else o_offsets |> OVertex.NTab.doadd (vert, (Tensor.upper_index_count tensor) + num_out - 1)
    val new_verts = verts |> OVertex.NSet.add vert
    val edges_to_contract = OVertex.NSet.fold
                              (Edge.NSet.union_merge o (Graph.enames_between graph vert))
                              new_verts Edge.NSet.empty
    val (new_i_offsets, new_o_offsets, new_tensor) =
      Edge.NSet.fold (contract_edge graph) edges_to_contract (new_i_offsets,new_o_offsets,new_tensor)
  in (new_i_offsets, new_o_offsets, new_verts, new_tensor)
  end
  
  fun tensor_for_graph graph = let
    val (i_offsets,o_offsets,_,t) =
      OVertex.NSet.fold_rev (append_vertex graph)
                            (Graph.get_vnames graph)
                            (OVertex.NTab.empty,OVertex.NTab.empty,OVertex.NSet.empty,
                            Tensor.id TData.dimension 0)
  in (boundary_list i_offsets, boundary_list o_offsets,t)
  end
end


