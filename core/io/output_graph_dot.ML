signature DOT_STYLE =
sig
  structure Graph : BANG_GRAPH
  val style_for_ivertex_data : Graph.OVertex.IData.data -> string
end

functor OutputGraphDot(
structure DotStyle : DOT_STYLE
) : OUTPUT_DOT =
struct
  structure Graph = DotStyle.Graph
  type T = string
  type data = Graph.T
  fun node_block graph = let
    fun sty nm = case Graph.get_vertex graph nm
                   of (Graph.OVertex.IVert data, _) => " " ^ (DotStyle.style_for_ivertex_data data)
                    | (Graph.OVertex.EVert, _) => " [style=filled,fillcolor=gray,shape=box,width=0,height=0,fontcolor=white]"
    fun f nm list = (Graph.Vertex.pretty_name nm)::
                    (Pretty.str (sty nm))::
                    (Pretty.str ";\n")::
                    list
  in Graph.Vertex.NSet.fold_rev f (Graph.get_vnames graph) []
  end
  
  fun edge_block graph = let
    fun f nm list = let
      val (_, (s,t)) = Graph.get_edge graph nm
    in (Graph.Vertex.pretty_name s)::
       (Pretty.str " -> ")::
       (Graph.Vertex.pretty_name t)::
       (Pretty.str " [label=")::
       (Graph.Edge.pretty_name nm)::
       (Pretty.str "];\n")::list
    end
  in Graph.Edge.NSet.fold_rev f (Graph.get_enames graph) []
  end
  
  fun output graph = Pretty.string_of (Pretty.block (
    [Pretty.str "digraph G {\n"] @
    node_block graph @
    edge_block graph @
    [Pretty.str "}\n"]
  ))
end
