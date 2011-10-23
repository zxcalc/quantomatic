signature DOT_STYLE =
sig
  structure Graph : BANG_GRAPH
  val style_for_ivertex_data : Graph.OVData.IData.data -> string
end

functor OutputGraphDot(
structure DotStyle : DOT_STYLE
) : OUTPUT_DOT =
struct
  structure Graph = DotStyle.Graph
  type T = string
  type data = Graph.T
  fun node_block graph = let
    val boxtab = VBMap.get_codtab (Graph.get_bboxes graph)
    fun sty nm = case Graph.get_vertex graph nm
                   of (Graph.OVData.IVert data, _) => " " ^ (DotStyle.style_for_ivertex_data data)
                    | (Graph.OVData.EVert, _) => " [style=filled,fillcolor=gray,shape=box,width=0,height=0,fontcolor=white]"
    fun f nm list = (V.pretty_name nm)::
                    (Pretty.str (sty nm))::
                    (Pretty.str ";\n")::
                    list
    fun box_f (box,vset) list = 
      (Pretty.str "subgraph cluster_")::
      (B.pretty_name box)::
      (Pretty.str " {\n")::
      (V.NSet.fold_rev f vset
      (Pretty.str "}\n"::list))
  in B.NTab.fold box_f boxtab (V.NSet.fold_rev f (Graph.get_unboxed graph) [])
  end
  
  fun edge_block graph = let
    fun f nm list = let
      val (_, (s,t)) = Graph.get_edge graph nm
    in (V.pretty_name s)::
       (Pretty.str " -> ")::
       (V.pretty_name t)::
       (Pretty.str " [label=")::
       (E.pretty_name nm)::
       (Pretty.str "];\n")::list
    end
  in E.NSet.fold_rev f (Graph.get_enames graph) []
  end
  
  fun output graph = Pretty.string_of (Pretty.block (
    [Pretty.str "digraph G {\n"] @
    node_block graph @
    edge_block graph @
    [Pretty.str "}\n"]
  ))
end
