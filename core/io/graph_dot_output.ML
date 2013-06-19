signature OUTPUT_DOT = OUTPUT where type T = string;

functor OutputSStrNameDot(N: SSTR_NAME) : OUTPUT_DOT
= struct
  type T = string;
  type data = N.name;
  fun output s = N.string_of_name s;
end;

signature DOT_STYLE =
sig
  type nvdata;
  val style_for_ivertex_data : nvdata -> string
end

functor OutputGraphDot(
  structure Graph : BANG_GRAPH
  structure DotStyle : DOT_STYLE
  sharing type DotStyle.nvdata = Graph.nvdata
) : OUTPUT_DOT =
struct
  structure Graph = Graph;
  type T = string;
  type data = Graph.T;
  fun node_block graph = let
    fun sty v = case Graph.get_vertex_data graph v
                   of Graph.NVert data => " " ^ (DotStyle.style_for_ivertex_data data)
                    | Graph.WVert => " [style=filled,fillcolor=gray,shape=box,width=0,height=0,fontcolor=white]"
    fun f v lst = (V.pretty_name v)::
                    (Pretty.str (sty v))::
                    (Pretty.str ";\n")::
                    lst
    fun box_f box lst = 
      (Pretty.str "subgraph cluster_")::
      (B.pretty_name box)::
      (Pretty.str " {\n")::
      (V.NSet.fold_rev f (Graph.get_vertices_in_bbox graph box)
      (Pretty.str "}\n"::lst))
  in B.NSet.fold box_f (Graph.get_bboxes graph) (V.NSet.fold_rev f (Graph.get_unbboxed graph) [])
  end;
  
  fun edge_block graph = let
    fun f e lst = let
      val (s,t) = (Graph.get_edge_source graph e, Graph.get_edge_target graph e)
    in (V.pretty_name s)::
       (Pretty.str " -> ")::
       (V.pretty_name t)::
       (Pretty.str " [label=")::
       (E.pretty_name e)::
       (Pretty.str "];\n")::lst
    end
  in E.NSet.fold_rev f (Graph.get_edges graph) []
  end;
  
  fun output graph = Pretty.string_of (Pretty.block (
    [Pretty.str "digraph G {\n"] @
    node_block graph @
    edge_block graph @
    [Pretty.str "}\n"]
  ));
end
