

structure RG_DotStyle : DOT_STYLE =
struct
  structure Graph = RG_Theory.Graph
  fun style_for_ivertex_data (RG_InternVData.Znd _) = "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
    | style_for_ivertex_data (RG_InternVData.Xnd _) = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
end

structure RG_OutputGraphDot = OutputGraphDot(structure DotStyle = RG_DotStyle)

