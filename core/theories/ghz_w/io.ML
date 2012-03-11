structure GHZW_DotStyle : DOT_STYLE =
struct
  structure Graph = GHZW_Theory.Graph
  fun style_for_ivertex_data GHZW_VertexData.GHZ = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
    | style_for_ivertex_data GHZW_VertexData.W = "[style=filled,fillcolor=black,fontcolor=white,shape=circle]"
    | style_for_ivertex_data GHZW_VertexData.TICK = "[style=filled,fillcolor=red,fontcolor=white,shape=diamond]"
    | style_for_ivertex_data GHZW_VertexData.ZERO = "[style=filled,fillcolor=blue,fontcolor=black,shape=square]"
end

structure GHZW_OutputGraphDot = OutputGraphDot(structure DotStyle = GHZW_DotStyle)
