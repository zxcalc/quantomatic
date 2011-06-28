structure RGB_DotStyle : DOT_STYLE =
struct
  structure Graph = RGB_Theory.Graph
  fun style_for_ivertex_data RGB_VertexData.Red   = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
    | style_for_ivertex_data RGB_VertexData.Green = "[style=filled,fillcolor=green,fontcolor=white,shape=circle]"
    | style_for_ivertex_data RGB_VertexData.Blue  = "[style=filled,fillcolor=blue,fontcolor=white,shape=circle]"
end

structure RGB_OutputGraphDot = OutputGraphDot(structure DotStyle = RGB_DotStyle)
