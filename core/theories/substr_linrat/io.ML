

structure SUBSTR_LINRAT_DotStyle : DOT_STYLE =
struct
  structure Graph = SUBSTR_LINRAT_Theory.Graph
  fun style_for_ivertex_data x = "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
end;

structure SUBSTR_LINRAT_OutputGraphDot = OutputGraphDot(structure DotStyle = SUBSTR_LINRAT_DotStyle)

