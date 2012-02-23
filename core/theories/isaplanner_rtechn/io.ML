

structure RTechn_DotStyle : DOT_STYLE =
struct
  structure Graph = RTechn_Theory.Graph
  fun style_for_ivertex_data x = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
end;

structure RTechn_OutputGraphDot = OutputGraphDot(structure DotStyle = RTechn_DotStyle)
