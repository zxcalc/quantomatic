

structure Str_DotStyle : DOT_STYLE =
struct
  structure Graph = Str_Theory.Graph
  fun style_for_ivertex_data x = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
end;

structure Str_OutputGraphDot = OutputGraphDot(structure DotStyle = Str_DotStyle)
