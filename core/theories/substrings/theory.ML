structure Str_Theory = GraphicalTheory(
  structure OParam = Str_OGraphParam
  structure VComponentData = Str_OVertexComponentData
  structure EComponentData = EmptyElementComponentData
);

structure Str_Controller = Controller(Str_Theory); 

structure Str_DotStyle : DOT_STYLE =
struct
  structure Graph = GHZW_Theory.Graph
  fun style_for_ivertex_data x = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
end;

structure Str_OutputGraphDot = OutputGraphDot(structure DotStyle = Str_DotStyle)
