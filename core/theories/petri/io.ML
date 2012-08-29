structure Petri_DotStyle : DOT_STYLE =
struct
  structure Graph = Petri_Theory.Graph
  fun style_for_ivertex_data Petri_Data.BUF0   = "[style=filled,fillcolor=white,fontcolor=black,shape=square]"
    | style_for_ivertex_data Petri_Data.BUF1   = "[style=filled,fillcolor=black,fontcolor=white,shape=square]"
    | style_for_ivertex_data Petri_Data.COPY   = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
    | style_for_ivertex_data Petri_Data.SWITCH = "[style=filled,fillcolor=blue,fontcolor=black,shape=circle]"
end

structure Petri_OutputGraphDot = OutputGraphDot(structure DotStyle = Petri_DotStyle)
