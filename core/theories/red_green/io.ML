
(* *)

(* The RG theory, all in once place. *)
structure DB_RG_Theory = GraphicalTheory(
structure OParam = RG_OGraphParam
structure VComponentData = RG_OVertexComponentData
structure EComponentData = EmptyElementComponentData
);
structure RG_Theory : GRAPHICAL_THEORY = DB_RG_Theory;


structure RG_DotStyle : DOT_STYLE =
struct
  structure Graph = RG_Theory.Graph
  fun style_for_ivertex_data (RG_VertexData.Znd _) = "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
    | style_for_ivertex_data (RG_VertexData.Xnd _) = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
end

structure RG_OutputGraphDot = OutputGraphDot(structure DotStyle = RG_DotStyle)

structure RG_Controller = Controller(RG_Theory);

