

PolyML.Project.make "../layout/ograph_layout.ML";

local 
  structure Theory = Str_Theory;
  open Theory;
in
  val (_, g) = g4 |> Graph.add_vertex (OVData.IVert ("nar."));

  val input_vlist = V.NSet.list_of (Graph.get_inputs g);
  val (seen_vs,next_vs) = 
      OGraphPreLayout.add_to_next_vlist g input_vlist (V.NSet.empty,V.NSet.empty);
      
  val (seen_vs,next_vs) =
      OGraphPreLayout.add_to_next_vlist g (V.NSet.list_of next_vs) (seen_vs,V.NSet.empty); 
      
  val _ = Str_Theory.Graph.print g;
  val layouts = OGraphPreLayout.layout_graph g;
  val _ = map OGraphPreLayout.print_layout layouts;
  
  val boxes = map OGraphPreLayout.box_of_layout layouts;
end;

