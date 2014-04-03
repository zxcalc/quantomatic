
local
  structure Theory = Substrings_Theory
  open Theory
in
  (* make a dummy graph that is a single vertex *)
  val _ = File_Io.write_pretty "./foo.svg"
          (OGraphPreLayout.svg_pretty_in_context
            (OGraphPreLayout.svg_pretty_vertex (V.mk "x") 100 150))

  (* create the layout/drawing for the graph g4 with an extra vertex added. *)
  val (_, g) = g4 |> Graph.add_vertex (Graph.NVert ("nar."))

  (* test adding next vlist *)
  val input_vlist = V.NSet.list_of (Graph.get_inputs g)
  val (seen_vs,next_vs) =
      OGraphPreLayout.add_to_next_vlist g input_vlist (V.NSet.empty,V.NSet.empty)
  val (seen_vs,next_vs) =
      OGraphPreLayout.add_to_next_vlist g (V.NSet.list_of next_vs) (seen_vs,V.NSet.empty)

  (* test laying out the graph *)
  (* val _ = Str_Theory.Graph.print g; *)
  val layouts = OGraphPreLayout.layout_graph g
  (* val _ = map OGraphPreLayout.print_layout layouts; *)

  (* one box for each disconnected component *)
  val boxes = map OGraphPreLayout.box_of_layout layouts

  val positions = map (OGraphPreLayout.pos_maps_of_box 50 50) boxes

end

