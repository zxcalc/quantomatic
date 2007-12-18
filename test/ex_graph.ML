(* a simple kind of graph, and some basic manipulations *)


val g = StrGraph.empty;
val (n_a, g) = g |> (StrGraph.add_node (StrNE.Nd "a"));
val (n_b, g) = g |> (StrGraph.add_node (StrNE.Nd "b"));
val (e0, g) = g |> (StrGraph.add_edge (StrNE.Ed "e0") n_a n_b);
val (e1, g) = g |> (StrGraph.add_edge (StrNE.Ed "e1") n_a n_b);
val n_c = NName.mk "n_c";
val g = g |> (StrGraph.rename_nname n_a n_c);
val e3 = EName.mk "e_c";
val g = g |> (StrGraph.rename_ename e0 e3);

StrGraph.print g;


(* For debugging...

PolyML.exception_trace (fn () => 
  
);

*)

(*=== this is a graph of all the types of graphs,nodes and edges in the program *)
val ggg = StrGraph.empty;

val (nodes_and_edges, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "nodes_and_edges") (StrNE.Nd "sig"));
val (edge, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "edge") (StrNE.Nd "type"));
val (node, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "node") (StrNE.Nd "type"));
val (e0, ggg) = ggg |> (StrGraph.add_edge (StrNE.Ed "requires") nodes_and_edges edge);
val (e0, ggg) = ggg |> (StrGraph.add_edge (StrNE.Ed "requires") nodes_and_edges node);

val (str_nodes_and_edges, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "str_nodes_and_edges") (StrNE.Nd "sig"));
val (ed, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "ed") (StrNE.Nd "datatype"));
val (nd, ggg) = ggg |> (StrGraph.add_named_node (NName.mk "nd") (StrNE.Nd "datatype"));


val (e0, ggg) = ggg |> (StrGraph.add_edge (StrNE.Ed "specifies") str_nodes_and_edges ed);
val (e0, ggg) = ggg |> (StrGraph.add_edge (StrNE.Ed "specifies") str_nodes_and_edges nd);
val (e0, ggg) = ggg |> (StrGraph.add_edge (StrNE.Ed "includes") str_nodes_and_edges nodes_and_edges);


