(* a simple graph, and some basic manipulations *)


val g = QGraph.empty;
val (red, g) = g |> (QGraph.add_named_node (NName.mk "r") (Red "0"));
val (green, g) = g |> (QGraph.add_named_node (NName.mk "g") (Green "0"));
val (vnd1, g) = g |> (QGraph.add_named_node (NName.mk "x") OutVar);
val (vnd2, g) = g |> (QGraph.add_named_node (NName.mk "y") OutVar);
val (e0, g) = g |> (QGraph.add_named_edge (EName.mk "e1") Edge red green);
val (e1, g) = g |> (QGraph.add_named_edge (EName.mk "e2") Edge green vnd1);
val (e2, g) = g |> (QGraph.add_named_edge (EName.mk "e3") Edge green vnd2);

val lhs = g;

val g = QGraph.empty;
val g = g |> (QGraph.new_named_node red (Red "0"));
val (red2, g) = g |> (QGraph.add_named_node red (Red "0"));
val g = g |> (QGraph.new_named_node vnd1 OutVar);
val g = g |> (QGraph.new_named_node vnd2 OutVar);
val g = g |> (QGraph.new_named_edge e1 Edge red vnd1);
val g = g |> (QGraph.new_named_edge e2 Edge red2 vnd2);

val rhs = g;


QGraph.print lhs;
QGraph.print rhs;