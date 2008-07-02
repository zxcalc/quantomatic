local 

(* a simple kind of graph, and some basic manipulations *)
val g = StrUnitGraph.empty;
val (n_a, g) = g |> (StrUnitGraph.add_vertex "a");
val (n_b, g) = g |> (StrUnitGraph.add_vertex "b");
val (e0, g) = g |> (StrUnitGraph.add_edge () n_a n_b);
val (e1, g) = g |> (StrUnitGraph.add_edge () n_a n_b);
val n_c = "n_c";
val g = g |> (StrUnitGraph.rename_vname n_a n_c);
val e3 = "e_c";
val g = g |> (StrUnitGraph.rename_ename e0 e3);

val _ = StrUnitGraph.print g;


val g = StrUnitGraph.empty;
val (n_a, g) = g |> (StrUnitGraph.add_vertex "a");
val (n_b, g) = g |> (StrUnitGraph.add_vertex "b");
val (e0, g) = g |> (StrUnitGraph.add_edge () n_a n_b);
val g = g |> (StrUnitGraph.delete_vertex n_a);
val g = g |> (StrUnitGraph.delete_vertex n_b);

val _ = StrUnitGraph.print g;

(*=== this is a graph of all the types of graphs,nodes and edges in the program *)
val ggg = StrGraph.empty;

val (nodes_and_edges, ggg) = ggg |> (StrGraph.add_named_vertex ("nodes_and_edges") ("sig"));
val (edge, ggg) = ggg |> (StrGraph.add_named_vertex ("edge") ("type"));
val (node, ggg) = ggg |> (StrGraph.add_named_vertex ("node") ("type"));
val (e0, ggg) = ggg |> (StrGraph.add_edge ("requires") nodes_and_edges edge);
val (e0, ggg) = ggg |> (StrGraph.add_edge ("requires") nodes_and_edges node);

val (str_vertexs_and_edges, ggg) = ggg |> (StrGraph.add_named_vertex ("str_vertexs_and_edges") ("sig"));
val (ed, ggg) = ggg |> (StrGraph.add_named_vertex ("ed") ("datatype"));
val (nd, ggg) = ggg |> (StrGraph.add_named_vertex ("nd") ("datatype"));


val (e0, ggg) = ggg |> (StrGraph.add_edge ("specifies") str_vertexs_and_edges ed);
val (e0, ggg) = ggg |> (StrGraph.add_edge ("specifies") str_vertexs_and_edges nd);
val (e0, ggg) = ggg |> (StrGraph.add_edge ("includes") str_vertexs_and_edges nodes_and_edges);

in 
val _ = Pretty.writeln (Pretty.str "Tests passed: basic graphs.")
end;

(* For debugging...

PolyML.exception_trace (fn () => 
  
);

*)
