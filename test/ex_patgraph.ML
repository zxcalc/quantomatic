(* a simple kind of graph, and some basic manipulations *)

val g = PatStrGraph.empty;
val (n_a, g) = g |> (PatStrGraph.add_node (PatStrNE.Node (StrNE.Nd "a")));
val (n_b, g) = g |> (PatStrGraph.add_node (PatStrNE.Node (StrNE.Nd "b")));
val (e0, g) = g |> (PatStrGraph.add_edge (PatStrNE.OneEdge (StrNE.Ed "e0")) 
                                      n_a n_b);
val (e1, g) = g |> (PatStrGraph.add_edge (PatStrNE.OneEdge (StrNE.Ed "e1")) 
                                      n_a n_b);
val n_c = NName.mk "n_c";
val g = g |> (PatStrGraph.rename_nname n_a n_c);
val e3 = EName.mk "e_c";
val g = g |> (PatStrGraph.rename_ename e0 e3);

PatStrGraph.print g;


(* For debugging...

PolyML.exception_trace (fn () => 
  
);

*)