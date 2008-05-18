(* a simple graph, and some basic manipulations *)
val g = QGraph.empty;
val (n_a, g) = g |> (QGraph.add_node (NE.VarNode NE.InputNd));
val (n_b, g) = g |> (QGraph.add_node (NE.VarNode NE.OutputNd));
val (e0, g) = g |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) n_a n_b);
val (e1, g) = g |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) n_a n_b);
val n_c = NName.mk "n_c";
val g = g |> (QGraph.rename_nname n_a n_c);
val e3 = EName.mk "e_c";
val g = g |> (QGraph.rename_ename e0 e3);


QGraph.print g;

(* 3 CNOTs = swap example *)
val g2 = QGraph.empty;
val (in1, g2)  = g2 |> (QGraph.add_named_node (NName.mk "in1") 
					      (NE.VarNode NE.InputNd));
val (in2, g2)  = g2 |> (QGraph.add_named_node (NName.mk "in2")  
					      (NE.VarNode NE.InputNd));

val (out1, g2)  = g2 |> (QGraph.add_named_node (NName.mk "out1")  
					       (NE.VarNode NE.OutputNd));
val (out2, g2)  = g2 |> (QGraph.add_named_node (NName.mk "out2")  
					       (NE.VarNode NE.OutputNd));

val (green1, g2) = g2 |> (QGraph.add_named_node (NName.mk "green1")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green2, g2) = g2 |> (QGraph.add_named_node (NName.mk "green2")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green3, g2) = g2 |> (QGraph.add_named_node (NName.mk "green3")  
						(NE.Node (BasicQuNE.Znd "0")));

val (red1, g2) = g2 |> (QGraph.add_named_node (NName.mk "red1")  
					      (NE.Node (BasicQuNE.Xnd "0")));
val (red2, g2) = g2 |> (QGraph.add_named_node (NName.mk "red2")  
					      (NE.Node (BasicQuNE.Xnd "0")));
val (red3, g2) = g2 |> (QGraph.add_named_node (NName.mk "red3")  
					      (NE.Node (BasicQuNE.Xnd "0")));

val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in1 green1);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in2 red1);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red1 green1);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red1 green2);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red2 green1);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red2 green2);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red3 green2);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red2 green3);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) red3 green3);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) out1 green3);
val (e0, g2) = g2 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) out2 red3);

(* just a swap *)

val g3 = QGraph.empty;
val (in1, g3)  = g3 |> (QGraph.add_named_node (NName.mk "in1") 
					      (NE.VarNode NE.InputNd));
val (in2, g3)  = g3 |> (QGraph.add_named_node (NName.mk "in2")  
					      (NE.VarNode NE.InputNd));

val (out1, g3)  = g3 |> (QGraph.add_named_node (NName.mk "out1")  
					       (NE.VarNode NE.OutputNd));
val (out2, g3)  = g3 |> (QGraph.add_named_node (NName.mk "out2")  
					       (NE.VarNode NE.OutputNd));
val (e0, g3) = g3 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in1 out2);
val (e0, g3) = g3 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in2 out1);


(*  A MC pattern for a CNOT *)
val g4 = QGraph.empty;
val (in1, g4)  = g4 |> (QGraph.add_named_node (NName.mk "in1") 
					      (NE.VarNode NE.InputNd));
val (in2, g4)  = g4 |> (QGraph.add_named_node (NName.mk "in2")  
					      (NE.VarNode NE.InputNd));

val (out1, g4)  = g4 |> (QGraph.add_named_node (NName.mk "out1")  
					       (NE.VarNode NE.OutputNd));
val (out2, g4)  = g4 |> (QGraph.add_named_node (NName.mk "out2")  
					       (NE.VarNode NE.OutputNd));

val (green1, g4) = g4 |> (QGraph.add_named_node (NName.mk "green1")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green2, g4) = g4 |> (QGraph.add_named_node (NName.mk "green2")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green3, g4) = g4 |> (QGraph.add_named_node (NName.mk "green3")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green4, g4) = g4 |> (QGraph.add_named_node (NName.mk "green4")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green5, g4) = g4 |> (QGraph.add_named_node (NName.mk "green5")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green6, g4) = g4 |> (QGraph.add_named_node (NName.mk "green6")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green7, g4) = g4 |> (QGraph.add_named_node (NName.mk "green7")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green8, g4) = g4 |> (QGraph.add_named_node (NName.mk "green8")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green9, g4) = g4 |> (QGraph.add_named_node (NName.mk "green9")  
						(NE.Node (BasicQuNE.Znd "0")));
val (green10, g4) = g4 |> (QGraph.add_named_node (NName.mk "green10")  
						(NE.Node (BasicQuNE.Znd "0")));

val (H1, g4) = g4 |> (QGraph.add_named_node (NName.mk "H1")  
						(NE.Node (BasicQuNE.Hnd)));
val (H2, g4) = g4 |> (QGraph.add_named_node (NName.mk "H2")  
						(NE.Node (BasicQuNE.Hnd)));
val (H3, g4) = g4 |> (QGraph.add_named_node (NName.mk "H3")  
						(NE.Node (BasicQuNE.Hnd)));

val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in1 green2);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) in2 green3);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green3 green1);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green2 out1);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green2 H1);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) H1 green5);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green5 green4);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green5 green6);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green3 H2);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) H2 green6);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green6 green7);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green7 green8);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green7 H3);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) H3 green9);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green9 green10);
val (e0, g4) = g4 |> (QGraph.add_edge (NE.OneEdge BasicQuNE.Edge) green9 out2);





(* For debugging...

PolyML.exception_trace (fn () => 
  
);

*)