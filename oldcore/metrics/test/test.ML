local

structure Theory = Substrings_Theory;
open Theory;
in

structure Graph = Theory.Graph;
structure Metrics = MetricsController(structure Graph = Graph);

(* First make sure that Controller is well initialized *)
val mc = Metrics.init;
val names = Metrics.get_metrics_names mc;
val descs = Metrics.get_metrics_descs mc;
val desc = Metrics.get_current_desc mc;
val name = Metrics.get_current_metric_name;

(* Now create a simple graph *)
val g3 = Graph.empty;
val (vn1, g3) = g3 |> Graph.add_vertex (Graph.WVert);
val (vn2, g3) = g3 |> Graph.add_vertex (Graph.NVert ("blah.foo."));
val (vn3, g3) = g3 |> Graph.add_vertex (Graph.NVert ("blah.bar."));
val (en1, g3) = g3 |> Graph.add_edge 
  (Directed, Graph.default_edata) vn1 vn2;
val (en2, g3) = g3 |> Graph.add_edge 
  (Directed, Graph.default_edata) vn2 vn3;
val (en3, g3) = g3 |> Graph.add_edge 
  (Undirected, Graph.default_edata) vn3 vn3;

val value = (Metrics.get_current_compute mc) g3;

end;
