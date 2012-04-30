local 
(* Select the Theory we are going to work in. See theories/*/theory.ML. Below uses theories/substrings/theory.ML, which provides string data on vertices, no variable data, and prefix-substring as matching. *)
structure Theory = Str_Theory;
open Theory;
in
(* structure Graph = Theory.Graph;
   structure IO = Theory.IO;
   structure Vertex = Graph.OVertex;
   structure Edge = Graph.Edge;
*)

(* Create a new empty graph *)
val g0 = Graph.empty;

(* Write the graph to a file. *)
val _ = g0
  |> IO_Xml.Output.Graph.output
  |> XMLWriter.write_to_file 
       "../theories/substrings/test/graphs/empty_graph.graph";

(* Read the graph from the file *)
val g2 = "../theories/substrings/test/graphs/empty_graph.graph"
  |> XMLReader.read_from_file
  |> IO_Xml.Input.Graph.input;
  
(* We add Boundary vertex to g2, and save it as the file g3. To see what you can do to work with graphs, see the signatures in: 

  core/graph/basic_graph.ML
  core/graph/graph.ML
  core/graph/ograph.ML
  core/graph/bang_graph.ML  
  
We'll create a graph that has 3 vertics, connected in a line "vn1 -> vn2 -> vn3". The first vertex, vn1, is a boundary vertex, the other too are "blah.foo" and "blah.bar". "blah.bar" has an undirected self loop.
*)
(* Let g3 be the empty graph. *)
val g3 = Graph.empty;
(* add vertices *)
val (vn1, g3) = g3 |> Graph.add_vertex (OVData.EVert);
val (vn2, g3) = g3 |> Graph.add_vertex (OVData.IVert ("blah.foo."));
val (vn3, g3) = g3 |> Graph.add_vertex (OVData.IVert ("blah.bar."));
(* add edges *)
val (en1, g3) = g3 |> Graph.add_edge 
  (Graph.Directed, EData.default_data) vn1 vn2;
val (en2, g3) = g3 |> Graph.add_edge 
  (Graph.Directed, EData.default_data) vn2 vn3;
val (en3, g3) = g3 |> Graph.add_edge 
  (Graph.UnDirected, EData.default_data) vn3 vn3;

(* Write the graph g3 to a file. *)
val _ = g3
  |> IO_Xml.Output.Graph.output
  |> XMLWriter.write_to_file 
       "../theories/substrings/test/graphs/g3.graph";

(* Read the graph from the file *)
val g4 = "../theories/substrings/test/graphs/g3.graph"
  |> XMLReader.read_from_file
  |> IO_Xml.Input.Graph.input;

(* Make a simple rule that removes a self loop from "blah.bar.*" if it is connected to "blah.foo.*" by one edge and "blah.foo.*" is connected to one other vertex. *)
(** Make the graph g4, which is the same as g3 but with the self-loop removed. **)
val g4 = g4 |> Graph.delete_edge en3;
(** Make the (un-named, unstored) rule r1. **)
val r1 = Rule.mk (g3,g4);
(** Write the rule to disk **)
val _ = r1
  |> IO_Xml.Output.Rule.output
  |> XMLWriter.write_to_file 
       "../theories/substrings/test/rules/r1.graph";

end; (* *)
