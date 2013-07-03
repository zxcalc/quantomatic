local 
(* Select the Theory we are going to work in. See theories/*/theory.ML. Below uses theories/substrings/theory.ML, which provides string data on vertices, no variable data, and prefix-substring as matching. *)
structure Theory = Substrings_Theory
structure IO = Substrings_GraphicalTheoryIO
open Theory
in

(* Create a new empty graph *)
val g0 = Graph.empty

(* Write the graph to a file. *)
val _ = g0
  |> IO.OutputGraphJSON.output
  |> File_Io.write_json "../theories/substrings/test/graphs/empty_graph.graph"

(* Read the graph from the file *)
val g2 = "../theories/substrings/test/graphs/empty_graph.graph"
  |> File_Io.read_json
  |> IO.InputGraphJSON.input
  
(* We add Boundary vertex to g2, and save it as the file g3. To see what you can do to work with graphs, see the signatures in:

  core/graph/ograph.sig.ML
  core/graph/bang_graph.sig.ML

We'll create a graph that has 3 vertices, connected in a line "vn1 -> vn2 -> vn3". The first vertex, vn1, is a boundary vertex, the other too are "blah.foo" and "blah.bar". "blah.bar" has an undirected self loop.
*)
(* Let g3 be the empty graph. *)
val g3 = Graph.empty
(* add vertices *)
val (vn1, g3) = g3 |> Graph.add_vertex (Graph.WVert)
val (vn2, g3) = g3 |> Graph.add_vertex (Graph.NVert ("blah.foo."))
val (vn3, g3) = g3 |> Graph.add_vertex (Graph.NVert ("blah.bar."))
(* add edges *)
val (en1, g3) = g3 |> Graph.add_edge
  (Directed, Graph.default_edata) vn1 vn2
val (en2, g3) = g3 |> Graph.add_edge
  (Directed, Graph.default_edata) vn2 vn3
val (en3, g3) = g3 |> Graph.add_edge
  (Undirected, Graph.default_edata) vn3 vn3

(* Write the graph g3 to a file. *)
val _ = g3
  |> IO.OutputGraphJSON.output
  |> File_Io.write_json "../theories/substrings/test/graphs/g3.graph"

(* Read the graph from the file *)
val g4 = "../theories/substrings/test/graphs/g3.graph"
  |> File_Io.read_json
  |> IO.InputGraphJSON.input

(* Make a simple rule that removes a self loop from "blah.bar.*" if it is connected to "blah.foo.*" by one edge and "blah.foo.*" is connected to one other vertex. *)
(** Make the graph g4, which is the same as g3 but with the self-loop removed. **)
val g4 = g4 |> Graph.delete_edge en3
(** Make the (un-named, unstored) rule r1. **)
val r1 = Rule.mk (g3,g4)
(** Write the rule to disk **)
val _ = r1
  |> IO.OutputRuleJSON.output
  |> File_Io.write_json "../theories/substrings/test/rules/r1.graph"

end; (* *)
