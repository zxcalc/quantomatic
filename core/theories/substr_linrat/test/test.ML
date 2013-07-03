(*------------------------------------------------------------------------------
This file demonstrates some handy bits of ML for playing with graphs in Quantomatic.
------------------------------------------------------------------------------*)

local
(* Select the Theory we are going to work in. See theories/*/theory.ML. Below uses theories/substr_linrat/theory.ML, which provides string data on vertices, no variable data, and prefix-substring as matching. *)
structure Theory = Substr_Linrat_Theory
structure IO = Substr_Linrat_GraphicalTheoryIO
open Theory
in

(* Create a new empty graph *)
val g0 = Graph.empty

(* Write the graph to a file. *)
val _ = g0
  |> IO.OutputGraphJSON.output
  |> File_Io.write_json "graphs/empty_graph.graph"

(* Read the graph from the file *)
val g2 = "graphs/empty_graph.graph"
  |> File_Io.read_json
  |> IO.InputGraphJSON.input

(* We add Boundary vertex to g2, and save it as the file g3. To see what you can do to work with graphs, see the signatures in:

  core/graph/basic_graph.ML
  core/graph/graph.ML
  core/graph/ograph.ML
  core/graph/bang_graph.ML

We'll create a graph that has 3 vertics, connected in a line "vn1 -> vn2 -> vn3". The first vertex, vn1, is a boundary vertex, the other too are "blah.foo" and "blah.bar". "blah.bar" has an undirected self loop.
*)
(* Let g3 be the empty graph. *)
val g3 = Graph.empty
(* add vertices *)
val (vn1, g3) = g3 |> Graph.add_vertex (Graph.WVert)
val (vn2, g3) = g3 |> Graph.add_vertex (Graph.NVert
("blah.foo.", LinratAngleExpr.zero))

val (vn3, g3) = g3 |> Graph.add_vertex (Graph.NVert
("blah.bar.", LinratAngleExpr.zero))
(* add edges *)
val (en1, g3) = g3 |> Graph.add_edge
  (Directed, ()) vn1 vn2
val (en2, g3) = g3 |> Graph.add_edge
  (Directed, ()) vn2 vn3
val (en3, g3) = g3 |> Graph.add_edge
  (Undirected, ()) vn3 vn3

(* Write the graph g3 to a file. *)
val _ = g3
  |> IO.OutputGraphJSON.output
  |> File_Io.write_json "graphs/g3.graph"

(* Read the graph from the file *)
val g4 = "graphs/g3.graph"
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
  |> File_Io.write_json "rules/r1.graph"

end; (* *)
