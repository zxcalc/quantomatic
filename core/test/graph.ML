(* An instance of Basic Graph: strings at vertices and edges *)
structure DB_StrGraph = 
struct 
  structure BasicStrGraph = 
    BasicGraphFun(
      structure Vertex = StrVertex
      and Edge = StrEdge
      type data = unit
      val empty_data = ()
    );
  structure GraphTools = GraphToolsFun(BasicStrGraph);
  open BasicStrGraph 
  open GraphTools
end;

structure StrGraph : GRAPH = DB_StrGraph;

install_pp (make_pp ["StrGraph", "T"] 
                    (Pretty.pprint o StrGraph.pretty));

(* another basic kind of graph with unit for edge data *)
structure DB_StrUnitGraph = 
struct 
  structure BasicStrUnitGraph = 
    BasicGraphFun(
      structure Vertex = StrVertex
      and Edge = UnitEdge
      type data = unit
      val empty_data = ()
    );
  structure GraphTools = GraphToolsFun(BasicStrUnitGraph);
  open BasicStrUnitGraph 
  open GraphTools
end;

structure StrUnitGraph : GRAPH = DB_StrUnitGraph;

install_pp (make_pp ["StrUnitGraph", "T"] 
                    (Pretty.pprint o StrUnitGraph.pretty));

local 

val add_v = StrUnitGraph.add_vertex;
val add_e = StrUnitGraph.add_edge StrUnitGraph.Edge.default_data;

(* a simple kind of graph, and some basic manipulations *)
val g = StrUnitGraph.empty;
val (n_a, g) = g |> (add_v "a");
val (n_b, g) = g |> (add_v "b");
val (e0, g) = g |> (add_e n_a n_b);
val (e1, g) = g |> (add_e n_a n_b);
val n_c = StrUnitGraph.Vertex.mk "n_c";
val g = g |> (StrUnitGraph.rename_vname n_a n_c);
val e3 = StrUnitGraph.Edge.mk "e_c";
val g = g |> (StrUnitGraph.rename_ename e0 e3);

val _ = StrUnitGraph.print g;


val g = StrUnitGraph.empty;
val (n_a, g) = g |> (add_v "a");
val (n_b, g) = g |> (add_v "b");
val (e0, g) = g |> (add_e n_a n_b);
val g = g |> (StrUnitGraph.delete_vertex n_a);
val g = g |> (StrUnitGraph.delete_vertex n_b);

val _ = StrUnitGraph.print g;

(*=== this is a graph of all the types of graphs,nodes and edges in the program *)
structure V = StrGraph.Vertex;
structure E = StrGraph.Edge;
val ggg = StrGraph.empty;

val (nodes_and_edges, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "nodes_and_edges") ("sig"));
val (edge, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "edge") ("type"));
val (node, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "node") ("type"));
val (e0, ggg) = ggg |> (StrGraph.add_edge ("requires") nodes_and_edges edge);
val (e0, ggg) = ggg |> (StrGraph.add_edge ("requires") nodes_and_edges node);

val (str_vertexs_and_edges, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "str_vertexs_and_edges") ("sig"));
val (ed, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "ed") ("datatype"));
val (nd, ggg) = ggg |> (StrGraph.add_named_vertex (V.mk "nd") ("datatype"));


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
