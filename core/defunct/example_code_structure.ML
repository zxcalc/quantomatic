(* Hack around Limits of ML modules: Multiple inheritence by shared
ancenster substructure, shared inherrited data, and struct linear
signature inclusion. *)

(* basic graphs *)
signature BASIC_GRAPH 
= sig
type nd
type e
type T
val empty : T
val add_nd : nd -> T -> T
end;

(* graphs with nice API and tools *)
signature GRAPH 
= sig
include BASIC_GRAPH
val addnd_list : nd list -> T -> T
end;


(* graph tools functor gives back extra graph functions without
   changing the data being used. *)
signature GRAPH_TOOLS
= sig
structure G : BASIC_GRAPH (* Graph datatypes are hidden in this substructure; thus sharable and we can use open to express structure intersection *)
val addnd_list : G.nd list -> G.T -> G.T
end;

functor GraphToolsFun(B : BASIC_GRAPH)
: GRAPH_TOOLS
= struct 
structure G = B;
open G;
val addnd_list = fold add_nd;
end;

(* signature for a possible data-changing extension of graphs *)
signature PATGRAPH = 
sig
include GRAPH
val match : T -> T -> bool
end;

functor PatFun(Graph : GRAPH)
: PATGRAPH
= struct
  (* write Basic Graph stuff in separate defined signature *)
  structure BasicPatGraph = 
  struct (* extend data for pat graphs and provide internal tools as needed *)
    datatype T = PGraph of Graph.T
    datatype e = unit
    datatype nd = PNd of Graph.nd
    val empty = PGraph Graph.empty;
    fun add_nd (PNd n) (PGraph g) = PGraph (Graph.add_nd n g);
  end;
  (* create standard graph tools *)
  structure PatGraph = GraphToolsFun(BasicPatGraph);
  open PatGraph;
  open BasicPatGraph;
  (* extend with new utilities for this kind of graphs *)
  fun match (g1 : T) (g2 : T) = false;
end;


