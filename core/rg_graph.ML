signature RG_GRAPH = 
sig
include BGRAPH 
structure RGVertex : RG_VERTEX
sharing Vertex = BVertex
sharing Vertex = RGVertex 
sharing RGVertex.IVertex = IVertex
sharing BVertex.IVertex = IVertex 

(* Given selection set of vertices, make subgraph: 
   vertices with edges outside selection become boundary *)
val subgraph_of_selection : Vertex.NSet.T -> T -> T

end

structure DB_RGGraph
= struct 
  structure RGVertex = RGVertex;
  structure BGraph = BGraphFun(structure BVertex = RGVertex  
                               and Edge = UnitEdge)
  open BGraph;

(* IMPROVE: I think it can probably be made much more efficient *)
(* make a subgraph from a node selection: any vertices with edges
   outside selection become boundary, and all non-selected vertices are 
   deleted. *)
fun subgraph_of_selection vns g = 
    g |> (RGVertex.NSet.fold
            (fn n => fn g => 
                        if RGVertex.NSet.is_empty 
                             (RGVertex.NSet.subtract (adj_vnames g n) vns)
                        then g
                        else set_vertex (RGVertex.BVert ()) n g)
            vns)
      |> (RGVertex.NSet.fold 
            (fn n => fn g => 
                        if RGVertex.NSet.contains vns n then g 
                        else delete_vertex n g)
            (get_vnames g));
end;

structure RGGraph : RG_GRAPH = DB_RGGraph;

(* incomplete and needs lots more stuff... eg. table

structure RGGraph :> RG_GRAPH 
  where type Vertex.data = RGVertex.data
    and type Vertex.name = RGVertex.name 
    and type Vertex.renaming = RGVertex.renaming
    and type Vertex.NSet.T = RGVertex.NSet.T 
    and type 'a Vertex.NTab.T = 'a RGVertex.NTab.T 
    and type Edge.data = UnitEdge.data
    and type Edge.name = UnitEdge.name
    and type Edge.renaming = UnitEdge.renaming
    and type Edge.NSet.T = UnitEdge.NSet.T 
    and type 'a Edge.NTab.T = 'a UnitEdge.NTab.T 
  = DB_RGGraph;
*)


(* structure Vertex = RGGraph.Vertex;
structure Edge = RGGraph.Edge;
*)
