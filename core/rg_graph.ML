signature RG_GRAPH = 
sig
include BGRAPH
structure RGVertex : RG_VERTEX
sharing BVertex.BasicVert = RGVertex
end

structure DB_RGGraph
= struct 
  structure RGVertex = RGVertex;
  structure BGraph = BGraphFun(
    structure Vertex = RGVertex 
    and Edge = UnitEdge 
    and Boundary = UnitData)
open BGraph;
end;

structure RGGraph : RG_GRAPH = DB_RGGraph;

structure Vertex = RGGraph.Vertex;
structure Edge = RGGraph.Edge;
