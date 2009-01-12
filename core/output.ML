signature OUTPUT =
sig
  type intype
  type outtype
  val output : intype -> outtype
end


signature OUTPUT_GRAPH =
sig
  structure Graph : BANG_GRAPH

  include OUTPUT where type intype = Graph.T

  type graph = Graph.T (* a graph *)
  type vertex = Graph.Vertex.data
  type edge = Graph.Edge.data
  type bboxname = Graph.BBox.name
  type ename = Graph.Edge.name
  type vname = Graph.Vertex.name

  val output_graph : graph -> outtype
  val output_edges : graph ->   outtype	      
  val output_vertices : graph ->   outtype
  val output_bboxes : graph -> outtype
  val output_edge  : (ename * (edge * (vname * vname))) -> outtype
  val output_vertex  : (vname * (vertex * (Graph.Edge.NSet.T * Graph.Edge.NSet.T))) -> outtype 
  val output_bbox : graph -> bboxname -> outtype

end


signature OUTPUT_VDATA =
sig 
  structure V : BVERTEX
  include OUTPUT where type intype = V.IData.data
end

signature OUTPUT_EDATA =
sig 
  structure E : EDGE
  include OUTPUT where type intype = E.data
end
