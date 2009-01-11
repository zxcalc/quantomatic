signature OUTPUT =
sig
  structure Graph : BANG_GRAPH
  type graph = Graph.T (* a graph *)
  type vertex = Graph.Vertex.data
  type edge = Graph.Edge.data
  type bboxname = Graph.BBox.name
  type ename = Graph.Edge.name
  type vname = Graph.Vertex.name

  val output : graph -> unit

  type outtype (*  type of output; either  Pretty.T or unit*)

  val output_graph : graph -> outtype
  val output_edges : graph ->   outtype	      
  val output_vertices : graph ->   outtype
  val output_bboxes : graph -> outtype
  val output_edge  : (ename * (edge * (vname * vname))) -> outtype
  val output_vertex  : (vname * (vertex * (Graph.Edge.NSet.T * Graph.Edge.NSet.T))) -> outtype 
  val output_bbox : graph -> bboxname -> outtype

  val to_string : outtype -> string

end

signature OUTPUT_WRITER =
sig
  include OUTPUT;
  
  val default_file : string
  val output_to_file : string -> graph -> unit
					   
end


functor BasicWriter (Out : OUTPUT) : OUTPUT_WRITER
= 
struct 
structure Graph = Out.Graph
type graph = Out.graph;
type vertex = Out.vertex;
type edge = Out.edge;
type bboxname = Out.bboxname;
type ename = Out.ename ;
type vname = Out.vname ;
type outtype = Out.outtype;

  val default_file = "dotfiles/output.dot";

  fun output_to_file file g = 
      let 
	val s = Out.to_string (Out.output_graph g)
	val outstream =  TextIO.openOut file
      in 
	(
	 TextIO.output (outstream, s);
	 TextIO.closeOut outstream
	)
      end;

  val output = output_to_file default_file;

  val output_graph = Out.output_graph;
  val output_edges = Out.output_edges;
  val output_edge = Out.output_edge;
  val output_vertices = Out.output_vertices;
  val output_vertex = Out.output_vertex;
  val output_bboxes = Out.output_bboxes;
  val output_bbox = Out.output_bbox;
  val to_string = Out.to_string

end

