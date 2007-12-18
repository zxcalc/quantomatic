signature OUTPUT =
sig
  type graph (* a graph *)
  type node
  type edge
  type ename
  type nname

  val output : graph -> unit

  type outtype (*  type of output; either  Pretty.T or unit*)

  val output_graph : graph -> outtype
  val output_edges : graph ->   outtype	      
  val output_nodes : graph ->   outtype	      
  val output_edge  : (ename * (edge * (nname * nname))) -> outtype
  val output_node  : (nname * (node * (EName.Col.T * EName.Col.T))) -> outtype

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

type graph = Out.graph;
type node = Out.node;
type edge = Out.edge;
type ename = Out.ename ;
type nname = Out.nname ;
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
  val output_nodes = Out.output_nodes;
  val output_node = Out.output_node;
  val to_string = Out.to_string

end

