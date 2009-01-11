(* For loading and saving graphs as XML *)
(* rwd 9-jan-2009 *)

signature LOADSAVE =
sig 

  structure Graph : GRAPH
  type graph = Graph.T

  val load_graph : string -> graph;
  val save_graph : string -> graph -> unit; 
end


structure RGGraphLoaderSaver : LOADSAVE =
struct

structure Graph = RGGraph;
type graph = Graph.T;

val save_graph = RGGraphXMLWriter.output_to_file;

val load_graph = RGGraphInputXMLReader.read_file;

end

(*  -- just for debugging --
structure rgls = RGGraphLoaderSaver;
*)
