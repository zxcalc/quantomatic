signature INPUT =
sig
  structure G : BANG_GRAPH
  type graph = G.T (* a graph *)
  type vertex = G.Vertex.data
  type edge = G.Edge.data
  type bboxname = G.BBox.name
  type ename = G.Edge.name
  type vname = G.Vertex.name

  type intype (* the structured input type - usually XML.tree *)

  val input : intype -> graph (* this is the top level inputter *)
  val from_string : string -> intype

  exception Bad_input_exp of string;

  val input_graph : intype -> graph
  val input_edge  :  intype -> (ename * edge * vname * vname)
  val input_vertex  : intype -> (vname * vertex)
  val input_bbox : intype -> (bboxname * vname list)


end

signature INPUT_VDATA =
sig
  structure V : BVERTEX
  type vdata = V.IData.data
  type intype (* XML.tree *)
  val input_vdata : intype -> vdata
end

signature INPUT_EDATA =
sig
  structure E : EDGE
  type edata = E.data
  type intype (* XML.tree *)
  val input_edata : intype -> edata
end


(* -------------------------------------------------------- *)

signature INPUT_READER =
sig
  include INPUT;
  
  val default_file : string
  val read_file : string -> graph
					   
end


functor BasicReader (In : INPUT) : INPUT_READER
= 
struct 
structure G = In.G
type graph = In.graph;
type vertex = In.vertex;
type edge = In.edge;
type bboxname = In.bboxname;
type ename = In.ename ;
type vname = In.vname ;
type intype = In.intype;

exception Bad_input_exp = In.Bad_input_exp;

val default_file = "test_inputs/input.xml";

val input = In.input;
val input_graph = In.input_graph;
val input_edge = In.input_edge;
val input_vertex = In.input_vertex;
val input_bbox = In.input_bbox;
val from_string = In.from_string

fun read_file file = 
    let val instream =  TextIO.openIn file
    in 
      let fun read str =
	      case TextIO.inputLine instream
	       of  SOME s => read (str ^ s)
		 | NONE => str
      in 
	let val s = read ""
	in 
	  (
	   TextIO.closeIn instream ; 
	   (input o from_string) s
	  )
	end
      end
    end;

end

