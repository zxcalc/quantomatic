
(* Want to parameterise the output a bit *)

signature OUTPUT_DOT_BITS = 
sig

  include OUTPUT;

(* -- combinators for generating functions from OUTPUT -- *)       
  val mk_dot_vertex : outtype -> outtype -> outtype -> outtype ;

  val mk_output_vertices : ((vname * (vertex * (E.NSet.T * E.NSet.T))) -> outtype) 
			-> 
			(graph ->   outtype);
			
  val mk_output_edges : ((ename * (edge * (vname * vname))) -> outtype) 
			-> 
			(graph ->   outtype);

  val mk_output_graph : (graph ->   outtype)
			-> 
			(graph ->   outtype)
			->
			(graph ->   outtype);
											   
end;



(* ======================================================= *)


functor OutputDot_guts (Graph : GRAPH)  :  OUTPUT_DOT_BITS  =  
struct
  structure Graph = Graph;
  type graph = Graph.T;
  type vertex = Graph.VData.data;
  type edge = Graph.EData.data;
  type vname = V.name;
  type ename = E.name;

  type outtype = Pretty.T;
       
  val to_string = Pretty.string_of;
      
      (* Combinators to construct the other functions *)
  fun mk_output_vertices output_vertex g  
    = Pretty.chunks [Pretty.str "/* --- VERTICES -- */\n",
		     Pretty.chunks (map output_vertex (Graph.get_vertex_list g)),
		     Pretty.str "\n"];


  fun mk_output_edges (output_edge : (ename * (edge * (vname * vname))) -> outtype)( g : Graph.T) 
    = Pretty.chunks [Pretty.str "/* --- EDGES -- */\n",
		     Pretty.chunks (map output_edge (Graph.get_edge_list g)),
		     Pretty.str "\n"];
      



  fun mk_output_graph output_vertices output_edges g = 
      (Pretty.block [Pretty.str "digraph G {",		     
		     Pretty.chunks [Pretty.str "",
				    Pretty.str "ranksep=0.25;", 
				    Pretty.str "verticesep=0.25;",
				    Pretty.str "bgcolor=white;",
				    Pretty.str "",
				    output_vertices g,
				    output_edges g,
				    Pretty.str "}"]]);


  fun mk_dot_vertex style label name 
    = Pretty.block [Pretty.str "\"", 
		    name,
		    Pretty.str "\"",
		    Pretty.str "[",
		    Pretty.str "label=\"",
		    label,
		    Pretty.str "\",",
		    style,
		    Pretty.str "];\"",
		    name,
		    Pretty.str "\"--\"",
		    name,
		    Pretty.str "\"[headlabel=\"",
		    name,
		    Pretty.str "\",labeldistance=1.5,fontname=\"Courier\",color=white];"
		   ];
      

  fun output_vertex (name,(vdata, _)) 
    = mk_dot_vertex (Pretty.str "")  (Graph.VData.pretty_data vdata) (V.pretty_name name);



  fun output_edge (_,(edata, (src_vertex, target_vertex))) 
    = Pretty.block [Pretty.str "\"",
		    V.pretty_name src_vertex,
		    Pretty.str "\"->\"",
		    V.pretty_name target_vertex,
		    Pretty.str "\"[label=\"",
		    Graph.EData.pretty_data edata,
		    Pretty.str "\"];"];

  val output_edges = mk_output_edges output_edge;
  val output_vertices = mk_output_vertices output_vertex;
  val output_graph = mk_output_graph output_vertices output_edges;

  val output = Pretty.writeln o output_graph;

end;

(* === Dummy implementation for BasicGraph === *)

functor OutputDot (Graph : GRAPH) : OUTPUT =
struct 

structure guts = OutputDot_guts (Graph);
open guts;
     
end;

(*-------Specialised from above to red green GRAPHS------------------*)

functor RGGraphOutputDot (Graph : RG_GRAPH) : OUTPUT 
  =
struct 

structure Graph = Graph;
structure V = Graph.RGVertex
structure E = Graph.Edge
type graph = Graph.T;

type vertex = V.data;
type vname = V.name;
type edge = E.data;
type ename = E.name;

type outtype = Pretty.T;
val to_string = Pretty.string_of;

structure guts = OutputDot_guts (Graph);     
     
val default_Xdot_style = 
    "fillcolor=red,shape=circle,style=filled,height=0.2,fixedsize=true";
val default_Zdot_style = 
    "fillcolor=green,shape=circle,style=filled,height=0.2,fixedsize=true";
val default_Hdot_style = 
    "shape=rect, fillcolor=yellow, style=filled, height=0.2 width=0.2,fixedsize=true" ;
val boundary_dot_style = "shape=none,height=0.2,width=0.2,fixedsize=true";


(* handle vertices differently than OutputDot *)    
fun output_vertex (name,(vdata, _))
  = 
  let 
    val str_name = Pretty.string_of (V.pretty_name name) 
  in
    let val (style,label)  = 
	    case vdata of V.WVert => (boundary_dot_style, Pretty.str str_name)
			| V.NVert stuff =>
			  case stuff of V.Xnd a => (default_Xdot_style, LinratAngleExpr.pretty a)
				      | V.Znd a => (default_Zdot_style, LinratAngleExpr.pretty a)
				      | V.Hnd => (default_Hdot_style, Pretty.str "H")
    in 
      guts.mk_dot_vertex (Pretty.str style)  (label) (V.pretty_name name)
    end
  end;
    

fun output_edge (ename,(edata, (src_vertex, target_vertex))) 
  = Pretty.block [Pretty.str "\"",
		  V.pretty_name src_vertex,
		  Pretty.str "\"->\"",
		  V.pretty_name target_vertex,
		  Pretty.str "\";"];


fun output_vertices g  
  = Pretty.chunks [Pretty.str "/* --- VERTICES -- */\n",
		   Pretty.chunks (map output_vertex  (Graph.get_vertex_list g)),
		   Pretty.str "\n"];

val output_edges = guts.mk_output_edges output_edge;
val output_graph = guts.mk_output_graph output_vertices output_edges;

val output = Pretty.writeln o output_graph;


end;

(*-------------------------------------------------------*)

structure RGGraphDotWriter : OUTPUT_WRITER 
			      where type graph = RGGraph.T
 = BasicWriter (RGGraphOutputDot (RGGraph));

structure RGGraphDot : OUTPUT 
			      where type graph = RGGraph.T
 = RGGraphOutputDot (RGGraph);


