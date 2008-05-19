
(* Want to parameterise the output a bit *)

signature OUTPUT_DOT_BITS = 
sig

  include OUTPUT;

(* -- combinators for generating functions from OUTPUT -- *)       
  val mk_dot_vertex : outtype -> outtype -> outtype -> outtype ;

  val mk_output_vertices : ((vname * (vertex * (Graph.Edge.NSet.T * Graph.Edge.NSet.T))) -> outtype) 
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
  type vertex = Graph.Vertex.data;
  type edge = Graph.Edge.data;
  type vname = Graph.Vertex.name;
  type ename = Graph.Edge.name;

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
      (Pretty.block [Pretty.str "Graph G {",		     
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
    = mk_dot_vertex (Pretty.str "")  (Graph.Vertex.pretty_data vdata) (Graph.Vertex.pretty_name name);



  fun output_edge (_,(edata, (src_vertex, target_vertex))) 
    = Pretty.block [Pretty.str "\"",
		    Graph.Vertex.pretty_name src_vertex,
		    Pretty.str "\"--\"",
		    Graph.Vertex.pretty_name target_vertex,
		    Pretty.str "\"[label=\"",
		    Graph.Edge.pretty_data edata,
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
structure V = Graph.BVertex
structure E = Graph.Edge
structure In  = Graph.RGVertex
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
	    case vdata of V.BVert _ => (boundary_dot_style, Pretty.str str_name)
			| V.IVert stuff =>
			  case stuff of In.Xnd a => (default_Xdot_style, AngleExpr.pretty a)
				      | In.Znd a => (default_Zdot_style, AngleExpr.pretty a)
				      | In.Hnd => (default_Hdot_style, Pretty.str "H")
    in 
      guts.mk_dot_vertex (Pretty.str style)  (label) (V.pretty_name name)
    end
  end;
    

fun output_edge (ename,(edata, (src_vertex, target_vertex))) 
  = Pretty.block [Pretty.str "\"",
		  V.pretty_name src_vertex,
		  Pretty.str "\"--\"",
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


(*-------------------------------------------------------*)

structure StrGraphDotWriter : OUTPUT_WRITER 
			      where type graph = StrGraph.T
 = BasicWriter (OutputDot (StrGraph));

structure StrGraphDot : OUTPUT
			      where type graph = StrGraph.T
 = OutputDot (StrGraph);

