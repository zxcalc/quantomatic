
(* Want to parameterise the output a bit *)

signature OUTPUT_DOT_BITS = 
sig

  include OUTPUT;

(* -- combinators for generating functions from OUTPUT -- *)       
  val mk_dot_node : outtype -> outtype -> outtype -> outtype ;

  val mk_output_nodes : ((nname * (node * (EName.NTab.N.T * EName.NTab.N.T))) -> outtype) 
			-> 
			(graph ->   outtype);
			
  val mk_output_edges : ((ename * (edge * (nname * nname))) -> outtype) 
			-> 
			(graph ->   outtype);

  val mk_output_graph : (graph ->   outtype)
			-> 
			(graph ->   outtype)
			->
			(graph ->   outtype);
											   
end;



(* ======================================================= *)


functor OutputDot_guts (Graph : BASIC_GRAPH)  :  OUTPUT_DOT_BITS  =  
struct

  type graph = Graph.T;
  type node = Graph.node;
  type edge = Graph.edge;
  type nname = Graph.nname;
  type ename = Graph.ename;

  type outtype = Pretty.T;
       
  val to_string = Pretty.string_of;
      
      (* Combinators to construct the other functions *)
  fun mk_output_nodes output_node g  
    = Pretty.chunks [Pretty.str "/* --- NODES -- */\n",
		     Pretty.chunks (map output_node (Graph.get_node_list g)),
		     Pretty.str "\n"];


  fun mk_output_edges output_edge g  
    = Pretty.chunks [Pretty.str "/* --- EDGES -- */\n",
		     Pretty.chunks (map output_edge (Graph.get_edge_list g)),
		     Pretty.str "\n"];
      



  fun mk_output_graph output_nodes output_edges g = 
      (Pretty.block [Pretty.str "Graph G {",		     
		     Pretty.chunks [Pretty.str "",
				    Pretty.str "ranksep=0.25;", 
				    Pretty.str "nodesep=0.25;",
				    Pretty.str "bgcolor=white;",
				    Pretty.str "",
				    output_nodes g,
				    output_edges g,
				    Pretty.str "}"]]);


  fun mk_dot_node style label name 
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
      

  fun output_node (name,(ndata, _)) 
    = mk_dot_node (Pretty.str "")  (Graph.pretty_node ndata) (NName.pretty name);



  fun output_edge (_,(edata, (src_node, target_node))) 
    = Pretty.block [Pretty.str "\"",
		    NName.pretty src_node,
		    Pretty.str "\"--\"",
		    NName.pretty target_node,
		    Pretty.str "\"[label=\"",
		    Graph.pretty_edge edata,
		    Pretty.str "\"];"];

  val output_edges = mk_output_edges output_edge;
  val output_nodes = mk_output_nodes output_node;
  val output_graph = mk_output_graph output_nodes output_edges;

  val output = Pretty.writeln o output_graph;

end;

(* === Dummy implementation for BasicGraph === *)

functor OutputDot (Graph : BASIC_GRAPH) : OUTPUT =
struct 

structure guts = OutputDot_guts (Graph);
open guts;
     
end;

(*-------Specialised from above to QGRAPHS------------------*)

functor QGraphOutputDot (Graph : QGRAPH) : OUTPUT 
  =
struct 

type graph = Graph.T;
type node = Graph.node;
type edge = Graph.edge;
type nname = Graph.nname;
type ename = Graph.ename;

type outtype = Pretty.T;
val to_string = Pretty.string_of;

structure guts = OutputDot_guts (Graph);     
     
val default_Xdot_style = 
    "fillcolor=red,shape=circle,style=filled,height=0.2,fixedsize=true";
val default_Zdot_style = 
    "fillcolor=green,shape=circle,style=filled,height=0.2,fixedsize=true";
val default_Hdot_style = 
    "shape=rect, fillcolor=yellow, style=filled, height=0.2 width=0.2,fixedsize=true" ;
val default_outpoint_style = "shape=none,height=0.2,width=0.2,fixedsize=true";
val default_inpoint_style = "shape=none,height=0.2,width=0.2,fixedsize=true";

(* handle nodes differently than OutputDot *)    
fun output_node (name,(ndata, _))
  = 
  let 
    val str_name = Pretty.string_of (NName.pretty name) 
  in
    let val (style,label)  = 
	    case ndata
	     of  Graph.QuNE.Node (BasicQuNE.Xnd a) => (default_Xdot_style, a)
	       | Graph.QuNE.Node (BasicQuNE.Znd a) => (default_Zdot_style, a)
	       | Graph.QuNE.Node BasicQuNE.Hnd => (default_Hdot_style, "H")
	       | Graph.QuNE.VarNode Graph.QuNE.InputNd => 
		 (default_inpoint_style, str_name)
	       | Graph.QuNE.VarNode Graph.QuNE.OutputNd => 
		 (default_outpoint_style, str_name)
    in 
      guts.mk_dot_node (Pretty.str style)  (Pretty.str label) (NName.pretty name)
    end
  end;
    

fun output_edge (ename,(edata, (src_node, target_node))) 
  = Pretty.block [Pretty.str "\"",
		  NName.pretty src_node,
		  Pretty.str "\"--\"",
		  NName.pretty target_node,
		  Pretty.str "\";"];
    

fun is_varnode_in (name,(ndata, _)) = 
    case ndata
     of Graph.QuNE.VarNode Graph.QuNE.InputNd => true
      | _ => false

fun is_varnode_out (name,(ndata, _)) = 
    case ndata
     of Graph.QuNE.VarNode Graph.QuNE.OutputNd => true
      | _ => false

fun is_interior_node (name,(ndata, _)) = 
    case ndata
     of Graph.QuNE.VarNode _ => false
      | _ => true


fun output_varnode_inputs g = 
    Pretty.chunks [Pretty.str "/* --- INPUT NODES -- */\n",
		     Pretty.str "subgraph INPUTS {",
		     Pretty.str "rank=source;",
		     Pretty.chunks (map output_node (filter is_varnode_in (Graph.get_node_list g))),
		     Pretty.str "}\n"];

fun output_varnode_outputs g = 
    Pretty.chunks [Pretty.str "/* --- OUTPUT NODES -- */\n",
		     Pretty.str "subgraph OUTPUTS {",
		     Pretty.str "rank=sink;",
		     Pretty.chunks (map output_node (filter is_varnode_out (Graph.get_node_list g))),
		     Pretty.str "}\n"];

fun output_other_nodes g  
  = Pretty.chunks [Pretty.str "/* --- NODES -- */\n",
		   Pretty.chunks (map output_node (filter is_interior_node (Graph.get_node_list g))),
		   Pretty.str "\n"];

fun output_nodes g = 
    Pretty.block [output_varnode_inputs g, output_varnode_outputs g, output_other_nodes g];


val output_edges = guts.mk_output_edges output_edge;
val output_graph = guts.mk_output_graph output_nodes output_edges;

val output = Pretty.writeln o output_graph;


end;

(*-------------------------------------------------------*)

structure QGraphDotWriter : OUTPUT_WRITER 
			      where type graph = QGraph.T
 = BasicWriter (QGraphOutputDot (QGraph));

structure QGraphDot : OUTPUT 
			      where type graph = QGraph.T
 = QGraphOutputDot (QGraph);


(*-------------------------------------------------------*)

structure StrGraphDotWriter : OUTPUT_WRITER 
			      where type graph = StrGraph.T
 = BasicWriter (OutputDot (StrGraph));

structure StrGraphDot : OUTPUT
			      where type graph = StrGraph.T
 = OutputDot (StrGraph);

