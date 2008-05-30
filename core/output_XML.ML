
(* Want to parameterise the output a bit *)
signature OUTPUT_XML_BITS = 
sig
  
  include OUTPUT;
  
(* -- combinators for generating functions from OUTPUT -- *)       
(* 
val mk_xml_vertex : outtype -> outtype -> outtype ;
   *)
 
val mk_output_vertices : 
    ((vname * (vertex * (Graph.Edge.NSet.T * Graph.Edge.NSet.T))) -> outtype) 
    -> 
    (graph ->   outtype);			
      
val mk_output_vertex : 
    (vertex -> outtype) 
    ->
    ((vname * (vertex * (Graph.Edge.NSet.T * Graph.Edge.NSet.T))) -> outtype) 
    
val mk_output_edges : 
    ((ename * (edge * (vname * vname))) -> outtype) 
    -> 
    (graph ->   outtype);
    
val mk_output_edge :
    (edge -> outtype)
    ->
    ((ename * (edge * (vname * vname))) -> outtype) 
    
    
val mk_output_graph : (graph ->   outtype)
		      -> 
		      (graph ->   outtype)
		      ->
		      (graph ->   outtype);
        
end;



(* ======================================================= *)


functor OutputXML_guts (Graph : GRAPH)  :  OUTPUT_XML_BITS  =  
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
    = Pretty.block [Pretty.str "  ", Pretty.chunks (map output_vertex (Graph.get_vertex_list g))];


  fun mk_output_edges (output_edge : (ename * (edge * (vname * vname))) -> outtype)( g : Graph.T) 
    = Pretty.block [Pretty.str "  ", 
		    Pretty.chunks (map output_edge (Graph.get_edge_list g)) ];
      



  fun mk_output_graph output_vertices output_edges g = 
      (Pretty.chunks [Pretty.str "<graph>",
		      Pretty.str "<!-- VERTICES -->",
		      output_vertices g,
		      Pretty.str "<!-- EDGES -->",
		      output_edges g,
		      Pretty.str "</graph>"]);


  fun mk_xml_vertex data_xml name 
    = Pretty.chunks [Pretty.str "<vertex>",
		     Pretty.block [
		     Pretty.str "  ",
		     Pretty.str "<name>", 
		     name,
		     Pretty.str "</name>"],
		     Pretty.block [
		     Pretty.str "  ",
		     data_xml],
		     Pretty.str "</vertex>"
		    ];
  
(* rend_vdata is a function for rendering the vertex data *)
  fun mk_output_vertex rend_vdata (name,(vdata, _)) 
    = mk_xml_vertex  (rend_vdata vdata) (Graph.Vertex.pretty_name name);


(* rend_edata is a function for rendering the edge data *)
  fun mk_output_edge rend_edata (name,(edata, (src_vertex, target_vertex))) 
    = Pretty.chunks [Pretty.str "<edge>",
		     Pretty.block [
		     Pretty.str "  ",
		     Pretty.str "<name>",
		     Graph.Edge.pretty_name name,
		     Pretty.str "</name>",
		     Pretty.str "<source>",
		     Graph.Vertex.pretty_name src_vertex,
		     Pretty.str "</source>",
		     Pretty.str "<target>",
		     Graph.Vertex.pretty_name target_vertex,
		     Pretty.str "</target>",
		     rend_edata edata
		     ],
		     Pretty.str "</edge>"];

(* both the edge and vertex data renderers are dummies -- to be overridden *)

  val output_edge = mk_output_edge (fn _ => Pretty.str "");
  val output_edges = mk_output_edges output_edge;

  val output_vertex = mk_output_vertex (fn _ => Pretty.str "");
  val output_vertices = mk_output_vertices output_vertex;

  val output_graph = mk_output_graph output_vertices output_edges;

  val output = Pretty.writeln o output_graph;

end;

(* === Dummy implementation for BasicGraph === *)

functor OutputXML (Graph : GRAPH) : OUTPUT =
struct 

structure guts = OutputXML_guts (Graph);
open guts;
     
end;

(*-------Specialised from above to red green GRAPHS------------------*)

functor RGGraphOutputXML (Graph : RG_GRAPH) : OUTPUT 
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

structure guts = OutputXML_guts (Graph);     
     
val boundary = Pretty.str "<boundary>true</boundary>";
val nonboundary = Pretty.str "<boundary>false</boundary>";

val colourRed = Pretty.str "<colour>red</colour>";
val colourGreen = Pretty.str "<colour>green</colour>";
val colourH = Pretty.str "<colour>H</colour>";


(* -- this section depends strongly on the linrat_expr type ----*)
fun render_xml_rational (n,d) = 
    Pretty.chunks [Pretty.str ("<num>"^ (string_of_int  n)^"</num>"),
		   Pretty.str ("<denom>"^ (string_of_int  d)^"</denom>")];

fun render_xml_constant_part k =
    if Rational.is_zero k then
      (Pretty.str "")
    else
      Pretty.chunks [Pretty.str "<constant>",
		     Pretty.block [ Pretty.str "  ", (render_xml_rational k)],
		     Pretty.str "</constant>"];

fun render_xml_coeff_var (var, coeff) = 
    Pretty.chunks [Pretty.str "<variable>",
		   Pretty.block [Pretty.str "  ",
				 Pretty.chunks [Pretty.str "<name>",
						VName.pretty_name var,
						Pretty.str "</name>",
						Pretty.str "<coefficient>",
						Pretty.block [
						  Pretty.str "  ",
						  (render_xml_rational coeff)
						  ],
						Pretty.str "</coefficient>"
						]
				],
		   Pretty.str "</variable>"];

fun render_xml_variable_part tab = 
    let fun f kv [] = [render_xml_coeff_var kv]
	  | f kv rest = (render_xml_coeff_var kv)::rest
    in
      Pretty.chunks (VName.NTab.fold f tab [])
    end;

fun render_xml_angleexpr (pi,tab) = 
    if AngleExpr.is_zero (pi,tab) then 
      []
    else
    [Pretty.str "<angleexpr>",
     Pretty.block [Pretty.str "  ",
		   Pretty.chunks [
		   render_xml_constant_part pi,
		   render_xml_variable_part tab]
		  ],
     Pretty.str "</angleexpr>"]
				


(*
fun pretty (pi, tab) =
    let val ppi  = if (Coeff.is_zero pi) then []
		               else [Pretty.block [Coeff.pretty pi,Pretty.str "*Pi"]]
	      fun pretty1 (k,v) [] =
	          [(Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty_name k])]
	        | pretty1 (k,v) pl =
	          (Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty_name k])::
	          ((Pretty.str "  +  ")::pl)
    in 
      Pretty.block (VName.NTab.fold pretty1 tab ppi)
    end

*)
fun render_xml_rgdata (V.Xnd a) = colourRed::(render_xml_angleexpr a)
  | render_xml_rgdata (V.Znd a) = colourGreen::(render_xml_angleexpr a)
  | render_xml_rgdata (V.Hnd) = colourH::[]

fun render_xml_bdata (V.BVert _) = boundary
  | render_xml_bdata (V.IVert rg) = Pretty.chunks (nonboundary::(render_xml_rgdata rg))

val output_vertex = guts.mk_output_vertex render_xml_bdata;
val output_vertices = guts.mk_output_vertices output_vertex;
val output_edge = guts.output_edge;
val output_edges = guts.output_edges;
val output_graph = guts.mk_output_graph output_vertices output_edges;

val output = Pretty.writeln o output_graph;


end;

(*-------------------------------------------------------*)

structure RGGraphXMLWriter : OUTPUT_WRITER 
			      where type graph = RGGraph.T
 = BasicWriter (RGGraphOutputXML (RGGraph));

structure RGGraphXML : OUTPUT 
			      where type graph = RGGraph.T
 = RGGraphOutputXML (RGGraph);


(*-------------------------------------------------------*)

structure StrGraphXMLWriter : OUTPUT_WRITER 
			      where type graph = StrGraph.T
 = BasicWriter (OutputXML (StrGraph));

structure StrGraphXML : OUTPUT
			      where type graph = StrGraph.T
 = OutputXML (StrGraph);

