(*

Outputs an XML representation of a graph using Pretty.
TODO : recode using XML directly

*)

functor GraphOutputXML (structure Graph : BANG_GRAPH
                        structure OutVData : OUTPUT_VDATA
                        where type outtype = Pretty.T list
                        structure OutEData : OUTPUT_EDATA
                        where type outtype = Pretty.T
			sharing OutVData.V.SharingWithIData = Graph.BVertex.SharingWithIData
			sharing OutEData.E.EdgeSharingWithData = Graph.Edge.EdgeSharingWithData
) : OUTPUT_GRAPH 
= struct
  structure Graph = Graph;
  structure V = Graph.BVertex;
  structure E = Graph.Edge

  type graph = Graph.T;
  type vertex = V.data;
  type edge = E.data;
  type vname = V.name;
  type ename = E.name;
  type bboxes = Graph.VtoBoxMap.T;
  type bboxname = Graph.BBox.name;

  type intype = graph;
  type outtype = Pretty.T;

  val render_xml_rgdata = OutVData.output;  
  val render_xml_edgedata = OutEData.output;  
      
  val boundary = Pretty.str "<boundary>true</boundary>";
  val nonboundary = Pretty.str "<boundary>false</boundary>";


  fun render_xml_bdata (V.BVert) = boundary
    | render_xml_bdata (V.IVert rg) = 
      Pretty.chunks (nonboundary::(render_xml_rgdata rg))
				      

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
  fun output_vertex (name,(vdata, _)) 
    = mk_xml_vertex  (render_xml_bdata vdata) (Graph.Vertex.pretty_name name);

  fun output_vertices g = 
      Pretty.block [Pretty.str "  ", 
		    Pretty.chunks (map output_vertex (Graph.get_vertex_list g))
		   ];

  fun output_edge (name,(edata, (src_vertex, target_vertex))) 
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
		     render_xml_edgedata edata
		     ],
		     Pretty.str "</edge>"];


  fun output_edges g = 
      Pretty.block [Pretty.str "  ",
		    Pretty.chunks (map output_edge (Graph.get_edge_list g)) 
		   ];
      
fun output_bbox g box 
      = Pretty.chunks[Pretty.str "<bangbox>",
		      Pretty.block [
		      Pretty.str "  ",
		      Pretty.chunks[
		      Pretty.block [
		      Pretty.str "<name>",
		      Graph.BBox.pretty_name box,
		      Pretty.str "</name>"],
			Pretty.chunks (map 
				     (fn vname => Pretty.block
					 [Pretty.str "<boxedvertex>",
					  Graph.Vertex.pretty_name vname,
					  Pretty.str "</boxedvertex>"])
				     (Graph.Vertex.NSet.list_of 
					(Graph.get_bbox g box)))]],
		      Pretty.str "</bangbox>"
		      ];


  fun output_bboxes g =
      Pretty.block [Pretty.str "  ",
		    Pretty.chunks (map (output_bbox g)
				       (Graph.get_bbox_list g)) 
		   ];

  fun output_graph  g = 
      (Pretty.chunks [Pretty.str "<graph>",
		      Pretty.str "<!-- VERTICES -->",
		      output_vertices g,
		      Pretty.str "<!-- EDGES -->",
		      output_edges g,
		      Pretty.str "<!-- BANG BOXES  -->",
		      output_bboxes g,
		      Pretty.str "</graph>"]);

  val output = output_graph;

end;

(*-------------------------------------------------------*)

functor RGVDataOutputXML (Graph : RG_GRAPH) : OUTPUT_VDATA
=
struct
  structure V = Graph.RGVertex;
  type intype = V.IData.data;
  type outtype = Pretty.T list

val colourRed = Pretty.str "<colour>red</colour>";
val colourGreen = Pretty.str "<colour>green</colour>";
val colourH = Pretty.str "<colour>H</colour>";


(* -- this section depends strongly on the linrat_expr type ----*)
fun render_xml_rational (n,d) = 
    Pretty.chunks [Pretty.str ("<num>"^ (string_of_int  n)^"</num>"),
		   Pretty.str ("<denom>"^ (string_of_int  d)^"</denom>")];

fun render_xml_constant_part k =
    if AngleExpr.Coeff.is_zero k then
      (Pretty.str "")
    else
      Pretty.chunks [Pretty.str "<constant>",
		     Pretty.block [ Pretty.str "  ", (render_xml_rational k)],
		     Pretty.str "</constant>"];

fun render_xml_coeff_var (var, coeff) = 
    Pretty.chunks [Pretty.str "<variable>",
		   Pretty.block [Pretty.str "  ",
				 Pretty.chunks [
				 Pretty.block [Pretty.str "<name>",
					       VName.pretty_name var,
					       Pretty.str "</name>"],
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
		   Pretty.block [Pretty.str "<as_string>",
				 AngleExpr.pretty (pi,tab),
				 Pretty.str "</as_string>"],
		   render_xml_constant_part pi,
		   render_xml_variable_part tab]
		  ],
     Pretty.str "</angleexpr>"]

fun render_xml_rgdata (V.Xnd a) = colourRed::(render_xml_angleexpr a)
  | render_xml_rgdata (V.Znd a) = colourGreen::(render_xml_angleexpr a)
  | render_xml_rgdata (V.Hnd) = colourH::[]

val output = render_xml_rgdata
				
end

(*-------------------------------------------------------*)

functor RGEDataOutputXML (Graph : RG_GRAPH) : OUTPUT_EDATA
=
struct
  structure E = Graph.Edge;
  type intype = E.data;
  type outtype = Pretty.T
  fun output _ = Pretty.str ""
end

(*-------------------------------------------------------*)



structure RGGraphOutputXML : OUTPUT_GRAPH
  = GraphOutputXML (
    structure Graph = RGGraph
    and OutVData = RGVDataOutputXML(RGGraph)
    and OutEData = RGEDataOutputXML(RGGraph)
    );

