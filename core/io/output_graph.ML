(* Outputs an XML representation of a graph using Pretty.
   TODO : recode using XML directly
*)

(*  *)
functor OutputRGVertexDataXML (V : RG_VERTEX) : OUTPUT_PRETTY_XML = 
struct 
  
  type T = Pretty.T;
  type data = V.data;

  val colourRed = Pretty.str "<colour>red</colour>";
  val colourGreen = Pretty.str "<colour>green</colour>";
  val colourH = Pretty.str "<colour>H</colour>";
  
  fun output_idata (V.Xnd a) = [colourRed, OutputLinratXML.output a]
    | output_idata (V.Znd a) = [colourGreen, OutputLinratXML.output a]
    | output_idata (V.Hnd) = [colourH]

  fun output (V.EVert) = Pretty.str "<boundary>true</boundary>"
    | output (V.IVert rg) = 
      Pretty.chunks (Pretty.str "<boundary>false</boundary>"
                     ::(output_idata rg))
end;

(*-------------------------------------------------------*)
functor OutputNoEdgeDataXML (E : EDGE) : OUTPUT_PRETTY_XML
= struct
  type data = E.data;
  type T = Pretty.T
  fun output _ = Pretty.str ""
end

(* *)
functor OutputGraphXML (
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_PRETTY_XML 
        and OutputEdgeData : OUTPUT_PRETTY_XML 
  sharing type OutputVertexData.data = Graph.Vertex.data
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_PRETTY_XML
= struct
  structure V = Graph.OVertex;
  structure E = Graph.Edge

  structure VName = LinratExpr.VName;

  type T = Pretty.T;  
  type data = Graph.T;

  (* rend_vdata is a function for rendering the vertex data *)
  fun output_vertex (name,(vdata, _)) = 
      Pretty.chunks [Pretty.str "<vertex>",
		     Pretty.block [
		     Pretty.str "  ",
		     Pretty.str "<name>", 
		     V.pretty_name name,
		     Pretty.str "</name>"],
		     Pretty.block [
		     Pretty.str "  ",
		     OutputVertexData.output vdata],
		     Pretty.str "</vertex>"
		    ];

  fun output_vertices g = 
      Pretty.block [Pretty.str "  ", 
		    Pretty.chunks (map output_vertex (Graph.get_vertex_list g))
		   ];

  fun output_edge (name,((dir,edata), (src_vertex, target_vertex))) =
      Pretty.chunks [Pretty.str "<edge>",
		     Pretty.block [
		     Pretty.str "  ",
		     Pretty.str "<name>",
		     Graph.Edge.pretty_name name,
		     Pretty.str "</name>",
		     Pretty.str "<directed>",
		     Pretty.str (case dir of Graph.Directed => "True" 
		                           | Graph.UnDirected => "False"),
		     Pretty.str "</directed>",
		     Pretty.str "<source>",
		     Graph.Vertex.pretty_name src_vertex,
		     Pretty.str "</source>",
		     Pretty.str "<target>",
		     Graph.Vertex.pretty_name target_vertex,
		     Pretty.str "</target>",
		     OutputEdgeData.output edata
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

  fun output g = 
      (Pretty.chunks [Pretty.str "<graph>",
		      Pretty.str "<!-- VERTICES -->",
		      output_vertices g,
		      Pretty.str "<!-- EDGES -->",
		      output_edges g,
		      Pretty.str "<!-- BANG BOXES  -->",
		      output_bboxes g,
		      Pretty.str "</graph>"]);

end;


