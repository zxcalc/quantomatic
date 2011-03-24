(* Outputs an XML representation of a graph. *)

(*  *)
functor OutputRGVertexDataXMLList(
  V : OVERTEX where type IData.data = RG_VertexData.data 
) : OUTPUT_XML_LIST 
= struct 
  
  type T = XML.tree list;
  type data = V.data;

  open XML;
  open XML_Output_Utils;
  val colourRed = wrap_text_simple "colour" "red"
  val colourGreen = wrap_text_simple "colour" "green"
  val colourH = wrap_text_simple "colour" "H"

  fun output_idata (RG_VertexData.Xnd a) = 
      colourRed::(OutputLinratXML.output a)
    | output_idata (RG_VertexData.Znd a) = 
      colourGreen::(OutputLinratXML.output a)
    | output_idata (RG_VertexData.Hnd) = [colourH]

  fun output (V.EVert) = [wrap_bool_simple "boundary" true]
    | output (V.IVert rg) = 
      (wrap_bool_simple "boundary" false)::(output_idata rg)
end;

(*-------------------------------------------------------*)
functor OutputNoEdgeDataXMLList (E : EDGE) : OUTPUT_XML_LIST
= struct
  type data = E.data;
  type T = XML.tree list
  fun output _ = []
end

(* *)
functor OutputGraphContentsXMLList (
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.Vertex.data
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_XML_LIST
= struct
  structure V = Graph.OVertex;
  structure E = Graph.Edge

  structure VName = LinratExpr.VName;
  structure OutputVertexName = OutputSStrNameXML(V);
  structure OutputEdgeName = OutputSStrNameXML(E);
  structure OutputBBoxName = OutputSStrNameXML(Graph.BBox);

  type T = XML.tree list;
  type data = Graph.T;

  open XML;
  open XML_Output_Utils;
  (* rend_vdata is a function for rendering the vertex data *)
  fun output_vertex (name,(vdata, _)) = 
      wrap_els_simple "vertex" (
        (wrap_el_simple "name" (OutputVertexName.output name))::(OutputVertexData.output vdata)
      );

  fun output_vertices g = (map output_vertex (Graph.get_vertex_list g))

  fun output_edge (name,((dir,edata), (src_vertex, target_vertex))) =
      wrap_els_simple "edge" (
        [
          wrap_el_simple "name" (OutputEdgeName.output name),
          wrap_bool_simple "directed" (case dir of Graph.Directed => true 
		                                 | Graph.UnDirected => false),
          wrap_el_simple "source" (OutputVertexName.output src_vertex),
          wrap_el_simple "target" (OutputVertexName.output target_vertex)
        ]@OutputEdgeData.output edata
      );

  fun output_edges g = (map output_edge (Graph.get_edge_list g))

  fun output_bbox g box =
      wrap_els_simple "bangbox" (
          (wrap_el_simple "name" (OutputBBoxName.output box))
          ::(map (fn vname => wrap_el_simple "boxedvertex" (OutputVertexName.output vname))
                 (Graph.Vertex.NSet.list_of (Graph.get_bbox g box)))
      );


  fun output_bboxes g = (map (output_bbox g) (Graph.get_bbox_list g))

  fun output g = (output_vertices g)@(output_edges g)@(output_bboxes g)

end;

functor OutputGraphXML (
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.Vertex.data
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_XML
= OutputWrapXML(
    val tagname = "graph"
    val attrs = []
    structure Output1 = OutputGraphContentsXMLList(
      structure Graph = Graph
      structure OutputVertexData = OutputVertexData
      structure OutputEdgeData = OutputEdgeData
    )
)

(* vim:et:sts=2:sw=2
*)
