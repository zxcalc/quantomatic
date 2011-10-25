(*-------------------------------------------------------*)
functor OutputNoEdgeDataXMLList_v1(EData : UNIFIABLE_DATA) : OUTPUT_XML_LIST
= struct
  type data = EData.data;
  type T = XML.tree list
  fun output _ = []
end

(* *)
functor OutputGraphContentsXMLList_v1(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.VData.data
  sharing type OutputEdgeData.data = Graph.EData.data
) : OUTPUT_XML_LIST
= struct

  structure OutputVertexName = OutputSStrNameXML(V);
  structure OutputEdgeName = OutputSStrNameXML(E);
  structure OutputBBoxName = OutputSStrNameXML(B);

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
                 (V.NSet.list_of (Graph.get_bbox g box)))
      );


  fun output_bboxes g = (map (output_bbox g) (Graph.get_bbox_list g))

  fun output g = (output_vertices g)@(output_edges g)@(output_bboxes g)

end;

functor OutputGraphXML_v1(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.VData.data
  sharing type OutputEdgeData.data = Graph.EData.data
) : OUTPUT_XML
= OutputWrapXML(
    val tagname = "graph"
    val attrs = []
    structure Output1 = OutputGraphContentsXMLList_v1(
      structure Graph = Graph
      structure OutputVertexData = OutputVertexData
      structure OutputEdgeData = OutputEdgeData
    )
)

(* vim:et:sts=2:sw=2
*)
