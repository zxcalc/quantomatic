(*-------------------------------------------------------*)
functor OutputNoEdgeDataXMLList_v2(E : EDGE) : OUTPUT_XML_LIST
= struct
  type data = E.data;
  type T = XML.tree list
  fun output _ = []
end

(* Assumes: (Graph.V.dest, Graph.V.mk) is an isomorphism i.e. 
   Graph.V.dest o Graph.V.mk = Id_{string}, and
   Graph.V.mk o Graph.V.dest = Id_{V.name}
*)
functor OutputGraphContentsXMLList_v2(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.Vertex.data
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_XML_LIST
= struct
  structure V = Graph.OVertex;
  structure E = Graph.Edge;
  structure BB = Graph.BBox;
  structure VName = LinratExpr.VName;

  type T = XML.tree list;
  type data = Graph.T;

  open XML;
  open XML_Output_Utils;
  (* rend_vdata is a function for rendering the vertex data *)
  fun output_vertex (name,(vdata, _)) = 
      Elem (("vertex", [("name", V.dest name)]), 
            OutputVertexData.output vdata);

  fun output_vertices g = (map output_vertex (Graph.get_vertex_list g))
  fun output_edge (name,((dir,edata), (src_vertex, target_vertex))) =
      (Elem 
       (("edge", 
        [("name", E.dest name),
         ("dir", if dir = Graph.Directed then "true" else "false"),
         ("source", V.dest src_vertex),
         ("target", V.dest target_vertex)]), 
        OutputEdgeData.output edata)
      );
  fun output_edges g = (map output_edge (Graph.get_edge_list g))

  fun output_bbox g box =
      Elem (("bangbox", [("name" ,BB.dest box)]),
            (map (fn vname => Elem (("vertex",[]),[Text (V.dest vname)]))
                 (Graph.Vertex.NSet.list_of (Graph.get_bbox g box)))
           );

  fun output_bboxes g = (map (output_bbox g) (Graph.get_bbox_list g))

  fun output g = (output_vertices g)@(output_edges g)@(output_bboxes g)

end;

functor OutputGraphXML_v2(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.Vertex.data
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_XML
= OutputWrapXML(
    val tagname = "graph"
    val attrs = []
    structure Output1 = OutputGraphContentsXMLList_v2(
      structure Graph = Graph
      structure OutputVertexData = OutputVertexData
      structure OutputEdgeData = OutputEdgeData
    )
)


(* Functor to output typed data:
     <type>type info</type>
     <data>vertex or edge data</data>
   This is an OUTPUT_XML_LIST, which forms the content of the vertex or 
   edge tag. (The same functor can be used for both vertices and edges) 
*)
functor OutputTypedData(TypedDataParamIO : GRAPH_TYPED_DATA_PARAM_XML_IO) 
: OUTPUT_XML_LIST = 
struct
  open XML_Parse_Utils;
  type data = TypedDataParamIO.data;
  type T = XML.tree list;
  fun output d = 
      let
        val (typ,data_xml_list) = TypedDataParamIO.type_and_xml_of d;
      in [Elem (("type",[]),[Text typ]),
          Elem (("data",[]),data_xml_list)]
      end;
end;

(* vim:et:sts=2:sw=2
*)
