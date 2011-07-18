(*-------------------------------------------------------*)
functor OutputNoEdgeDataXMLList_v2(E : EDGE) : OUTPUT_XML_LIST
= struct
  type data = E.data;
  type T = XML.tree list
  fun output _ = []
end

functor OutputVertexXML_v2(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_XML_LIST
  sharing type OutputVertexData.data = Graph.Vertex.data
) : OUTPUT_XML
= struct
  structure V = Graph.OVertex;
  structure E = Graph.Edge;

  type T = XML.tree;
  type data = (V.name * (V.data * (E.NSet.T * E.NSet.T)));

  open XML;
  open XML_Output_Utils;

  fun output (name,(vdata, _)) = 
      Elem (("vertex", [("name", V.dest name)]), 
            OutputVertexData.output vdata);
end;

functor OutputEdgeXML_v2(
  structure Graph : BANG_GRAPH
        and OutputEdgeData : OUTPUT_XML_LIST
  sharing type OutputEdgeData.data = Graph.Edge.data
) : OUTPUT_XML
= struct
  structure V = Graph.OVertex;
  structure E = Graph.Edge;

  type T = XML.tree;
  type data = (E.name * ((Graph.dir_or_undir * E.data) * (V.name * V.name)));

  open XML;
  open XML_Output_Utils;

  fun output (name,((dir,edata), (src_vertex, target_vertex))) =
      (Elem 
       (("edge", 
        [("name", E.dest name),
         ("dir", if dir = Graph.Directed then "true" else "false"),
         ("source", V.dest src_vertex),
         ("target", V.dest target_vertex)]), 
        OutputEdgeData.output edata)
      );
end;

functor OutputBBoxXML_v2(
  structure Graph : BANG_GRAPH
) : OUTPUT_XML
= struct
  structure V = Graph.OVertex;
  structure BB = Graph.BBox;

  type T = XML.tree;
  type data = (BB.name * Graph.Vertex.NSet.T);

  open XML;
  open XML_Output_Utils;

  fun output (name,vertices) =
      Elem (("bangbox", [("name" ,BB.dest name)]),
            (map (fn vname => Elem (("vertex",[]),[Text (V.dest vname)]))
                 (Graph.Vertex.NSet.list_of vertices))
           );
end;

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
  structure vout = OutputVertexXML_v2(
    structure Graph = Graph
          and OutputVertexData = OutputVertexData);
  structure eout = OutputEdgeXML_v2(
    structure Graph = Graph
          and OutputEdgeData = OutputEdgeData);
  structure bbout = OutputBBoxXML_v2(
    structure Graph = Graph);

  type T = XML.tree list;
  type data = Graph.T;

  open XML;
  open XML_Output_Utils;

  fun output_vertices g = (map vout.output (Graph.get_vertex_list g))

  fun output_edges g = (map eout.output (Graph.get_edge_list g))

  fun output_bbox g box = bbout.output (box,(Graph.get_bbox g box))
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
functor OutputTypedDataXML(ComponentDataIO : ELEMENT_COMPONENT_DATA) 
: OUTPUT_XML_LIST = 
struct
  open XML_Parse_Utils;
  type data = ComponentDataIO.data;
  type T = XML.tree list;
  fun output d = 
      let
        val (typ,data_xml_list) = ComponentDataIO.type_and_xml_of d;
      in [Elem (("type",[]),[Text typ]),
          Elem (("data",[]),data_xml_list)]
      end;
end;

(* vim:et:sts=2:sw=2
*)
