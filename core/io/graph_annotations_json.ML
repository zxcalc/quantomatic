functor InputGraphAnnotationsJSON(
  structure Annotations : GRAPH_ANNOTATIONS
  val json_to_annotation : Json.json -> Annotations.data
) : GRAPH_ANN_INPUT_JSON
= struct
  open JsonInputUtils;
  structure Ann = Annotations;
  
  type data = Ann.T;
  val empty_annotation = Ann.init;
  
  fun input_vertex_annotation v json =
        Ann.set_vertex_annotation v (json_to_annotation json);
  fun input_edge_annotation e json =
        Ann.set_edge_annotation e (json_to_annotation json);
  fun input_bbox_annotation bb json =
        Ann.set_bbox_annotation bb (json_to_annotation json);
  val input_graph_annotation = Ann.set_graph_annotation o json_to_annotation;
end;

structure InputGraphStringTableAnnotationsJSON
  : GRAPH_ANN_INPUT_JSON where type data = GraphStringTableAnnotations.T
= struct
  open JsonInputUtils;
  
  structure InputAnn = InputGraphAnnotationsJSON(
    structure Annotations = GraphStringTableAnnotations
    val json_to_annotation = input_string_table
  );
  open InputAnn;
end;

functor OutputGraphAnnotationsJSON(
  structure Annotations : GRAPH_ANNOTATIONS
  val annotation_to_json : Annotations.data -> Json.json
) : GRAPH_ANN_OUTPUT_JSON
= struct
  open JsonOutputUtils;
  structure Ann = Annotations;
  
  type data = Ann.T;
  val empty_annotation = Ann.init;
  
  val output_vertex_annotation =
    annotation_to_json oo Ann.get_vertex_annotation;
  val output_edge_annotation =
    annotation_to_json oo Ann.get_edge_annotation;
  val output_bbox_annotation =
    annotation_to_json oo Ann.get_bbox_annotation;
  val output_graph_annotation =
    annotation_to_json o Ann.get_graph_annotation;
end;

structure OutputGraphStringTableAnnotationsJSON
  : GRAPH_ANN_OUTPUT_JSON where type data = GraphStringTableAnnotations.T
= struct
  open JsonOutputUtils;
  
  structure OutputAnn = OutputGraphAnnotationsJSON(
    structure Annotations = GraphStringTableAnnotations
    val annotation_to_json = output_string_table
  );
  open OutputAnn;
end;

