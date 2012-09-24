structure InputGraphStringTableAnnotationsJSON
  : GRAPH_ANN_INPUT_JSON where type data = GraphStringTableAnnotations.T
= struct
  open JsonInputUtils;
  structure Ann = GraphStringTableAnnotations;
  
  type data = Ann.T;
  val empty_annotation = Ann.init;
  
  fun add_entry (k,Json.String v) = Symtab.update_new (k,v)
    | add_entry (k,Json.Int i) = Symtab.update_new (k,Int.toString i)
    | add_entry (k,Json.Real r) = Symtab.update_new (k,Real.toString r)
    | add_entry (_,Json.Null) = I
    | add_entry _ = raise bad_input_exp ("Expected string","")
  
  fun build_userdata Json.Null = Symtab.empty
    | build_userdata (Json.Object obj) =
          Json.fold add_entry obj Symtab.empty
    | build_userdata _ = raise bad_input_exp ("Expected object","")
  
  fun input_vertex_annotation v json =
        Ann.set_vertex_annotation v (build_userdata json);
  fun input_edge_annotation e json =
        Ann.set_edge_annotation e (build_userdata json);
  fun input_bangbox_annotation bb json =
        Ann.set_bangbox_annotation bb (build_userdata json);
  val input_graph_annotation = Ann.set_graph_annotation o build_userdata;
end;

structure OutputGraphStringTableAnnotationsJSON
  : GRAPH_ANN_OUTPUT_JSON where type data = GraphStringTableAnnotations.T
= struct
  open JsonOutputUtils;
  structure Ann = GraphStringTableAnnotations;
  
  type data = Ann.T;
  val empty_annotation = Ann.init;
  
  fun update_entry (k,v) = Json.update (k,Json.String v)
  fun write_userdata ud =
    if Symtab.is_empty ud
    then Json.Null
    else Json.Object (Symtab.fold update_entry ud Json.empty_obj);
  
  val output_vertex_annotation = write_userdata oo Ann.get_vertex_annotation;
  val output_edge_annotation = write_userdata oo Ann.get_edge_annotation;
  val output_bangbox_annotation = write_userdata oo Ann.get_bangbox_annotation;
  val output_graph_annotation = write_userdata o Ann.get_graph_annotation;
end;

