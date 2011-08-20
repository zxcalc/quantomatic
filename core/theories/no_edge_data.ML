(*  *)
structure NoEdgeData_ParamIO 
: GRAPH_TYPED_DATA_PARAM_XML_IO where type data = unit 
= struct
  type data = unit
  fun of_type_and_xml ("unit",_) = ()
    | of_type_and_xml (n,xml_list) = 
      raise XML_Parse_Utils.bad_input_exp 
        ("NoEdgeData_ParamIO: Bad type or XML; type = " ^ n ^ "XML: " ^ 
         (Pretty.string_of (Pretty.list "[" "]" 
           (map (Pretty.str o XML_Parse_Utils.get_text) xml_list))), 
         "data");
  fun type_and_xml_of () = ("unit",[]);
end;
