(*-------------------------------------------------------*)

structure OutputAsCData : OUTPUT_XML_LIST
=struct

  type T = XML.tree list;
  type data = string;

  open XML;
  open XML_Output_Utils;

  fun output s = Text ("<![CDATA[" ^  s ^ "]]>")::[]

end;

structure OutputUserDataXMLList:OUTPUT_XML_LIST
=struct
  type T = XML.tree list;
  type data = string Symtab.table;

  open XML;
  open XML_Output_Utils;

  fun output_entry (k, s) = 
    Elem (("entry", [("dataname", k)]),
    OutputAsCData.output s);

  fun output tab = map output_entry (Symtab.dest tab);

end;


functor OutputVertexUDXMLList(
  structure Graph : BANG_GRAPH
) : OUTPUT_XML_LIST
= struct

  type T = XML.tree list;
  type data = Graph.T;

  open XML;
  open XML_Output_Utils;

  fun output_vertex g (name, vdata) = 
    Elem (("vertex_data", [("name", V.dest name)]),
      (OutputUserDataXMLList.output
      (Graph.get_vertex_user_data g name)));

    fun output g = map (output_vertex g) (Graph.get_vertex_list g);
end;

structure OutputGraphUDXML: OUTPUT_XML
= struct

  type T = XML.tree;
  type data = string Symtab.table;

  open XML;
  open XML_Output_Utils;

  fun output tab = 
    wrap_els_simple "graph_data" (OutputUserDataXMLList.output tab);
end;


functor OutputGraphUserDataXMLList(
  structure Graph : BANG_GRAPH
) : OUTPUT_XML_LIST
= struct
  structure udvout = OutputVertexUDXMLList(
  structure Graph = Graph
  );
  type T = XML.tree list;
  type data = Graph.T;

  open XML;
  open XML_Output_Utils;

  fun output_graph_user_data g = [OutputGraphUDXML.output (Graph.get_user_data g)]
  fun output_vertices_user_data g = udvout.output g
  fun output g = (output_vertices_user_data g)@(output_graph_user_data g)

end;
(* vim:et:sts=2:sw=2
*)
