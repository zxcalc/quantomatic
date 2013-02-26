functor OutputAnnotationXMLList(Nm : SSTR_NAME)
= struct
  fun output_element label (name, data) =
    XML.Elem ((label, [("name", Nm.dest name)]),
      (XML_Output_Utils.output_symtab_entries data));

  fun output (nm, label) = map (output_element label) (Nm.NTab.list_of nm);
end;


structure OutputGraphStringTableAnnotationsXML : OUTPUT_XML
= struct
  structure A = GraphStringTableAnnotations;

  structure VAnn = OutputAnnotationXMLList(V);
  structure EAnn = OutputAnnotationXMLList(E);
  structure BAnn = OutputAnnotationXMLList(B);
  type T = XML.tree;
  type data = A.T;

  open XML;
  open XML_Output_Utils;

  fun output_symtab_entry (k, s) = Elem (("entry", [("name", k)]), [Text s]);
  fun output_symtab tab = map output_symtab_entry (Symtab.dest tab);

  fun output_graph_user_data ud =
    [wrap_els_simple "graph_data" (output_symtab (A.get_graph_annotation ud))];
  fun output_vertices_user_data ud = VAnn.output (A.get_vertex_annotation_tab ud,
                                                    "vertex_userdata")
  fun output_edges_user_data ud = EAnn.output (A.get_edge_annotation_tab ud, "edge_userdata")
  fun output_bangboxes_user_data ud = BAnn.output (A.get_bangbox_annotation_tab ud,
                                                   "bangbox_userdata")
  fun output ud = XML.Elem (("user_data",[]),
      (output_graph_user_data ud)@(output_vertices_user_data ud)@
      (output_edges_user_data ud)@(output_bangboxes_user_data ud));
end;

