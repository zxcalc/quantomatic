(*-------------------------------------------------------*)

structure OutputAsCData : OUTPUT_XML_LIST
=struct

  type T = XML.tree list;
  type data = string;

  open XML;
  open XML_Output_Utils;

  fun output s = (CData s)::[]

end;

structure OutputUserDataXMLList:OUTPUT_XML_LIST
=struct
  type T = XML.tree list;
  type data = string Symtab.table;

  open XML;
  open XML_Output_Utils;

  fun output_entry (k, s) = 
    Elem (("entry", [("name", k)]),
    OutputAsCData.output s);

  fun output tab = map output_entry (Symtab.dest tab);

end;

functor OutputElementsUDXMLList(
  structure Nm : SSTR_NAME
) : OUTPUT_XML_LIST
= struct

  type T = XML.tree list;
  type data = ((string Symtab.table) Nm.NTab.T) * string;

  open XML;
  open XML_Output_Utils;

  fun output_element label (name, data) =
    Elem ((label, [("name", Nm.dest name)]),
      (OutputUserDataXMLList.output data));

    fun output (nm, label) = map (output_element label) (Nm.NTab.list_of nm);
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
  structure UIData : USER_DATA
) : OUTPUT_XML_LIST
= struct
  structure udvout = OutputElementsUDXMLList(
    structure Nm = V;
  );
  structure udeout = OutputElementsUDXMLList(
    structure Nm = E;
  );
  structure udbout = OutputElementsUDXMLList(
    structure Nm = B;
  )
  type T = XML.tree list;
  type data = UIData.T;

  open XML;
  open XML_Output_Utils;

  fun output_graph_user_data ud = [OutputGraphUDXML.output (UIData.get_g_ud ud)]
  fun output_vertices_user_data ud = udvout.output (UIData.get_vs_ud ud,
                                                    "vertex_userdata")
  fun output_edges_user_data ud = udeout.output (UIData.get_es_ud ud, "edge_userdata")
  fun output_bangboxes_user_data ud = udbout.output (UIData.get_bs_ud ud,
                                                   "bangbox_userdata")
  fun output ud = (output_graph_user_data ud)@(output_vertices_user_data ud)@
                  (output_edges_user_data ud)@(output_bangboxes_user_data ud);

end;

functor OutputUIDataXML(
  structure UIData : USER_DATA
) : OUTPUT_XML 
= OutputWrapXML(
  val tagname = "user_data"
  val attrs = []
  structure Output1 = OutputGraphUserDataXMLList(
    structure UIData = UIData
  )
)

structure OutputGraphAndUserDataXMLList : OUTPUT_XML_LIST
=struct
  type T = XML.tree list
  type data = XML.tree * XML.tree;

  fun output (t1,t2) = [t1]@[t2];
end;
(* vim:et:sts=2:sw=2
*)
