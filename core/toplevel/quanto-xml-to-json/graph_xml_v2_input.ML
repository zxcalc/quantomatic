(* XMl Parsers for BANG_GRAPH, RG_VERTEX, and RG_EDGE *)
functor InputGraphXML_v2(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_XML
        and InputEdgeData : INPUT_XML
  sharing type InputVertexData.data = Graph.vdata
  sharing type InputEdgeData.data = Graph.edata
)  : INPUT_XML
= struct
  open XML_Parse_Utils

  structure G = Graph
  
  type T = XML.tree  (* XML.tree *)
  type data = Graph.T
  
  fun input_edge_rel xml =
      let
        val _ = assert_tag "edge" xml
        val children = get_children xml
        val name = E.mk (get_attribute xml "name")
            handle bad_input_exp (s,p) => raise bad_input_exp (s,"edge/"^p)
        val dir_str = (get_attribute xml "dir")
                      handle bad_input_exp (s,p) =>
                        raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val dir = if dir_str = "true" then true
                  else if dir_str = "false" then false
                  else raise bad_input_exp ("Edge direction must be either 'true' or 'false', it cannot be: '" ^ dir_str ^ "'","edge("^(E.dest name)^").dir")
        val source = V.mk (get_attribute xml "source")
            handle bad_input_exp (s,p) =>
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val target = V.mk (get_attribute xml "target")
            handle bad_input_exp (s,p) =>
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val edata = InputEdgeData.input xml
            handle bad_input_exp (s,p) =>
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
      in (name,dir,edata,source,target) end
  
  fun input_bbox xml =
      let
        val _ = assert_tag "bangbox" xml
        val name = B.mk (get_attribute xml "name")
            handle bad_input_exp (s,p) =>
              raise bad_input_exp (s,"bangbox/"^p)
        val children = get_children xml
        val bangverts = List.map (V.mk o get_unique_child_text)
                                 (filter_elems "vertex" children)
            handle bad_input_exp (s,p) =>
              raise bad_input_exp (s,"bangbox("^(B.dest name)^")/"^p)
      in (name,bangverts) end
  
  fun add_v (vname,vdata) g =
      G.add_named_vertex vname vdata g
  fun add_e (ename,dir,edata,src,tgt) g =
      G.add_named_edge ename
            (if dir then Directed else Undirected,edata)
            src tgt g
  fun add_bb (bname,bvlist) g =
      let val g' = G.add_named_bbox bname g
          val vset = V.NSet.of_list bvlist
      in G.add_to_bbox_anon bname vset g' end
  
  fun input_vertex xml =
      let val () = assert_tag "vertex" xml
          val name = (get_attribute xml "name")
              handle bad_input_exp (s,p) => raise bad_input_exp (s,"vertex/"^p)
      in (V.mk name, InputVertexData.input xml)
         handle bad_input_exp (s,p) => 
           raise bad_input_exp (s,"vertex("^name^")/"^p)
      end

  (* A graph may be wrapped into a graph_and_user_data tag
   * or not. *)
  fun input_graph xml =
      (let
         val is_wrapped = test_tag "graph_and_user_data" xml
         val xml' = if is_wrapped then hd (get_children xml)
                                  else xml
         val () = assert_tag "graph" xml' (* Actually we already checked *)
         val children = get_children xml'
         val vertices = List.map input_vertex (filter_elems "vertex" children)
         val edges = List.map input_edge_rel (filter_elems "edge" children)
         val bboxes = List.map input_bbox (filter_elems "bangbox" children)
       in
         G.empty
           |> fold add_v vertices
           |> fold add_e edges
           |> fold add_bb bboxes
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"graph/"^p)
  
  val input = input_graph

end (* structure InputXML *)


(* Input some typed data.
   The XML is the XML of the vertex or edge. i.e. it looks like this:
     <vertex><type>...</type><data>...</data> ... </vertex>
   The same functor can be used for both vertices and edges: XML_TAG_NAME can
   be vertex or edge
*)
structure InputNoData : INPUT_XML =
struct
  exception bad_input_exp = XML_Parse_Utils.bad_input_exp
  type T = XML.tree
  type data = unit
  fun input _ = ()
end

