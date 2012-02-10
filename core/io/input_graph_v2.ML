(* XMl Parsers for BANG_GRAPH, RG_VERTEX, and RG_EDGE *)
(* FIXME: structure used is a bit squanky and ineffeccient. *)
functor InputGraphXML_v2(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_XML
        and InputEdgeData : INPUT_XML
  sharing type InputVertexData.data = Graph.VData.data
  sharing type InputEdgeData.data = Graph.EData.data
)  : INPUT_XML
= struct
  open XML_Parse_Utils; 

  structure G = Graph;

  type T = XML.tree;  (* XML.tree *)
  type data = Graph.T;

  fun data_to_string _ = "[Graph]";
 
  fun input_entry xml =
    let
      val _ = assert_tag "entry" xml
      val child = get_unique_child xml
      val k = (get_attribute xml "dataname") 
              handle bad_input_exp (s,p) => raise bad_input_exp (s,"entry/"^p)
      val data = get_cdata child
    in
      (k, data) 
    end;

  fun input_graph_ud xml = 
    let
      val _ = assert_tag "graph_data" xml
      val children = get_children xml
      fun mk_tab entries = Symtab.make entries
    in
      mk_tab (map input_entry (filter_elems "entry" children))
    end;

  fun input_vertex_ud xml =
    let
      val _ = assert_tag "vertex_data" xml
      val name = (get_attribute xml "name")
              handle bad_input_exp (s,p) => raise bad_input_exp (s,"vertex_data/"^p)
      val children = get_children xml
      fun mk_tab entries = Symtab.make entries
    in
      (V.mk name, mk_tab (map input_entry (filter_elems "entry" children)))
         handle bad_input_exp (s,p) => 
           raise bad_input_exp (s,"vertex_data("^name^")/"^p)
    end;

  fun set_graph_ud tab g =
    G.update_graph_user_data (fn _ => tab) g;

  fun set_vertex_ud (vn,tab) g =
    let
      val brel = G.get_vertices_u_d_brel g;
      fun get_uv_name v = case (VVUDBRel.lookup_domf brel v) of
        SOME x => (case (UV.NSet.get_first x) of
            NONE =>  raise bad_input_exp ("Vertex "^V.dest vn^" has no user" 
              ^"data attached to it.", "(vertex_user_data"^(V.dest vn))
            | SOME y => y)
       | NONE =>  raise bad_input_exp ("Vertex "^V.dest vn^" has no user" 
            ^"data attached to it.", "(vertex_user_data"^(V.dest vn))
    in
      G.update_vertex_user_data (fn _ => tab) (get_uv_name vn) g
    end;

  fun input_user_data xml g =
    let
      val _ = assert_tag "user_data" xml
      val children = get_children xml
      val graph_data = case (lookup_unique_elem "graph_data" children) of
        SOME x => input_graph_ud x
        | NONE => Symtab.empty
      val vertices_ud = map input_vertex_ud (filter_elems "vertex_data" children)
    in
      g
        |> set_graph_ud graph_data
        |> fold set_vertex_ud vertices_ud
    end;

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
                  else raise bad_input_exp ("Edge direction must be either 'true' or 'false', it cannot be: '" ^ dir_str ^ "'","edge("^(E.dest name)^").dir");
        val source = V.mk (get_attribute xml "source")
            handle bad_input_exp (s,p) => 
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val target = V.mk (get_attribute xml "target")
            handle bad_input_exp (s,p) => 
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val edata = InputEdgeData.input xml
            handle bad_input_exp (s,p) => 
              raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
      in (name,dir,edata,source,target) end;
  
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
      in (name,bangverts) end;
  
  fun add_v (vname,vdata) g = 
      snd (G.add_named_vertex vname vdata g)
  fun add_e (ename,dir,edata,src,tgt) g = 
      snd (G.add_named_edge ename 
            (if dir then G.Directed else G.UnDirected,edata) 
            src tgt g);  
  fun add_bb (bname,bvlist) g = 
      let val (realname, g') = G.new_box bname g
          val vset  = V.NSet.of_list bvlist
      in G.move_to_box realname vset g' end
  
  fun input_vertex xml = 
      let val () = assert_tag "vertex" xml;
          val name = (get_attribute xml "name")
              handle bad_input_exp (s,p) => raise bad_input_exp (s,"vertex/"^p)
      in (V.mk name, InputVertexData.input xml)
         handle bad_input_exp (s,p) => 
           raise bad_input_exp (s,"vertex("^name^")/"^p)
      end;

  fun input_graph xml =
      (let 
         val () = assert_tag "graph" xml
         val children = get_children xml
         val vertices = List.map input_vertex (filter_elems "vertex" children)
         val edges = List.map input_edge_rel (filter_elems "edge" children)
         val bboxes = List.map input_bbox (filter_elems "bangbox" children)
         (* Evaluation is delayed because we need 
          * the graph to be populated first *)
         fun parse_user_data g = (case (lookup_unique_elem "user_data" children) of
            NONE => g
            | SOME x => input_user_data x g)
       in
         G.empty 
           |> fold add_v vertices 
           |> fold add_e edges
           |> fold add_bb bboxes 
           |> parse_user_data
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"graph/"^p);
  
  val input = input_graph;
			   
end (* structure InputXML *)


(* Input some typed data. 
   The XML is the XML of the vertex or edge. i.e. it looks like this: 
     <XML_TAG_NAME ... ><type>...</type><data>...</data> ... </vertex>
   The same functor can be used for both vertices and edges: XML_TAG_NAME can 
   be vertex or edge 
*)
functor InputTypedDataXML(ComponentDataIO : ELEMENT_COMPONENT_DATA) 
: INPUT_XML = 
struct
  open XML_Parse_Utils;
  type T = XML.tree;
  type data = ComponentDataIO.data;
  fun data_to_string _ = "[data]"; (* FIXME: frop data_to_string? *)
  fun input xml =
      let 
        val children = get_children xml
        val typ = 
            apply_to_unique_elem get_unique_child_text "type" children;
        val data = get_unique_child_of_typ "data" xml;
      in ComponentDataIO.of_type_and_xml (typ,get_children data) 
         handle ComponentDataIO.unknown_type_exp _ => raise bad_input_exp 
                                                       ("Unknown type \"" ^ typ ^ "\"", "data/type")
              | bad_input_exp (s,p) => raise bad_input_exp (s,"data/"^p)
      end;
end;

(* vim:et:sts=2:sw=2
*)
