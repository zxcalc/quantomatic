(* The original format for Quanto's graphs *)
structure Input_RG_Graph_XML_v1 : INPUT_XML
= struct
  open XML_Parse_Utils

  structure Graph = RG_Theory.Graph

  type T = XML.tree
  type data = Graph.T

  fun input_vertex_idata xml =
      let val children = get_children xml
          val col = (String.map Char.toLower o get_unique_child_text
                     o get_unique_elem "colour") children
      in
        if (col = "h") then RG_Data.Hnd
        else
          let
            val angle =
                case lookup_unique_elem "angleexpr" children of NONE => LinratAngleExpr.zero
                   | SOME a => InputLinratXML.input a
          in
            if (col = "green") then RG_Data.Znd angle
            else if (col = "red") then RG_Data.Xnd angle
            else raise bad_input_exp ("Unknown colour : "^col^".","")
          end
      end

  fun input_vertex xml =
      let
        val children = get_children xml
        val name = V.mk (apply_to_unique_elem get_unique_child_text "name" children)
                     handle bad_input_exp (s,p) => raise bad_input_exp (s,"vertex/"^p)
      in
        (name, case lookup_unique_elem "boundary" children
                 of NONE => (Graph.NVert (input_vertex_idata xml))
                  | SOME boundary_xml =>
                      if get_boolean (get_unique_child boundary_xml) then Graph.WVert
                      else (Graph.NVert (input_vertex_idata xml)))
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"vertex("^(V.dest name)^")/"^p)
      end

  fun input_edge_rel xml =
      let
        val _ = assert_tag "edge" xml
        val children = get_children xml
        val name = E.mk (apply_to_unique_elem get_unique_child_text "name" children)
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"edge/"^p)
        val source = V.mk (apply_to_unique_elem get_unique_child_text "source" children)
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
        val target = V.mk (apply_to_unique_elem get_unique_child_text "target" children)
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"edge("^(E.dest name)^")/"^p)
      in (name,source,target) end

  fun input_bbox xml =
      let
        val _ = assert_tag "bangbox" xml
        val children = get_children xml
        val name = B.mk (apply_to_unique_elem get_unique_child_text "name" children)
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"bangbox/"^p)
        val bangverts = List.map (V.mk o get_unique_child_text)
                                 (filter_elems "boxedvertex" children)
          handle bad_input_exp (s,p) => raise bad_input_exp (s,"bangbox("^(B.dest name)^")/"^p)
      in (name,bangverts) end

  (* uncurry some graph methods *)
  fun add_v (vname,vdata) g = Graph.add_named_vertex vname vdata g

  fun add_e (ename,src,tgt) g =
      Graph.add_named_edge ename (Undirected,()) src tgt g

  fun add_bb (bname,bvlist) g =
      let val g' = Graph.add_named_bbox bname g
          val vset  = V.NSet.of_list bvlist
      in
        Graph.add_to_bbox_anon bname vset g'
      end

  fun input_graph xml =
      (let
         val () = assert_tag "graph" xml
         val children = get_children xml
         val vertices = List.map input_vertex (filter_elems "vertex" children)
         val edges = List.map input_edge_rel (filter_elems "edge" children)
         val bboxes = List.map input_bbox (filter_elems "bangbox" children)
       in
         Graph.empty
           |> fold add_v vertices
           |> fold add_e edges
           |> fold add_bb bboxes
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"graph/"^p)

  val input = input_graph
end

(* vim:et:sts=2:sw=2
*)
