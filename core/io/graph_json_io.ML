signature GRAPH_JSON =
sig
  structure Graph : BANG_GRAPH
  val input : Json.json -> Graph.T
end

functor GraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.nvdata
  sharing type InputEdgeData.data = Graph.edata
)  : GRAPH_JSON
= struct
  structure Graph = Graph

  open JsonInputUtils

  fun get_nv_vdata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => Graph.default_nv_vdata
       | SOME data => (Graph.NVert (InputVertexData.input data)
                       handle InputVertexData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => Graph.default_nv_vdata

  fun get_edata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => Graph.default_edata
       | SOME data => (InputEdgeData.input data
                       handle InputEdgeData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => Graph.default_edata

  fun get_wverts obj prop = let
    fun add_vert1 (Json.String vn) g =
          Graph.add_named_vertex (V.mk vn) Graph.WVert g
      | add_vert1 _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert2 (vn,Json.Null) g = add_vert1 (Json.String vn) g
      | add_vert2 (vn,Json.Object obj') g =
          Graph.add_named_vertex (V.mk vn) Graph.WVert g
      | add_vert2 _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert2,add_vert1) obj prop
    handle Graph.duplicate_vertex_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_nverts obj prop = let
    fun add_def_vert (Json.String vn) g =
          Graph.add_named_vertex (V.mk vn) Graph.default_nv_vdata g
      | add_def_vert _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert (vn,Json.Null) g = add_def_vert (Json.String vn) g
      | add_vert (vn,Json.Object obj') g =
          g |> Graph.add_named_vertex (V.mk vn) (get_nv_vdata obj' "data")
            |> (case Json.lookup obj' "annotation"
                  of SOME (Json.Object ann) => Graph.set_vertex_annotation (V.mk vn, ann)
                   | SOME Json.Null => I
                   | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                   | NONE => I)
      | add_vert _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert,add_def_vert) obj prop
    handle Graph.duplicate_vertex_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_edges dir_or_undir obj prop = let
    fun add_edge (en,Json.Object obj') g =
         (g |> Graph.add_named_edge (E.mk en)
                 (dir_or_undir,(get_edata obj' "data"))
                 (V.mk (get_string obj' "src"))
                 (V.mk (get_string obj' "tgt"))
            |> (case Json.lookup obj' "annotation"
                  of SOME (Json.Object ann) => Graph.set_edge_annotation (E.mk en, ann)
                   | SOME Json.Null => I
                   | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                   | NONE => I)
          handle Graph.no_such_vertex_exp (_,vn,_) =>
            raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",""))
      | add_edge _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_easy add_edge obj prop
    handle Graph.duplicate_edge_exp (en,_) =>
             raise bad_input_exp ("Duplicate edge ("^(E.dest en)^")", prop)
  end

  fun get_bboxes obj prop = let
    fun set_bbox_parent (bn,Json.Object obj') g =
          (let
             val bp = get_string_easy "" obj' "parent"
           in
             if bp = ""
             then g
             else g |> Graph.add_child_to_bbox (B.mk bp) (B.mk bn)
           end
           handle Graph.no_such_bbox_exp (_,b,_) =>
                    raise bad_input_exp
                      ("Bang box "^(B.dest b)^" does not exist",
                       "parent")
                | Graph.bbox_bad_parent_exp (b1,b2,_) =>
                    raise bad_input_exp
                      ((B.dest b1)^" is not a subgraph of "^(B.dest b2),
                       "parent"))
      | set_bbox_parent _ g = g
    fun add_bbox (bn,Json.Null) g =
          Graph.add_named_bbox (B.mk bn) g
      | add_bbox (bn,Json.Object obj') g =
          (let
             val b = B.mk bn
             fun get_vname (Json.String s) = V.mk s
               | get_vname _ = raise bad_input_exp ("Expected string","contents")
             val vs = map get_vname (get_array_easy obj' "contents")
           in
             g |> (Graph.add_named_bbox b)
               |> (Graph.add_to_bbox_anon b (V.NSet.of_list vs))
               |> (case Json.lookup obj' "annotation"
                     of SOME (Json.Object ann) => Graph.set_bbox_annotation (B.mk bn, ann)
                      | SOME Json.Null => I
                      | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                      | NONE => I)
           end
           handle Graph.no_such_vertex_exp (_,vn,_) =>
             raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",
                                  "contents"))
      | add_bbox (bn,_) _ = raise bad_input_exp ("Expected object",bn)
  in
    (* we do this in two stages, so that ordering doesn't matter *)
    (fold_obj_easy set_bbox_parent obj prop)
    o
    (fold_obj_easy add_bbox obj prop)
    handle Graph.duplicate_bbox_exp (bn,_) =>
             raise bad_input_exp ("Duplicate bang box ("^(B.dest bn)^")", prop)
  end

  (*fun get_ann obj prop (g,ann) =
        (g, IA.input_graph_annotation (get_easy Json.Null obj prop) ann)
        handle IA.bad_input_exp (m,l) =>
          raise bad_input_exp (m,prepend_prop prop l)*)

  fun input (Json.Object obj) =
        Graph.empty
            |> get_wverts obj "wire_vertices"
            |> get_nverts obj "node_vertices"
            |> get_edges Undirected obj "undir_edges"
            |> get_edges Directed obj "dir_edges"
            |> get_bboxes obj "bang_boxes"
            |> (case Json.lookup obj "annotation"
                  of SOME (Json.Object ann) => Graph.set_graph_annotation ann
                   | SOME Json.Null => I
                   | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                   | NONE => I)
    | input Json.Null = Graph.empty
    | input _ = raise bad_input_exp ("Expected object","")
end