signature GRAPH_JSON =
sig
  structure Graph : BANG_GRAPH
  val input : Json.json -> Graph.T
  val output : Graph.T -> Json.json
  val output_vertex_desc : Graph.T -> V.name -> Json.json
  val output_edge_desc : Graph.T -> E.name -> Json.json
  val output_bbox_desc : Graph.T -> B.name -> Json.json
end

functor GraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type InputVertexData.data = OutputVertexData.data = Graph.nvdata
  sharing type InputEdgeData.data = OutputEdgeData.data = Graph.edata
) : GRAPH_JSON
= struct
  structure Graph = Graph

  open JsonInputUtils
  open JsonOutputUtils

  (*************************************)
  (************** Input ****************)
  (*************************************)

  fun input_nv_vdata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => Graph.default_nv_vdata
       | SOME data => (Graph.NVert (InputVertexData.input data)
                       handle InputVertexData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => Graph.default_nv_vdata

  fun input_edata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => Graph.default_edata
       | SOME data => (InputEdgeData.input data
                       handle InputEdgeData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => Graph.default_edata

  fun input_wverts obj prop = let
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

  fun input_nverts obj prop = let
    fun add_def_vert (Json.String vn) g =
          Graph.add_named_vertex (V.mk vn) Graph.default_nv_vdata g
      | add_def_vert _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert (vn,Json.Null) g = add_def_vert (Json.String vn) g
      | add_vert (vn,Json.Object obj') g =
          g |> Graph.add_named_vertex (V.mk vn) (input_nv_vdata obj' "data")
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

  fun input_edges dir_or_undir obj prop = let
    fun add_edge (en,Json.Object obj') g =
         (g |> Graph.add_named_edge (E.mk en)
                 (dir_or_undir,(input_edata obj' "data"))
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

  fun input_bboxes obj prop = let
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
            |> input_wverts obj "wire_vertices"
            |> input_nverts obj "node_vertices"
            |> input_edges Undirected obj "undir_edges"
            |> input_edges Directed obj "dir_edges"
            |> input_bboxes obj "bang_boxes"
            |> (case Json.lookup obj "annotation"
                  of SOME (Json.Object ann) => Graph.set_graph_annotation ann
                   | SOME Json.Null => I
                   | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                   | NONE => I)
    | input Json.Null = Graph.empty
    | input _ = raise bad_input_exp ("Expected object","")


  (*************************************)
  (************** Output ***************)
  (*************************************)

  fun output_vertex_desc g v = let
    val (is_wv,vd) =
      case Graph.get_vertex_data g v
        of Graph.WVert => (true,Json.Null)
         | Graph.NVert d => (false,OutputVertexData.output d)
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (V.dest v))
                     |> update ("is_wire_vertex",Json.Bool is_wv)
                     |> update ("data",vd)
                     |> (case Graph.get_vertex_annotation_opt g v
                           of SOME ann => update ("annotation",Json.Object ann)
                            | NONE => I)
    )
  end

  fun output_edge_desc g e = let
    val ((dir,ed),(s,t)) = (Graph.get_edge_dir_and_data g e, (Graph.get_edge_source g e, Graph.get_edge_target g e))
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (E.dest e))
                     |> update ("is_directed",Json.Bool (dir = Directed))
                     |> update ("src",Json.String (V.dest s))
                     |> update ("tgt",Json.String (V.dest t))
                     |> update ("data",OutputEdgeData.output ed)
                     |> (case Graph.get_edge_annotation_opt g e
                           of SOME ann => update ("annotation",Json.Object ann)
                            | NONE => I)
    )
  end

  fun output_bbox_desc g b = let
    val add_parent =
      case B.NSet.get_local_bot (Graph.get_bbox_parents g b) (* TODO: handle multiple parents *)
        of SOME bp => update ("parent",Json.String (B.dest bp))
         | NONE => I
    val jvs = map (fn v => Json.String (V.dest v))
                  (V.NSet.list_of (Graph.get_vertices_in_bbox g b))
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (B.dest b))
                     |> add_parent
                     |> update ("contents",Json.Array jvs)
                     |> (case Graph.get_bbox_annotation_opt g b
                           of SOME ann => update ("annotation",Json.Object ann)
                            | NONE => I)
    )
  end

  fun get_verts g = let
    fun nv_info v vd =
      Json.Object (
        Json.empty_obj |> update ("data",OutputVertexData.output vd)
                       |> (case Graph.get_vertex_annotation_opt g v
                             of SOME ann => update ("annotation",Json.Object ann)
                              | NONE => I)
      )
    fun wv_info v =
      Json.Object (
        Json.empty_obj |> (case Graph.get_vertex_annotation_opt g v
                             of SOME ann => update ("annotation",Json.Object ann)
                              | NONE => I)
      )
    fun add_vert v (wvobj,nvobj) =
      case Graph.get_vertex_data g v
        of Graph.NVert vd => (wvobj, update (V.dest v, nv_info v vd) nvobj)
         | Graph.WVert => (update (V.dest v,wv_info v) wvobj, nvobj)
    val (wvobj,nvobj) = V.NSet.fold add_vert
                                    (Graph.get_vertices g)
                                    (Json.empty_obj,Json.empty_obj)
  in
    (Json.Object wvobj,Json.Object nvobj)
  end

  fun get_edges g = let
    fun edge_info e ed s t =
      Json.Object (
        Json.empty_obj |> update ("src",Json.String (V.dest s))
                       |> update ("tgt",Json.String (V.dest t))
                       |> update ("data",OutputEdgeData.output ed)
                       |> (case Graph.get_edge_annotation_opt g e
                             of SOME ann => update ("annotation",Json.Object ann)
                              | NONE => I)
      )
    fun add_edge e (dobj,udobj) = let
      val (s,t) = (Graph.get_edge_source g e, Graph.get_edge_target g e)
      val (edir, ed) = Graph.get_edge_dir_and_data g e
    in case edir of Directed => (update (E.dest e,edge_info e ed s t) dobj, udobj)
                  | Undirected => (dobj,update (E.dest e,edge_info e ed s t) udobj)
    end
    val (dobj,udobj) = E.NSet.fold add_edge
                                   (Graph.get_edges g)
                                   (Json.empty_obj,Json.empty_obj)
  in
    (Json.Object dobj,Json.Object udobj)
  end

  fun get_bboxes g = let
    fun add_bbox b = let
        val add_parent =
          case B.NSet.get_local_bot (Graph.get_bbox_parents g b) (* TODO: handle multiple parents *)
            of SOME bp => update ("parent",Json.String (B.dest bp))
             | NONE => I
        val jvs = map (fn v => Json.String (V.dest v))
                      (V.NSet.list_of (Graph.get_vertices_in_bbox g b))
        val obj = Json.empty_obj |> add_parent
                                 |> update ("contents",Json.Array jvs)
                                 |> (case Graph.get_bbox_annotation_opt g b
                                       of SOME ann => update ("annotation",Json.Object ann)
                                        | NONE => I)
      in
        update (B.dest b, Json.Object obj)
      end
  in
    Json.Object (B.NSet.fold add_bbox (Graph.get_bboxes g) Json.empty_obj)
  end

  fun output g = let
    val (wverts,nverts) = get_verts g
    val (dedges,udedges) = get_edges g
    val bboxes = get_bboxes g
  in
    Json.Object
    (Json.empty_obj |> opt_update ("wire_vertices",wverts)
                    |> opt_update ("node_vertices",nverts)
                    |> opt_update ("undir_edges",udedges)
                    |> opt_update ("dir_edges",dedges)
                    |> opt_update ("bang_boxes",bboxes)
                    |> (if Json.is_empty_obj (Graph.get_graph_annotation g) then I
                        else update ("annotation",Json.Object (Graph.get_graph_annotation g)))
    )
  end
end