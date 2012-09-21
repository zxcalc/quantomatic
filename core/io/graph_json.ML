(**
 * JSON graph parsing and writing
 *
 * GRAPH ::=
 * {
 *   "wire_vertices": VERTEX_LIST,
 *   "node_vertices": VERTEX_LIST,
 *   "dir_edges": EDGE_LIST,
 *   "undir_edges": EDGE_LIST,
 *   "bang_boxes": BBOX_LIST,
 *   "data": GDATA,
 *   "udata": UDATA
 * }
 *
 * VERTEX_LIST ::= PLAIN_VLIST | { NAME : VDESC (, NAME : VDESC)* } | {}
 * PLAIN_VLIST ::= [ NAME (, NAME)* ] | []
 * VDESC       ::= { "data": VDATA, "udata": UDATA }
 * EDGE_LIST   ::= { NAME : EDESC (, NAME : EDESC)* }
 * EDESC       ::= { "src" : NAME, "tgt" : NAME, "data": EDATA, "udata": UDATA }
 * BBOX_LIST   ::= { NAME : BBDESC (, NAME : BBDESC)* }
 * BBDESC      ::= { "contains": PLAIN_VLIST, "parent": NAME, "data": BBDATA, "udata": UDATA }
 *
 * - the formats of VDATA, EDATA, BBDATA and GDATA depend on the theory
 * - UDATA is user data
 * - "data" and "udata" are optional everywhere
 * - in fact, everything is optional except for the "src" and "tgt" properties
 *   of EDATA
 * - a null value is the same as omitting the element
 *
 * Caveats of current implementation:
 * - UDATA is not written or parsed
 * - BBDATA and GDATA are not supported by Quantomatic at all
 *)


functor InputGraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.VData.data
  sharing type InputEdgeData.data = Graph.EData.data
)  : INPUT_JSON
= struct
  structure G = Graph;

  open JsonInputUtils;
  type data = Graph.T;

  val def_vdata = G.OVData.NVert G.OVData.IData.default_data
  val def_edata = G.EData.default_data

  fun get_vdata obj propName =
    case Json.lookup obj propName
      of SOME data => (InputVertexData.input data
                       handle InputVertexData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => def_vdata

  fun get_edata obj propName =
    case Json.lookup obj propName
      of SOME data => (InputEdgeData.input data
                       handle InputEdgeData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => def_edata

  fun get_wverts obj prop = let
    fun add_vert1 (Json.String vn) g = G.add_named_vertex (V.mk vn) G.OVData.WVert g
      | add_vert1 _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert2 (vn,_) g = G.add_named_vertex (V.mk vn) G.OVData.WVert g
  in
    fold_obj_or_arr_easy (add_vert2,add_vert1) obj prop
    handle G.vname_already_exists_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_nverts obj prop = let
    fun add_def_vert (Json.String vn) g = G.add_named_vertex (V.mk vn) def_vdata g
      | add_def_vert _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert (vn,Json.Null) g =
          G.add_named_vertex (V.mk vn) def_vdata g
      | add_vert (vn,Json.Object obj') g =
          G.add_named_vertex (V.mk vn) (get_vdata obj' "data") g
      | add_vert _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert,add_def_vert) obj prop
    handle G.vname_already_exists_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_edges dir_or_undir obj prop = let
    fun add_edge (en,Json.Object obj') g =
          (G.add_named_edge (E.mk en)
                            (dir_or_undir,(get_edata obj' "data"))
                            (V.mk (get_string obj' "src"))
                            (V.mk (get_string obj' "tgt"))
                            g
           handle G.no_such_vname_exp (_,vn,_) =>
             raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",""))
      | add_edge _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_easy add_edge obj prop
    handle G.ename_already_exists_exp (en,_) =>
             raise bad_input_exp ("Duplicate edge ("^(E.dest en)^")", prop)
  end

  fun get_bboxes obj prop = let
    fun set_bbox_parent (bn,Json.Object obj') g =
          (let
             val bp = get_string_easy "" obj' "parent"
           in
             if bp = ""
             then g
             else G.set_bbox_parent (B.mk bn) (B.mk bp) g
           end
           handle G.no_such_bbox_exp (_,b,_) =>
                    raise bad_input_exp
                      ("Bang box "^(B.dest b)^" does not exist",
                       "parent")
                | G.bbox_bad_parent_exp (b1,b2,_) =>
                    raise bad_input_exp
                      ((B.dest b1)^" is not a subgraph of "^(B.dest b2),
                       "parent"))
      | set_bbox_parent _ g = g
    fun add_bbox (bn,Json.Null) g = G.add_named_bbox (B.mk bn) g
      | add_bbox (bn,Json.Object obj') g =
          (let
             val b = B.mk bn
             fun get_vname (Json.String s) = V.mk s
               | get_vname _ = raise bad_input_exp ("Expected string","contents")
             val vs = map get_vname (get_array_easy obj' "contents")
           in
             g |> (G.add_named_bbox b)
               |> (G.doadd_to_bbox b (V.NSet.of_list vs))
           end
           handle G.no_such_vname_exp (_,vn,_) =>
             raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",
                                  "contents"))
      | add_bbox (bn,_) _ = raise bad_input_exp ("Expected object",bn)
  in
    (* we do this in two stages, so that ordering doesn't matter *)
    (fold_obj_easy set_bbox_parent obj prop)
    o
    (fold_obj_easy add_bbox obj prop)
    handle G.bbox_already_exists_exp (bn,_) =>
             raise bad_input_exp ("Duplicate bang box ("^(B.dest bn)^")", prop)
  end

  fun input (Json.Object obj) =
        G.empty |> get_wverts obj "wire_vertices"
                |> get_nverts obj "node_vertices"
                |> get_edges G.UnDirected obj "undir_edges"
                |> get_edges G.Directed obj "dir_edges"
                |> get_bboxes obj "bang_boxes"
    | input Json.Null = G.empty
    | input _ = raise bad_input_exp ("Expected object","")
end;

functor OutputGraphJSON(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type OutputVertexData.data = Graph.VData.data
  sharing type OutputEdgeData.data = Graph.EData.data
)  : OUTPUT_JSON
= struct
  structure G = Graph;

  open JsonOutputUtils;
  type data = Graph.T;

  fun get_verts g = let
    fun vert_info vd =
      Json.Object (
        Json.empty_obj |> Json.update ("data",OutputVertexData.output vd)
      )
    fun add_vert (v,(G.OVData.WVert,_)) (Json.Array wvs,nvj) =
          (Json.Array ((Json.String (V.dest v))::wvs),nvj)
      | add_vert (v,(vd,_)) (wvj,Json.Object nvobj) =
          (wvj,Json.Object (Json.update (V.dest v,vert_info vd) nvobj))
  in
    V.NTab.fold add_vert
                (G.get_vertices g)
                (Json.Array [],Json.Object Json.empty_obj)
  end

  fun get_edges g = let
    fun edge_info ed s t =
      Json.Object (
        Json.empty_obj |> Json.update ("src",Json.String (V.dest s))
                       |> Json.update ("tgt",Json.String (V.dest t))
                       |> Json.update ("data",OutputEdgeData.output ed)
      )
    fun add_edge (e,((G.Directed,ed),(s,t))) (Json.Object dobj,udj) =
          (Json.Object (Json.update (E.dest e,edge_info ed s t) dobj),udj)
      | add_edge (e,((G.UnDirected,ed),(s,t))) (dj,Json.Object udobj) =
          (dj,Json.Object (Json.update (E.dest e,edge_info ed s t) udobj))
  in
    E.NTab.fold add_edge
                (G.get_edges g)
                (Json.Object Json.empty_obj,Json.Object Json.empty_obj)
  end

  fun get_bboxes g = let
    fun add_bbox b = let
        val add_parent =
          case G.lookup_bbox_parent g b
            of SOME bp => Json.update ("parent",Json.String (B.dest bp))
             | NONE => I
        val jvs = map (fn v => Json.String (V.dest v))
                      (V.NSet.list_of (G.get_bbox_contents g b))
        val obj = Json.empty_obj |> add_parent
                                 |> Json.update ("contents",Json.Array jvs)
      in
        Json.update (B.dest b, Json.Object obj)
      end
  in
    Json.Object (B.NSet.fold add_bbox (G.get_bboxes g) Json.empty_obj)
  end

  fun output g = let
      val (wverts,nverts) = get_verts g
      val (dedges,udedges) = get_edges g
      val bboxes = get_bboxes g
      fun add (_,Json.Array []) = I
        | add (v as (_,Json.Object obj)) =
            (case Json.properties obj
               of [] => I
               | _ => Json.update v)
        | add v = Json.update v
    in
      Json.Object
      (Json.empty_obj |> add ("wire_vertices",wverts)
                      |> add ("node_vertices",nverts)
                      |> add ("undir_edges",udedges)
                      |> add ("dir_edges",dedges)
                      |> add ("bang_boxes",bboxes))
    end
end;

