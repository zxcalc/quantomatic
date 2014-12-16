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
 *   "annotation": ANNDATA
 * }
 * VERTEX_LIST ::= PLAIN_VLIST | { NAME : VDESC (, NAME : VDESC)* } | {}
 * PLAIN_VLIST ::= [ NAME (, NAME)* ] | []
 * EDGE_LIST   ::= { NAME : EDESC (, NAME : EDESC)* } | {}
 * BBOX_LIST   ::= { NAME : BBDESC (, NAME : BBDESC)* } | {}
 * VDESC ::=
 * {
 *   "data": VDATA,
 *   "annotation": ANNDATA
 * }
 * EDESC ::=
 * {
 *   "src" : NAME,
 *   "tgt" : NAME,
 *   "data": EDATA,
 *   "annotation": ANNDATA
 * }
 * BBDESC ::=
 * {
 *   "contents": PLAIN_VLIST,
 *   "parent": NAME,
 *   "data": BBDATA,
 *   "annotation": ANNDATA
 * }
 *
 * - the formats of VDATA, EDATA, BBDATA and GDATA depend on the theory
 * - ANNDATA is some sort of annotation, in a format to be decided by the tool;
 *   the controller uses it to store user data
 * - "data" and "annotation" are optional everywhere
 * - in fact, everything is optional except for the "src" and "tgt" properties
 *   of EDATA
 * - a null value is the same as omitting the element
 *
 * output_vertex, output_edge and output_bbox produce the following outputs:
 * VERTEX ::=
 * {
 *   "name": NAME,
 *   "is_wire_vertex": true|false,
 *   "data": VDATA,
 *   "annotation": ANNDATA
 * }
 * EDGE ::=
 * {
 *   "name": NAME,
 *   "is_directed": true|false,
 *   "src" : NAME,
 *   "tgt" : NAME,
 *   "data": EDATA,
 *   "annotation": ANNDATA
 * }
 * BANGBOX ::=
 * {
 *   "name": NAME,
 *   "contents": PLAIN_VLIST,
 *   "parent": NAME,
 *   "data": BBDATA,
 *   "annotation": ANNDATA
 * }
 *
 * Caveats of current implementation:
 * - BBDATA and GDATA are not supported by Quantomatic at all
 *
 * Note about arguments to functors:
 * Json.Null is treated as the "default" value.  If Output*Data.output returns
 * Json.Null, nothing is written, and the default vertex or edge data will be
 * used when reading the file again.  Input*Data.input will never be passed
 * Json.Null, as this is assumed to mean the default vertex or edge data.
 *
 * Similarly, annotations are not written at all if their values are null.
 * However, OutputAnnotation is notified of both null and missing annotations by
 * being passed a Json.Null value (this allows the annotation type to maintain a
 * record of all graph components.
 *)

signature GRAPH_ANN_INPUT_JSON
= sig
  exception bad_input_exp of string * string
  type data
  val empty_annotation : data
  val input_vertex_annotation : V.name -> Json.json -> data -> data
  val input_edge_annotation : E.name -> Json.json -> data -> data
  val input_bbox_annotation : B.name -> Json.json -> data -> data
  val input_graph_annotation : Json.json -> data -> data
end

structure EmptyGraphAnnotationInputJson
  : GRAPH_ANN_INPUT_JSON where type data = unit
= struct
  exception bad_input_exp of string * string
  type data = unit
  val empty_annotation = ()
  fun input_vertex_annotation _ _ = I
  fun input_edge_annotation _ _ = I
  fun input_bbox_annotation _ _ = I
  fun input_graph_annotation _ = I
end

signature GRAPH_ANN_OUTPUT_JSON
= sig
  type data
  val empty_annotation : data
  val output_vertex_annotation : data -> V.name -> Json.json
  val output_edge_annotation : data -> E.name -> Json.json
  val output_bbox_annotation : data -> B.name -> Json.json
  val output_graph_annotation : data -> Json.json
end

structure EmptyGraphAnnotationOutputJson
  : GRAPH_ANN_OUTPUT_JSON where type data = unit
= struct
  type data = unit
  val empty_annotation = ()
  fun output_vertex_annotation _ _ = Json.Null
  fun output_edge_annotation _ _ = Json.Null
  fun output_bbox_annotation _ _ = Json.Null
  fun output_graph_annotation _ = Json.Null
end


functor InputAnnotatedGraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.nvdata
  sharing type InputEdgeData.data = Graph.edata
  structure InputAnnotation : GRAPH_ANN_INPUT_JSON
)  : INPUT_JSON
= struct
  structure Graph = Graph
  structure IA = InputAnnotation

  open JsonInputUtils
  type data = Graph.T * InputAnnotation.data

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
    fun add_vert1 (Json.String vn) (g,ann) =
          (Graph.add_named_vertex (V.mk vn) Graph.WVert g,
           IA.input_vertex_annotation (V.mk vn) Json.Null ann)
      | add_vert1 _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert2 (vn,Json.Null) (g,ann) = add_vert1 (Json.String vn) (g,ann)
      | add_vert2 (vn,Json.Object obj') (g,ann) =
          (Graph.add_named_vertex (V.mk vn) Graph.WVert g,
           IA.input_vertex_annotation (V.mk vn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_vert2 _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert2,add_vert1) obj prop
    handle Graph.duplicate_vertex_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_nverts obj prop = let
    fun add_def_vert (Json.String vn) (g,ann) =
          (Graph.add_named_vertex (V.mk vn) Graph.default_nv_vdata g,
           IA.input_vertex_annotation (V.mk vn) Json.Null ann)
      | add_def_vert _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert (vn,Json.Null) (g,ann) = add_def_vert (Json.String vn) (g,ann)
      | add_vert (vn,Json.Object obj') (g,ann) =
          (Graph.add_named_vertex (V.mk vn) (get_nv_vdata obj' "data") g,
           IA.input_vertex_annotation (V.mk vn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_vert _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert,add_def_vert) obj prop
    handle Graph.duplicate_vertex_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_edges dir_or_undir obj prop = let
    fun add_edge (en,Json.Object obj') (g,ann) =
          (Graph.add_named_edge (E.mk en)
                            (dir_or_undir,(get_edata obj' "data"))
                            (V.mk (get_string obj' "src"))
                            (V.mk (get_string obj' "tgt"))
                            g
             handle Graph.no_such_vertex_exp (_,vn,_) =>
               raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",""),
           IA.input_edge_annotation (E.mk en) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_edge _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_easy add_edge obj prop
    handle Graph.duplicate_edge_exp (en,_) =>
             raise bad_input_exp ("Duplicate edge ("^(E.dest en)^")", prop)
  end

  fun get_bboxes obj prop = let
    fun set_bbox_parent (bn,Json.Object obj') (g,ann) =
          (let
             val bp = get_string_easy "" obj' "parent"
           in
             if bp = ""
             then (g,ann)
             else (g |> Graph.add_child_to_bbox (B.mk bp) (B.mk bn),ann)
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
    fun add_bbox (bn,Json.Null) (g,ann) =
          (Graph.add_named_bbox (B.mk bn) g,
           IA.input_bbox_annotation (B.mk bn) Json.Null ann)
      | add_bbox (bn,Json.Object obj') (g,ann) =
          (let
             val b = B.mk bn
             fun get_vname (Json.String s) = V.mk s
               | get_vname _ = raise bad_input_exp ("Expected string","contents")
             val vs = map get_vname (get_array_easy obj' "contents")
           in
             g |> (Graph.add_named_bbox b)
               |> (Graph.add_to_bbox_anon b (V.NSet.of_list vs))
           end
           handle Graph.no_such_vertex_exp (_,vn,_) =>
             raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",
                                  "contents"),
           IA.input_bbox_annotation (B.mk bn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_bbox (bn,_) _ = raise bad_input_exp ("Expected object",bn)
  in
    (* we do this in two stages, so that ordering doesn't matter *)
    (fold_obj_easy set_bbox_parent obj prop)
    o
    (fold_obj_easy add_bbox obj prop)
    handle Graph.duplicate_bbox_exp (bn,_) =>
             raise bad_input_exp ("Duplicate bang box ("^(B.dest bn)^")", prop)
  end

  fun get_ann obj prop (g,ann) =
        (g, IA.input_graph_annotation (get_easy Json.Null obj prop) ann)
        handle IA.bad_input_exp (m,l) =>
          raise bad_input_exp (m,prepend_prop prop l)

  fun input (Json.Object obj) =
        (Graph.empty, IA.empty_annotation)
            |> get_wverts obj "wire_vertices"
            |> get_nverts obj "node_vertices"
            |> get_edges Undirected obj "undir_edges"
            |> get_edges Directed obj "dir_edges"
            |> get_bboxes obj "bang_boxes"
            |> get_ann obj "annotation"
    | input Json.Null = (Graph.empty,IA.empty_annotation)
    | input _ = raise bad_input_exp ("Expected object","")
end

functor InputGraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.nvdata
  sharing type InputEdgeData.data = Graph.edata
)  : INPUT_JSON
= struct
  structure SubInput = InputAnnotatedGraphJSON(
    structure Graph = Graph
    structure InputVertexData = InputVertexData
    structure InputEdgeData = InputEdgeData
    structure InputAnnotation = EmptyGraphAnnotationInputJson
  )
  open SubInput
  type data = Graph.T
  val input = fst o SubInput.input
end

signature OUTPUT_GRAPH_JSON
= sig
  include OUTPUT_JSON
  val output_vertex : data -> V.name -> T
  val output_edge : data -> E.name -> T
  val output_bbox : data -> B.name -> T
end

functor OutputAnnotatedGraphJSON(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type OutputVertexData.data = Graph.nvdata
  sharing type OutputEdgeData.data = Graph.edata
  structure OutputAnnotation : GRAPH_ANN_OUTPUT_JSON
)  : OUTPUT_GRAPH_JSON
= struct
  structure G = Graph
  structure OA = OutputAnnotation

  open JsonOutputUtils
  type data = Graph.T * OutputAnnotation.data

  fun output_vertex (g,ann) v = let
    val (is_wv,vd) =
      case Graph.get_vertex_data g v
        of Graph.WVert => (true,Json.Null)
         | Graph.NVert d => (false,OutputVertexData.output d)
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (V.dest v))
                     |> update ("is_wire_vertex",Json.Bool is_wv)
                     |> update ("data",vd)
                     |> update ("annotation",OA.output_vertex_annotation ann v)
    )
  end

  fun output_edge (g,ann) e = let
    val ((dir,ed),(s,t)) = (Graph.get_edge_dir_and_data g e, (Graph.get_edge_source g e, Graph.get_edge_target g e))
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (E.dest e))
                     |> update ("is_directed",Json.Bool (dir = Directed))
                     |> update ("src",Json.String (V.dest s))
                     |> update ("tgt",Json.String (V.dest t))
                     |> update ("data",OutputEdgeData.output ed)
                     |> update ("annotation",OA.output_edge_annotation ann e)
    )
  end

  fun output_bbox (g,ann) b = let
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
                     |> update ("annotation",OA.output_bbox_annotation ann b)
    )
  end

  fun get_verts (g,ann) = let
    fun nv_info v vd =
      Json.Object (
        Json.empty_obj |> update ("data",OutputVertexData.output vd)
                       |> update ("annotation",OA.output_vertex_annotation ann v)
      )
    fun wv_info v =
      Json.Object (
        Json.empty_obj |> update ("annotation",OA.output_vertex_annotation ann v)
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

  fun get_edges (g,ann) = let
    fun edge_info e ed s t =
      Json.Object (
        Json.empty_obj |> update ("src",Json.String (V.dest s))
                       |> update ("tgt",Json.String (V.dest t))
                       |> update ("data",OutputEdgeData.output ed)
                       |> update ("annotation",OA.output_edge_annotation ann e)
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

  fun get_bboxes (g,ann) = let
    fun add_bbox b = let
        val add_parent =
          case B.NSet.get_local_bot (Graph.get_bbox_parents g b) (* TODO: handle multiple parents *)
            of SOME bp => update ("parent",Json.String (B.dest bp))
             | NONE => I
        val jvs = map (fn v => Json.String (V.dest v))
                      (V.NSet.list_of (Graph.get_vertices_in_bbox g b))
        val obj = Json.empty_obj |> add_parent
                                 |> update ("contents",Json.Array jvs)
                                 |> update ("annotation",OA.output_bbox_annotation ann b)
      in
        update (B.dest b, Json.Object obj)
      end
  in
    Json.Object (B.NSet.fold add_bbox (Graph.get_bboxes g) Json.empty_obj)
  end

  fun output (g,ann) = let
    val (wverts,nverts) = get_verts (g,ann)
    val (dedges,udedges) = get_edges (g,ann)
    val bboxes = get_bboxes (g,ann)
  in
    Json.Object
    (Json.empty_obj |> opt_update ("wire_vertices",wverts)
                    |> opt_update ("node_vertices",nverts)
                    |> opt_update ("undir_edges",udedges)
                    |> opt_update ("dir_edges",dedges)
                    |> opt_update ("bang_boxes",bboxes)
                    |> update ("annotation",OA.output_graph_annotation ann))
  end
end

functor OutputGraphJSON(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type OutputVertexData.data = Graph.nvdata
  sharing type OutputEdgeData.data = Graph.edata
)  : OUTPUT_GRAPH_JSON
= struct
  structure SubOutput = OutputAnnotatedGraphJSON(
    structure Graph = Graph
    structure OutputVertexData = OutputVertexData
    structure OutputEdgeData = OutputEdgeData
    structure OutputAnnotation = EmptyGraphAnnotationOutputJson
  )
  open SubOutput
  type data = Graph.T
  fun output g = SubOutput.output (g,())
  fun output_vertex g = SubOutput.output_vertex (g,())
  fun output_edge g = SubOutput.output_edge (g,())
  fun output_bbox g = SubOutput.output_bbox (g,())
end

