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
 *   "contains": PLAIN_VLIST,
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
  exception bad_input_exp of string*string;
  type data;
  val empty_annotation : data;
  val input_vertex_annotation : V.name -> Json.json -> data -> data;
  val input_edge_annotation : E.name -> Json.json -> data -> data;
  val input_bangbox_annotation : B.name -> Json.json -> data -> data;
  val input_graph_annotation : Json.json -> data -> data;
end;

structure EmptyGraphAnnotationInputJson
  : GRAPH_ANN_INPUT_JSON where type data = unit
= struct
  exception bad_input_exp of string*string;
  type data = unit;
  val empty_annotation = ();
  fun input_vertex_annotation _ _ = I;
  fun input_edge_annotation _ _ = I;
  fun input_bangbox_annotation _ _ = I;
  fun input_graph_annotation _ = I;
end;

signature GRAPH_ANN_OUTPUT_JSON
= sig
  type data;
  val output_vertex_annotation : data -> V.name -> Json.json;
  val output_edge_annotation : data -> E.name -> Json.json;
  val output_bangbox_annotation : data -> B.name -> Json.json;
  val output_graph_annotation : data -> Json.json;
end;

structure EmptyGraphAnnotationOutputJson
  : GRAPH_ANN_OUTPUT_JSON where type data = unit
= struct
  type data = unit;
  fun output_vertex_annotation _ _ = Json.Null;
  fun output_edge_annotation _ _ = Json.Null;
  fun output_bangbox_annotation _ _ = Json.Null;
  fun output_graph_annotation _ = Json.Null;
end;


functor InputAnnotatedGraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.OVData.IData.data
  sharing type InputEdgeData.data = Graph.EData.data
  structure InputAnnotation : GRAPH_ANN_INPUT_JSON
)  : INPUT_JSON
= struct
  structure G = Graph;
  structure IA = InputAnnotation;

  open JsonInputUtils;
  type data = Graph.T * InputAnnotation.data;

  val def_vdata = G.OVData.NVert G.OVData.IData.default_data
  val def_edata = G.EData.default_data

  fun get_vdata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => def_vdata
       | SOME data => (G.OVData.NVert (InputVertexData.input data)
                       handle InputVertexData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => def_vdata

  fun get_edata obj propName =
    case Json.lookup obj propName
      of SOME Json.Null => def_edata
       | SOME data => (InputEdgeData.input data
                       handle InputEdgeData.bad_input_exp (m,l) =>
                         raise bad_input_exp (m,prepend_prop propName l))
       | NONE => def_edata

  fun get_wverts obj prop = let
    fun add_vert1 (Json.String vn) (g,ann) =
          (G.add_named_vertex (V.mk vn) G.OVData.WVert g,
           IA.input_vertex_annotation (V.mk vn) Json.Null ann)
      | add_vert1 _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert2 (vn,Json.Null) (g,ann) = add_vert1 (Json.String vn) (g,ann)
      | add_vert2 (vn,Json.Object obj') (g,ann) =
          (G.add_named_vertex (V.mk vn) G.OVData.WVert g,
           IA.input_vertex_annotation (V.mk vn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_vert2 _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert2,add_vert1) obj prop
    handle G.vname_already_exists_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_nverts obj prop = let
    fun add_def_vert (Json.String vn) (g,ann) =
          (G.add_named_vertex (V.mk vn) def_vdata g,
           IA.input_vertex_annotation (V.mk vn) Json.Null ann)
      | add_def_vert _ _ = raise bad_input_exp ("Expected string","")
    fun add_vert (vn,Json.Null) (g,ann) = add_def_vert (Json.String vn) (g,ann)
      | add_vert (vn,Json.Object obj') (g,ann) =
          (G.add_named_vertex (V.mk vn) (get_vdata obj' "data") g,
           IA.input_vertex_annotation (V.mk vn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_vert _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_or_arr_easy (add_vert,add_def_vert) obj prop
    handle G.vname_already_exists_exp (vn,_) =>
             raise bad_input_exp ("Duplicate vertex ("^(V.dest vn)^")", prop)
  end

  fun get_edges dir_or_undir obj prop = let
    fun add_edge (en,Json.Object obj') (g,ann) =
          (G.add_named_edge (E.mk en)
                            (dir_or_undir,(get_edata obj' "data"))
                            (V.mk (get_string obj' "src"))
                            (V.mk (get_string obj' "tgt"))
                            g
             handle G.no_such_vname_exp (_,vn,_) =>
               raise bad_input_exp ("Vertex "^(V.dest vn)^" does not exist",""),
           IA.input_edge_annotation (E.mk en) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_edge _ _ = raise bad_input_exp ("Expected object","")
  in
    fold_obj_easy add_edge obj prop
    handle G.ename_already_exists_exp (en,_) =>
             raise bad_input_exp ("Duplicate edge ("^(E.dest en)^")", prop)
  end

  fun get_bboxes obj prop = let
    fun set_bbox_parent (bn,Json.Object obj') (g,ann) =
          (let
             val bp = get_string_easy "" obj' "parent"
           in
             if bp = ""
             then (g,ann)
             else (G.set_bbox_parent (B.mk bn) (B.mk bp) g,ann)
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
    fun add_bbox (bn,Json.Null) (g,ann) =
          (G.add_named_bbox (B.mk bn) g,
           IA.input_bangbox_annotation (B.mk bn) Json.Null ann)
      | add_bbox (bn,Json.Object obj') (g,ann) =
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
                                  "contents"),
           IA.input_bangbox_annotation (B.mk bn) (get_easy Json.Null obj' "annotation") ann
             handle IA.bad_input_exp (m,l) =>
               raise bad_input_exp (m,prepend_prop "annotation" l))
      | add_bbox (bn,_) _ = raise bad_input_exp ("Expected object",bn)
  in
    (* we do this in two stages, so that ordering doesn't matter *)
    (fold_obj_easy set_bbox_parent obj prop)
    o
    (fold_obj_easy add_bbox obj prop)
    handle G.bbox_already_exists_exp (bn,_) =>
             raise bad_input_exp ("Duplicate bang box ("^(B.dest bn)^")", prop)
  end

  fun get_ann obj prop (g,ann) =
        (g, IA.input_graph_annotation (get_easy Json.Null obj prop) ann)
        handle IA.bad_input_exp (m,l) =>
          raise bad_input_exp (m,prepend_prop prop l)

  fun input (Json.Object obj) =
        (G.empty, IA.empty_annotation)
            |> get_wverts obj "wire_vertices"
            |> get_nverts obj "node_vertices"
            |> get_edges G.UnDirected obj "undir_edges"
            |> get_edges G.Directed obj "dir_edges"
            |> get_bboxes obj "bang_boxes"
            |> get_ann obj "annotation"
    | input Json.Null = (G.empty,IA.empty_annotation)
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor InputGraphJSON(
  structure Graph : BANG_GRAPH
        and InputVertexData : INPUT_JSON
        and InputEdgeData : INPUT_JSON
  sharing type InputVertexData.data = Graph.OVData.IData.data
  sharing type InputEdgeData.data = Graph.EData.data
)  : INPUT_JSON
= struct
  structure SubInput = InputAnnotatedGraphJSON(
    structure Graph = Graph
    structure InputVertexData = InputVertexData
    structure InputEdgeData = InputEdgeData
    structure InputAnnotation = EmptyGraphAnnotationInputJson
  );
  open SubInput;
  type data = Graph.T;
  val input = fst o SubInput.input;
end;

functor OutputAnnotatedGraphJSON(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type OutputVertexData.data = Graph.OVData.IData.data
  sharing type OutputEdgeData.data = Graph.EData.data
  structure OutputAnnotation : GRAPH_ANN_OUTPUT_JSON
)  : OUTPUT_JSON
= struct
  structure G = Graph;
  structure OA = OutputAnnotation;

  open JsonOutputUtils;
  type data = Graph.T * OutputAnnotation.data;

  fun get_verts (g,ann) = let
    fun vert_info v vd =
      Json.Object (
        Json.empty_obj |> update ("data",OutputVertexData.output vd)
                       |> update ("annotation",OA.output_vertex_annotation ann v)
      )
    fun add_vert (v,(G.OVData.WVert,_)) (wvs,nvobj) =
          ((Json.String (V.dest v))::wvs,nvobj)
      | add_vert (v,(G.OVData.NVert vd,_)) (wvs,nvobj) =
          (wvs,update (V.dest v,vert_info v vd) nvobj)
    val (wvs,nvobj) = V.NTab.fold add_vert
                                  (G.get_vertices g)
                                  ([],Json.empty_obj)
  in
    (Json.Array wvs,Json.Object nvobj)
  end

  fun get_edges (g,ann) = let
    fun edge_info e ed s t =
      Json.Object (
        Json.empty_obj |> update ("src",Json.String (V.dest s))
                       |> update ("tgt",Json.String (V.dest t))
                       |> update ("data",OutputEdgeData.output ed)
                       |> update ("annotation",OA.output_edge_annotation ann e)
      )
    fun add_edge (e,((G.Directed,ed),(s,t))) (dobj,udobj) =
          (update (E.dest e,edge_info e ed s t) dobj,udobj)
      | add_edge (e,((G.UnDirected,ed),(s,t))) (dobj,udobj) =
          (dobj,update (E.dest e,edge_info e ed s t) udobj)
    val (dobj,udobj) = E.NTab.fold add_edge
                                   (G.get_edges g)
                                   (Json.empty_obj,Json.empty_obj)
  in
    (Json.Object dobj,Json.Object udobj)
  end

  fun get_bboxes (g,ann) = let
    fun add_bbox b = let
        val add_parent =
          case G.lookup_bbox_parent g b
            of SOME bp => update ("parent",Json.String (B.dest bp))
             | NONE => I
        val jvs = map (fn v => Json.String (V.dest v))
                      (V.NSet.list_of (G.get_bbox_contents g b))
        val obj = Json.empty_obj |> add_parent
                                 |> update ("contents",Json.Array jvs)
                                 |> update ("annotation",OA.output_bangbox_annotation ann b)
      in
        update (B.dest b, Json.Object obj)
      end
  in
    Json.Object (B.NSet.fold add_bbox (G.get_bboxes g) Json.empty_obj)
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
end;

functor OutputGraphJSON(
  structure Graph : BANG_GRAPH
        and OutputVertexData : OUTPUT_JSON
        and OutputEdgeData : OUTPUT_JSON
  sharing type OutputVertexData.data = Graph.OVData.IData.data
  sharing type OutputEdgeData.data = Graph.EData.data
)  : OUTPUT_JSON
= struct
  structure SubOutput = OutputAnnotatedGraphJSON(
    structure Graph = Graph
    structure OutputVertexData = OutputVertexData
    structure OutputEdgeData = OutputEdgeData
    structure OutputAnnotation = EmptyGraphAnnotationOutputJson
  );
  open SubOutput;
  type data = Graph.T;
  fun output g = SubOutput.output (g,());
end;

