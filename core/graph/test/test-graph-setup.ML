(* Graph structures for testing *)

structure Test_Graph_Data =
struct
  val pretty_theory_name = Pretty.str "test_graph"

  datatype nvdata = VUnit1
                  | VUnit2
                  | VExpr1 of LinratAngleExpr.T
                  | VExpr2 of LinratAngleExpr.T
                  | VString1 of string
                  | VString2 of string
  datatype edata = EUnit1
                 | EUnit2
                 | EExpr1 of LinratAngleExpr.T
                 | EExpr2 of LinratAngleExpr.T
                 | EString1 of string
                 | EString2 of string
  
  (* node-vertex data *)
  val default_nvdata = VUnit1

  fun nvdata_eq (VUnit1, VUnit1) = true
    | nvdata_eq (VUnit2, VUnit2) = true
    | nvdata_eq (VExpr1 a, VExpr1 b) = LinratAngleExpr.eq a b
    | nvdata_eq (VExpr2 a, VExpr2 b) = LinratAngleExpr.eq a b
    | nvdata_eq (VString1 a, VString1 b) = (a = b)
    | nvdata_eq (VString2 a, VString2 b) = (a = b)
    | nvdata_eq _ = false
      
  fun pretty_nvdata VUnit1 = Pretty.str "VUnit1"
    | pretty_nvdata VUnit2 = Pretty.str "VUnit2"
    | pretty_nvdata (VExpr1 a) = 
        Pretty.block [Pretty.str "VExpr1(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_nvdata (VExpr2 a) = 
        Pretty.block [Pretty.str "VExpr2(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_nvdata (VString1 s) = 
        Pretty.block [Pretty.str "VString1(", Pretty.str s, Pretty.str ")"]
    | pretty_nvdata (VString2 s) = 
        Pretty.block [Pretty.str "VString2(", Pretty.str s, Pretty.str ")"]

  (* edge data *)
  val default_edata = EUnit1
  fun edata_eq (EUnit1, EUnit1) = true
    | edata_eq (EUnit2, EUnit2) = true
    | edata_eq (EExpr1 a, EExpr1 b) = LinratAngleExpr.eq a b
    | edata_eq (EExpr2 a, EExpr2 b) = LinratAngleExpr.eq a b
    | edata_eq (EString1 a, EString1 b) = (a = b)
    | edata_eq (EString2 a, EString2 b) = (a = b)
    | edata_eq _ = false
  
  fun pretty_edata EUnit1 = Pretty.str "EUnit1"
    | pretty_edata EUnit2 = Pretty.str "EUnit2"
    | pretty_edata (EExpr1 a) = 
        Pretty.block [Pretty.str "EExpr1(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_edata (EExpr2 a) = 
        Pretty.block [Pretty.str "EExpr2(", LinratAngleExpr.pretty a, Pretty.str ")"]
    | pretty_edata (EString1 s) = 
        Pretty.block [Pretty.str "EString1(", Pretty.str s, Pretty.str ")"]
    | pretty_edata (EString2 s) = 
        Pretty.block [Pretty.str "EString2(", Pretty.str s, Pretty.str ")"]
  
  (* matching and substitution *)
  type psubst = LinratAngleExpr.match
  type subst = psubst
  fun init_psubst_from_data _ = LinratAngleExpr.empty_match
  val solve_psubst = Seq.single

  fun match_nvdata ((VExpr1 a1),(VExpr1 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_nvdata ((VExpr2 a1),(VExpr2 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_nvdata (v1,v2) m = 
        if nvdata_eq (v1, v2) then SOME m else NONE

  fun subst_in_nvdata sub (VExpr1 a) = VExpr1 (LinratAngleExpr.subst sub a)
    | subst_in_nvdata sub (VExpr2 a) = VExpr2 (LinratAngleExpr.subst sub a)
    | subst_in_nvdata _ vd = vd

  fun match_edata ((EExpr1 a1),(EExpr1 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_edata ((EExpr2 a1),(EExpr2 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_edata (e1,e2) m = 
        if edata_eq (e1,e2) then SOME m else NONE
  
  fun subst_in_edata sub (EExpr1 a) = EExpr1 (LinratAngleExpr.subst sub a)
    | subst_in_edata sub (EExpr2 a) = EExpr2 (LinratAngleExpr.subst sub a)
    | subst_in_edata _ ed = ed
    
  val pretty_subst = LinratAngleExpr.pretty_match
  val print_subst = Pretty.writeln o pretty_subst
end

(*
 * These signatures define ographs/bang-graphs whose Data sub-structure
 * is the one given above. This allows alternative implementations of
 * graphs to be tested with the same set of test functors.
 *)

signature TEST_OGRAPH = OGRAPH
    where type nvdata = Test_Graph_Data.nvdata
      and type edata  = Test_Graph_Data.edata
      and type psubst = Test_Graph_Data.psubst
      and type subst  = Test_Graph_Data.subst

signature TEST_BANG_GRAPH = BANG_GRAPH
    where type nvdata = Test_Graph_Data.nvdata
      and type edata  = Test_Graph_Data.edata
      and type psubst = Test_Graph_Data.psubst
      and type subst  = Test_Graph_Data.subst

structure Test_Bang_Graph = BangGraph(structure Data = Test_Graph_Data)
structure Test_OGraph = Test_Bang_Graph : OGRAPH



structure Test_Bang_Graph_IO =
struct
  structure ComponentDataIO (* : GRAPH_COMPONENT_DATA_IO *)
  = struct
    type vdata = Test_Graph_Data.nvdata
    type edata = Test_Graph_Data.edata

    structure IVDataInputJSON : INPUT_JSON =
    struct
      open JsonInputUtils
      open Test_Graph_Data
      structure L = InputLinratJSON
      type data = vdata
      val get_angle = L.input oo (get_easy Json.Null)
      val to_lower = String.implode o (map Char.toLower) o String.explode
      fun input (Json.String t) =
            (case to_lower t
               of "vunit1" => VUnit1
                | "vunit2" => VUnit2
                | "vexpr1" => VExpr1 LinratAngleExpr.zero
                | "vexpr2" => VExpr2 LinratAngleExpr.zero
                | "vstring1" => VString1 ""
                | "vstring2" => VString2 ""
                | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
        | input (Json.Object obj) =
            (case to_lower (get_string obj "type")
               of "vunit1" => VUnit1
                | "vunit2" => VUnit2
                | "vexpr1" => VExpr1 (get_angle obj "angle")
                | "vexpr2" => VExpr2 (get_angle obj "angle")
                | "vstring1" => VString1 (get_string_easy "" obj "data")
                | "vstring2" => VString2 (get_string_easy "" obj "data")
                | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
        | input _ = raise bad_input_exp ("Expected object","")
    end
    structure IVDataOutputJSON : OUTPUT_JSON =
    struct
      open JsonOutputUtils
      open Test_Graph_Data
      structure L = OutputLinratJSON
      type data = vdata
      fun output VUnit1 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VUnit1")
            )
        | output VUnit2 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VUnit2")
            )
        | output (VExpr1 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VExpr1")
                             |> update ("angle",L.output a)
            )
        | output (VExpr2 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VExpr2")
                             |> update ("angle",L.output a)
            )
        | output (VString1 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VString1")
                             |> update ("data",Json.String s)
            )
        | output (VString2 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VString2")
                             |> update ("data",Json.String s)
            )
    end
    structure EDataInputJSON : INPUT_JSON =
    struct
      open JsonInputUtils
      open Test_Graph_Data
      structure L = InputLinratJSON
      type data = edata
      val get_angle = L.input oo (get_easy Json.Null)
      val to_lower = String.implode o (map Char.toLower) o String.explode
      fun input (Json.String t) =
            (case to_lower t
               of "eunit1" => EUnit1
                | "eunit2" => EUnit2
                | "eexpr1" => EExpr1 LinratAngleExpr.zero
                | "eexpr2" => EExpr2 LinratAngleExpr.zero
                | "estring1" => EString1 ""
                | "estring2" => EString2 ""
                | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
        | input (Json.Object obj) =
            (case to_lower (get_string obj "type")
               of "eunit1" => EUnit1
                | "eunit2" => EUnit2
                | "eexpr1" => EExpr1 (get_angle obj "angle")
                | "eexpr2" => EExpr2 (get_angle obj "angle")
                | "estring1" => EString1 (get_string_easy "" obj "data")
                | "estring2" => EString2 (get_string_easy "" obj "data")
                | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
        | input _ = raise bad_input_exp ("Expected object","")
    end
    structure EDataOutputJSON : OUTPUT_JSON =
    struct
      open JsonOutputUtils
      open Test_Graph_Data
      structure L = OutputLinratJSON
      type data = edata
      fun output EUnit1 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EUnit1")
            )
        | output EUnit2 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EUnit2")
            )
        | output (EExpr1 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EExpr1")
                             |> update ("angle",L.output a)
            )
        | output (EExpr2 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EExpr2")
                             |> update ("angle",L.output a)
            )
        | output (EString1 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EString1")
                             |> update ("data",Json.String s)
            )
        | output (EString2 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EString2")
                             |> update ("data",Json.String s)
            )
    end

    structure DotStyle : DOT_STYLE =
    struct
      open Test_Graph_Data
      type vdata = vdata
      fun style_for_ivertex_data VUnit1 =
            "[style=filled,fillcolor=white,fontcolor=black,shape=square]"
        | style_for_ivertex_data VUnit2 =
            "[style=filled,fillcolor=black,fontcolor=black,shape=square]"
        | style_for_ivertex_data (VExpr1 _) =
            "[style=filled,fillcolor=red,fontcolor=black,shape=circle]"
        | style_for_ivertex_data (VExpr2 _) =
            "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
        | style_for_ivertex_data (VString1 _) =
            "[style=filled,fillcolor=blue,fontcolor=black,shape=triangle]"
        | style_for_ivertex_data (VString2 _) =
            "[style=filled,fillcolor=pink,fontcolor=black,shape=triangle]"
    end
  end
  structure InputGraphJSON = InputGraphJSON(
    structure Graph = Test_Bang_Graph
    structure InputVertexData = ComponentDataIO.IVDataInputJSON
    structure InputEdgeData = ComponentDataIO.EDataInputJSON
  )
  structure OutputGraphJSON = OutputGraphJSON(
    structure Graph = Test_Bang_Graph
    structure OutputVertexData = ComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = ComponentDataIO.EDataOutputJSON
  )
  structure OutputGraphDot = OutputGraphDot(
    structure Graph = Test_Bang_Graph
    structure DotStyle = ComponentDataIO.DotStyle
  )
end


functor Test_OGraph_Tools(
  structure G : TEST_OGRAPH
)
= struct
  structure G    = G
  structure Data = Test_Graph_Data
  (*structure GIso = GraphIsoFun(structure Graph = G)*)
  structure Expr = LinratAngleExpr

  val vert = G.NVert
  fun unwrap_vert G.WVert = raise ERROR "wire vertex"
    | unwrap_vert (G.NVert vd) = vd

  val vunit1     = vert Data.VUnit1
  val vunit2     = vert Data.VUnit2
  fun vexpr1   a = vert (Data.VExpr1 (Expr.parse a))
  val vexpr1_a   = vert o Data.VExpr1
  fun vexpr2   a = vert (Data.VExpr2 (Expr.parse a))
  val vexpr2_a   = vert o Data.VExpr2
  fun vstring1 s = vert (Data.VString1 s)
  fun vstring2 s = vert (Data.VString2 s)

  val eunit1     = Data.EUnit1
  val eunit2     = Data.EUnit2
  fun eexpr1   a = (Data.EExpr1 (Expr.parse a))
  val eexpr1_a   = Data.EExpr1
  fun eexpr2   a = (Data.EExpr2 (Expr.parse a))
  val eexpr2_a   = Data.EExpr2
  fun estring1 s = (Data.EString1 s)
  fun estring2 s = (Data.EString2 s)

  (* creating stuff *)
  fun add_vunit1   n   = G.add_named_vertex (V.mk n) vunit1
  fun add_vunit2   n   = G.add_named_vertex (V.mk n) vunit2
  fun add_vexpr1   n a = G.add_named_vertex (V.mk n) (vexpr1 a)
  fun add_vexpr1_a n a = G.add_named_vertex (V.mk n) (vexpr1_a a)
  fun add_vexpr2   n a = G.add_named_vertex (V.mk n) (vexpr2 a)
  fun add_vexpr2_a n a = G.add_named_vertex (V.mk n) (vexpr2_a a)
  fun add_vstring1 n s = G.add_named_vertex (V.mk n) (vstring1 s)
  fun add_vstring2 n s = G.add_named_vertex (V.mk n) (vstring2 s)

  fun add_dir_eunit1 n s t =
        G.add_named_edge (E.mk n) (Directed,eunit1) (V.mk s) (V.mk t)
  fun add_dir_eunit2 n s t =
        G.add_named_edge (E.mk n) (Directed,eunit2) (V.mk s) (V.mk t)
  fun add_dir_eexpr1 n s t a =
        G.add_named_edge (E.mk n) (Directed,(eexpr1 a)) (V.mk s) (V.mk t)
  fun add_dir_eexpr1_a n s t a =
        G.add_named_edge (E.mk n) (Directed,(eexpr1_a a)) (V.mk s) (V.mk t)
  fun add_dir_eexpr2 n s t a =
        G.add_named_edge (E.mk n) (Directed,(eexpr2 a)) (V.mk s) (V.mk t)
  fun add_dir_eexpr2_a n s t a =
        G.add_named_edge (E.mk n) (Directed,(eexpr2_a a)) (V.mk s) (V.mk t)
  fun add_dir_estring1 n str s t =
        G.add_named_edge (E.mk n) (Directed,(estring1 str)) (V.mk s) (V.mk t)
  fun add_dir_estring2 n str s t =
        G.add_named_edge (E.mk n) (Directed,(estring2 str)) (V.mk s) (V.mk t)

  fun add_undir_eunit1 n s t =
        G.add_named_edge (E.mk n) (Undirected,Data.EUnit1) (V.mk s) (V.mk t)
  fun add_undir_eunit2 n s t =
        G.add_named_edge (E.mk n) (Undirected,Data.EUnit2) (V.mk s) (V.mk t)
  fun add_undir_eexpr1 n s t a =
        G.add_named_edge (E.mk n) (Undirected,(Data.EExpr1 (Expr.parse a))) (V.mk s) (V.mk t)
  fun add_undir_eexpr1_a n s t a =
        G.add_named_edge (E.mk n) (Undirected,(Data.EExpr1 a)) (V.mk s) (V.mk t)
  fun add_undir_eexpr2 n s t a =
        G.add_named_edge (E.mk n) (Undirected,(Data.EExpr2 (Expr.parse a))) (V.mk s) (V.mk t)
  fun add_undir_eexpr2_a n s t a =
        G.add_named_edge (E.mk n) (Undirected,(Data.EExpr2 a)) (V.mk s) (V.mk t)
  fun add_undir_estring1 n str s t =
        G.add_named_edge (E.mk n) (Undirected,(Data.EString1 str)) (V.mk s) (V.mk t)
  fun add_undir_estring2 n str s t =
        G.add_named_edge (E.mk n) (Undirected,(Data.EString2 str)) (V.mk s) (V.mk t)

  (* testing stuff *)
  fun assert_vnames exp_vs actual_vset =
    let
      val exp_vset = V.NSet.of_list (map V.mk exp_vs)
      val pretty_set = Pretty.string_of o V.NSet.pretty
    in
      if V.NSet.eq exp_vset actual_vset
      then ()
      else raise ERROR ("assert_vnames: expected "^(pretty_set exp_vset)^
                        " but got "^(pretty_set actual_vset))
    end
  fun assert_enames exp_es actual_eset =
    let
      val exp_ens = E.NSet.of_list (map E.mk exp_es)
      val pretty_set = Pretty.string_of o E.NSet.pretty
    in
      if E.NSet.eq exp_ens actual_eset
      then ()
      else raise ERROR ("assert_enames: expected "^(pretty_set exp_ens)^
                        " but got "^(pretty_set actual_eset))
    end
  (*fun assert_xnames exp_vs actual_vset =
    let
      val exp_vset = X.NSet.of_list (map X.mk exp_vs)
      val pretty_set = Pretty.string_of o X.NSet.pretty
    in
      if X.NSet.eq exp_vset actual_vset
      then ()
      else raise ERROR ("assert_xnames: expected "^(pretty_set exp_vset)^
                        " but got "^(pretty_set actual_vset))
    end*)
  fun assert_vertices g exp_vs =
    let
      val actual_vset = G.get_vertices g
      val _ = assert_vnames (map fst exp_vs) actual_vset
      fun check_verts ((vn,(vd,(ins,outs)))::vs) =
        let
          val (vd',(ins',outs')) = (G.get_vertex_data g (V.mk vn),
                                    (G.get_in_edges g (V.mk vn),
                                     G.get_out_edges g (V.mk vn)))
          val _ = Testing.assert ("same data for "^vn)
                    (G.vdata_eq (vd,vd'))
          val _ = Testing.assert ("same ins for "^vn)
                    (E.NSet.eq ins' (E.NSet.of_list (map E.mk ins)))
          val _ = Testing.assert ("same outs for "^vn)
                    (E.NSet.eq outs' (E.NSet.of_list (map E.mk outs)))
        in check_verts vs end
        | check_verts [] = ()
    in check_verts exp_vs end
  fun assert_edges g exp_es =
    let
      val actual_eset = G.get_edges g
      val _ = assert_enames (map fst exp_es) actual_eset
      fun check_edges ((en,((dir,ed),(src,tgt)))::es) =
        let
          val ((dir',ed'),(src',tgt')) = (G.get_edge_dir_and_data g (E.mk en),
                                          (G.get_edge_source g (E.mk en),
                                           G.get_edge_target g (E.mk en)))
          val _ = Testing.assert ("same dir for "^en)
                    (dir_eq dir dir')
          val _ = Testing.assert ("same data for "^en)
                    (G.edata_eq (ed,ed'))
          val _ = Testing.assert ("same source for "^en)
                    (V.name_eq ((V.mk src),src'))
          val _ = Testing.assert ("same target for "^en)
                    (V.name_eq ((V.mk tgt),tgt'))
        in check_edges es end
        | check_edges [] = ()
    in check_edges exp_es end
  fun assert_none _ NONE = ()
    | assert_none name (SOME _) = raise ERROR (name^" is not NONE")

  (* FIXME: move into Testing in isaplib *)
  fun test_force s f v =
    case Testing.test s f v of
         NONE => raise Testing.failed_tests_exp (!Testing.tests_failed_ref)
       | SOME x => x

  (* TODO: re-introduce tests requiring iso checks *)
  (*fun assert_iso_graph msg exp actual =
    if GIso.test exp actual
    then actual
    else (writeln "Expected:" G.print exp
          writeln "Got:" G.print actual
          raise Testing.assertion_failed_exp msg)*)
end

(*
(* Some helpful tools for creating string graphs *)
functor Test_OGraph_Tools(
  G : OGRAPH where type EData.data = Test_Graph_Data.edata
               and type OVData.IData.data = Test_Graph_Data.vdata
)
= struct
  structure OG = G

  fun unwrap_nv G.OVData.WVert = raise ERROR "wire vertex"
    | unwrap_nv (G.OVData.NVert vd) = vd
  val nv = G.OVData.NVert
  fun add_wv n = G.add_named_vertex (V.mk n) G.OVData.WVert

  structure GraphTools = Test_Graph_Tools(
    structure G  = G
    val vert = nv
    val unwrap_vert = unwrap_nv
  )
  open GraphTools

  structure G = OG

  fun assert_iso_graph_up_to_wires msg exp actual =
    assert_iso_graph msg (G.minimise exp) (G.minimise actual)
end
*)

(* Some helpful tools for creating graphs *)
functor Test_Bang_Graph_Tools(
  structure BG : TEST_BANG_GRAPH
) =
struct
  structure BG = BG

  (*fun unwrap_nv BG.WVert = raise ERROR "wire vertex"
    | unwrap_nv (BG.NVert vd) = vd
  val nv = BG.NVert*)
  fun add_wv n = BG.add_named_vertex (V.mk n) BG.WVert

  structure Test_OG_Tools = Test_OGraph_Tools(structure G = BG)
  open Test_OG_Tools

  structure G = BG
  (*structure GIso = BangGraphIso(G)*)
  structure GJsonInput = Test_Bang_Graph_IO.InputGraphJSON
  structure GJsonOutput = Test_Bang_Graph_IO.OutputGraphJSON
  structure GDotOutput = Test_Bang_Graph_IO.OutputGraphDot

  fun add_to_bbox _ [] = I
    | add_to_bbox n vs =
        let
          val bn = B.mk n
          val vset = V.NSet.of_list (map V.mk vs)
        in
          G.add_to_bbox_anon bn vset
        end
  fun add_bbox n vs = add_to_bbox n vs o G.add_named_bbox (B.mk n)
  fun add_child_bbox n parent vs =
          G.add_child_to_bbox (B.mk parent) (B.mk n) o
          add_to_bbox n vs o
          G.add_named_bbox (B.mk n)

  local
    fun write_to_file file s = let
      val outstream = TextIO.openOut file
    in
      (TextIO.output (outstream, s);
       TextIO.closeOut outstream)
    end
  in
    fun graph_from_json str    = GJsonInput.input str
    fun load_graph      path   = GJsonInput.input (Json.read_file path)
    fun save_dot_graph  g path = write_to_file path (GDotOutput.output g)

    val print_dot_graph        = writeln o GDotOutput.output
    val print_json_graph       = Pretty.writeln o Json.pretty o GJsonOutput.output
  end

  fun num_bboxes g = B.NSet.cardinality (G.get_bboxes g)
  
  fun assert_n_bboxes g n () =
    if (num_bboxes g) = n
    then () else raise ERROR(
      "expected: " ^ Int.toString n ^ " bboxes, "^
      "got: " ^ Int.toString (num_bboxes g))

  (*fun assert_iso_graph msg exp actual =
    if GIso.test exp actual
    then actual
    else (writeln "Expected:" G.print exp
          writeln "Got:" G.print actual
          raise Testing.assertion_failed_exp msg)

  fun assert_iso_graph_up_to_wires msg exp actual =
    assert_iso_graph msg (G.minimise exp) (G.minimise actual)*)
end

