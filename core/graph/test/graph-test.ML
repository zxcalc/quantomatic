(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Test_GraphParam
: GRAPH_PARAM
= struct

  structure VData = Test_InternVData;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData 
                         structure S = Test_InternVData 
                         val default_data = ());
  
  (* vertex, and vertex data variable dependencies *)
  structure VDep = VXBRel;
  
  (* internal name data *)
  type T = VDep.T;

  (* pretty printing *)
  val pretty = VDep.pretty;
  val print = VDep.print;
  
  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;
  exception unknown_type_exp of string;

  fun subst_xname_in_vdata n1 n2 d =
      Test_InternVData.map_to_angle 
        (untryify (LinratAngleExpr.try_rename1 n1 n2)) d 
      handle tryify_failed () => 
        raise X.Rnm.invalid_renaming_exp "subst_xname_in_vdata";
  fun subst_xname_in_edata _ _ d = d;
  
  fun get_xnames vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun dep_vnames_of_xname vdep n = VDep.codf vdep n;
  fun dep_enames_of_xname _    _ = E.NSet.empty;

  fun try_rename1_xname n1 n2 vdep = VDep.try_rename1_cod n1 n2 vdep;
  fun try_rename1_vname n1 n2 vdep = VDep.try_rename1_dom n1 n2 vdep;

  fun rename_xname n1 n2 vdep = 
      (case VDep.try_rename1_cod n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_xname"
       | SOME x => x);

  fun rename_vname n1 n2 vdep = 
      (case VDep.try_rename1_dom n1 n2 vdep of NONE => 
         raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_vname"
       | SOME x => x);

  fun rename_ename _ _ vdep = vdep;
  
  (* rename names in T *)

  (* rename internal names in the dependency structure *)
  structure CRnmX : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = X
           type obj = T
           val get_nameset = get_xnames
           val try_rename1 = try_rename1_xname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmV : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = V
           type obj = T
           val get_nameset = get_vnames
           val try_rename1 = try_rename1_vname
    end);

  (* rename edges in the dependency structure *)
  structure CRnmE : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = E type obj = T);

  (* empty name-dependency information *)
  val empty = VDep.empty;

  fun add_vertex vd vn vdep = 
      (case Test_InternVData.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (LinratAngleExpr.get_vars angle) vdep);
  fun add_edge _ _ vdep = vdep;

  fun delete_vname vn vdep = VDep.del_dom vn vdep;
  fun delete_ename _ vdep = vdep;

  fun vertex_type_name (Test_InternVData.Xnd _) = "X"
    | vertex_type_name (Test_InternVData.Znd _) = "Z"
    | vertex_type_name (Test_InternVData.Hnd) = "hadamard"
  fun vertex_default_data "X" = (Test_InternVData.Xnd LinratAngleExpr.zero)
    | vertex_default_data "Z" = (Test_InternVData.Znd LinratAngleExpr.zero)
    | vertex_default_data "hadamard" = (Test_InternVData.Hnd)
    | vertex_default_data s = raise unknown_type_exp s
  fun edge_type_name () = "unit"
  fun edge_default_data "unit" = ()
    | edge_default_data s = raise unknown_type_exp s

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vname vn
           |> add_vertex vd vn;
  fun replace_edge _ _ vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge _ _ = empty;
  
  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = VData.subst; (* : LinratAngleExpr.T X.NTab.T; *)

  val empty_subst = VData.empty_subst; (* : X.NTab.empty; *)
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge _ _ u = SOME u;

  (* raises match_exp *)
  fun match_vertex d1 d2 u = 
      (case try_match_vertex d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 
  
  fun match_edge d1 d2 u =
      (case try_match_edge d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 

  fun subst_in_vertex u vd = VData.subst_in_data u vd;
  fun subst_in_edge _ ed = ed;

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  val pretty_theory_name = Pretty.str "red_green";

  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type subst = subst;
  end;
end;


structure Test_OVData = OVertexData(Test_InternVData);
structure Test_OGraphParam = OGraphParam(
  structure Param = Test_GraphParam 
        and OVData = Test_OVData);

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Test_VertexComponentData_Param
  structure OVData = Test_OVData);
structure Test_OVertexComponentData = ElementComponentData(Test_OVertexComponentData_Param);

(* Basic structure for testing graphs later... *)
structure DB_Test_Graph 
= struct 
  structure BasicBangGraph = BasicBangGraphFun(
    structure Param = Test_OGraphParam
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

structure Test_Graph : BANG_GRAPH = DB_Test_Graph;


(* ML utility code for building test graphs *)
structure Test_MkG = 
struct
  structure IVData = Test_InternVData;
  structure G = Test_Graph;
  structure OVData = Test_Graph.OVData;
  structure GIso = BangGraphIso(Test_Graph);

  val bvert = OVData.EVert;
  val parse_angle = LinratAngleExpr.parse;
  val zero_angle = LinratAngleExpr.zero;
  val var_angle = LinratAngleExpr.mk_var o X.mk;
  fun var_iangle i v = LinratAngleExpr.mk (0,1) [(v,i)];
  val add_angles = LinratAngleExpr.add_expr;
  val pi_angle = LinratAngleExpr.pi;
  fun mkZ a = OVData.IVert (IVData.Znd a);
  fun mkX a = OVData.IVert (IVData.Xnd a);
  val mkH = OVData.IVert IVData.Hnd;
  val undir_edge = (G.UnDirected,());
  val dir_edge = (G.Directed,());
  val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);

  (* FIXME: move into Testing in isaplib *)
  fun test_force s f v =
    case Testing.test s f v of
         NONE => raise Testing.failed_tests_exp (!Testing.tests_failed_ref)
       | SOME x => x
end;


(* test some basic graph constructions *)
local

open Test_MkG;

(* a simple graph, and some basic manipulations *)
val g = G.empty;
val (n_a, g) = g |> G.add_vertex bvert;
val (n_b, g) = g |> G.add_vertex bvert;
val (e0, g) = g |> G.add_edge undir_edge n_a n_b;
val (_, g) = g |> G.add_edge undir_edge n_a n_b;
val n_c = (V.mk "n_c");
val g = g |> G.rename_vname n_a n_c;
val e3 = (E.mk "e_c");
val g = g |> G.rename_ename e0 e3;

val in1 = V.mk "in1"
val in2 = V.mk "in2"
val out1 = V.mk "out1"
val out2 = V.mk "out2"
val green1 = V.mk "green1"
val green2 = V.mk "green2"
val green3 = V.mk "green3"
val green4 = V.mk "green4"
val green5 = V.mk "green5"
val green6 = V.mk "green6"
val green7 = V.mk "green7"
val green8 = V.mk "green8"
val green9 = V.mk "green9"
val green10 = V.mk "green10"
val red1 = V.mk "red1"
val red2 = V.mk "red2"
val red3 = V.mk "red3"
val H1 = V.mk "H1"
val H2 = V.mk "H2"
val H3 = V.mk "H3"

(* 3 CNOTs = swap example *)
val g2 = G.empty;
val g2 = g2 |> G.new_named_vertex in1 bvert
            |> G.new_named_vertex in2 bvert
            |> G.new_named_vertex out1 bvert
            |> G.new_named_vertex out2 bvert
            |> G.new_named_vertex green1 (mkZ zero_angle)
            |> G.new_named_vertex green2 (mkZ zero_angle)
            |> G.new_named_vertex green3 (mkZ zero_angle)
            |> G.new_named_vertex red1 (mkX zero_angle)
            |> G.new_named_vertex red2 (mkX zero_angle)
            |> G.new_named_vertex red3 (mkX zero_angle)
            |> G.doadd_edge undir_edge in1 green1
            |> G.doadd_edge undir_edge in2 red1
            |> G.doadd_edge undir_edge red1 green1
            |> G.doadd_edge undir_edge red1 green2
            |> G.doadd_edge undir_edge red2 green1
            |> G.doadd_edge undir_edge red2 green2
            |> G.doadd_edge undir_edge red3 green2
            |> G.doadd_edge undir_edge red2 green3
            |> G.doadd_edge undir_edge red3 green3
            |> G.doadd_edge undir_edge out1 green3
            |> G.doadd_edge undir_edge out2 red3;

(* just a swap *)
val g3 = G.empty;
val g3 = g3 |> G.new_named_vertex in1 bvert
            |> G.new_named_vertex in2 bvert
            |> G.new_named_vertex out1 bvert
            |> G.new_named_vertex out2 bvert
            |> G.doadd_edge undir_edge in1 out2
            |> G.doadd_edge undir_edge in2 out1;

(*  A MC pattern for a CNOT *)
val g4 = G.empty;
val g4 = g4 |> G.new_named_vertex in1 bvert
            |> G.new_named_vertex in2 bvert
            |> G.new_named_vertex out1 bvert
            |> G.new_named_vertex out2 bvert
            |> G.new_named_vertex green1 (mkZ zero_angle)
            |> G.new_named_vertex green2 (mkZ zero_angle)
            |> G.new_named_vertex green3 (mkZ zero_angle)
            |> G.new_named_vertex green4 (mkZ zero_angle)
            |> G.new_named_vertex green5 (mkZ zero_angle)
            |> G.new_named_vertex green6 (mkZ zero_angle)
            |> G.new_named_vertex green7 (mkZ zero_angle)
            |> G.new_named_vertex green8 (mkZ zero_angle)
            |> G.new_named_vertex green9 (mkZ zero_angle)
            |> G.new_named_vertex green10 (mkZ zero_angle)
            |> G.new_named_vertex H1 mkH
            |> G.new_named_vertex H2 mkH
            |> G.new_named_vertex H3 mkH
            |> G.doadd_edge undir_edge in1 green2
            |> G.doadd_edge undir_edge in2 green3
            |> G.doadd_edge undir_edge green3 green1
            |> G.doadd_edge undir_edge green2 out1
            |> G.doadd_edge undir_edge green2 H1
            |> G.doadd_edge undir_edge H1 green5
            |> G.doadd_edge undir_edge green5 green4
            |> G.doadd_edge undir_edge green5 green6
            |> G.doadd_edge undir_edge green3 H2
            |> G.doadd_edge undir_edge H2 green6
            |> G.doadd_edge undir_edge green6 green7
            |> G.doadd_edge undir_edge green7 green8
            |> G.doadd_edge undir_edge green7 H3
            |> G.doadd_edge undir_edge H3 green9
            |> G.doadd_edge undir_edge green9 green10
            |> G.doadd_edge undir_edge green9 out2;

(* Plugging *)

in
(* val (g,g2,g3,g4) = (g,g2,g3,g4) *)
val _ = Pretty.writeln (Pretty.str "Tests set 1 Passed: test/graph.ML.");
end


(* Basic test for internal variable dependency management. *)
local 
  open Test_MkG;
  
  val _ = let 
  (* create the graph *)
  val (g,n_x2) = let 
    val n_a = V.mk "n_a"
    val n_b = V.mk "n_b"
    val n_x = V.mk "n_x"
    val e0 = E.mk "e0"
    val e1 = E.mk "e1"
    val g = G.empty;
    val g = g |> G.new_named_vertex n_a bvert
              |> G.new_named_vertex n_b bvert
              |> G.new_named_vertex n_x (mkX (parse_angle "2/3*x"))
              |> G.new_named_edge e0 undir_edge n_a n_x
              |> G.new_named_edge e1 undir_edge n_x n_b
  
    val _ = Testing.test "Single variable in dependencies" (fn () =>
      if X.name_eq (the (X.NSet.tryget_singleton (G.get_xnames g)), 
                            (X.mk "x")) then ()
      else raise ERROR "Variable name was wrong") ();
    
    val _ = Testing.test "vname dep is the right singleton" (fn () =>
      let 
        val dep_v_name = the (V.NSet.tryget_singleton 
            (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "x"))); 
      in
        if V.name_eq (n_x, dep_v_name) then ()
        else raise ERROR "Vertex name was wrong"
      end) ();
      
    val n_x2 = (V.mk "n_x2");
    val g = g |> G.rename_vname n_x n_x2;
    val g = g |> G.rename_ename e0 (E.mk "e_c");
    val _ = (writeln "g before renaming: "; G.print g);
    val g = g |> G.rename_xname (X.mk "x") (X.mk "y");
    val _ = (writeln "g after renaming: "; G.print g);
    
    val _ = Testing.test "vname dep is the right singleton after renaming" 
    (fn () =>
      let 
        val dep_v_name = the (V.NSet.tryget_singleton 
            (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "y"))); 
      in
        if V.name_eq (n_x2, dep_v_name) then ()
        else raise ERROR "Vertex name was wrong"
      end) ();
  in (g,n_x2) end;

  val _ = Testing.test "Single variable was renamed in dependencies" 
  (fn () => let 
    val true = X.name_eq (the (X.NSet.tryget_singleton (G.get_xnames g)), 
                          (X.mk "y")) 
    val (d,_) = G.get_vertex g n_x2;
    val true =  X.name_eq (the (X.NSet.tryget_singleton 
                         (G.Param.get_xnames (G.Param.of_vertex d n_x2))), 
                        (X.mk "y"));
    in () end) ();

  val _ = Testing.test "After renaming, still has correct singleton vname dep" (fn () => let 
    val dep_v_name = the (V.NSet.tryget_singleton 
        (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "y"))); 
    val true = V.name_eq (n_x2, dep_v_name); 
    in () end) ();
  
  val _ = Testing.test "After renaming, internal vertex data was updated" (fn () => let 
    val d = G.get_vertex_data g n_x2;
    val expected_data = (mkX (parse_angle "2/3*y"));
    val true = G.VData.data_eq (d,expected_data);
    in () end) ();
  in () end; (* these tests should not bind any names. *)
  
  val _ = Testing.test "G.CRnmX.rename: renaming of internal variables: X(a+b)-"
  (fn () => let
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (x1, g) = g |> G.add_vertex (mkX (parse_angle "a + b"));
    val (e0, g) = g |> G.add_edge undir_edge x1 b1;
    val xnames = G.get_xnames g;
    val _ = (writeln "graph before renaming: "; G.print g);
    val _ = (writeln "\n"; G.Param.print (G.get_param g));
    val xrn = X.Rnm.mk_from_avoids xnames;
    val (xrn', g') = G.CRnmX.rename xrn g;
    val _ = (writeln "\n"; X.Rnm.print xrn');
    val _ = (writeln "\n"; G.Param.print (G.get_param g'));
    val _ = (writeln "graph after renaming: "; G.print g');
    val xnames' = G.get_xnames g';
    val () = X.NSet.fold (fn n => if X.NSet.contains xnames n then 
                  raise ERROR "old name was accidentally kept in graph xnames!" else I)
              xnames' ();
    val (d,_) = G.get_vertex g' x1;
    val xnames'' = G.Param.get_xnames (G.Param.of_vertex d x1);
    val () = X.NSet.fold (fn n => if X.NSet.contains xnames n then 
                  raise ERROR "old name was accidentally kept vertex data!" else I)
              xnames'' ();
    in () end) ();
in
val _ = Testing.assert_no_failed_tests();
val _ = "UNIT TESTS FOR LinratAngleExpr PASSED!";
end; (* ends local namespace used for testing *)


local
  open Test_MkG
  val b1 = V.mk "b1"
  val b2 = V.mk "b2"
  val b3 = V.mk "b3"
  val b4 = V.mk "b4"
  val b5 = V.mk "b5"
  val b6 = V.mk "b6"
  val c1 = V.mk "c1"
  val c2 = V.mk "c2"
  val c3 = V.mk "c3"
  val r1 = V.mk "r1"
  val r2 = V.mk "r2"
  val r3 = V.mk "r3"

  val g = G.empty
  val g = g |> G.new_named_vertex b1 bvert
            |> G.new_named_vertex b2 bvert
            |> G.new_named_vertex b3 bvert
            |> G.new_named_vertex b4 bvert
            |> G.new_named_vertex b5 bvert
            |> G.new_named_vertex b6 bvert
            |> G.new_named_vertex c1 bvert
            |> G.new_named_vertex c2 bvert
            |> G.new_named_vertex c3 bvert
            |> G.new_named_vertex r1 (mkX zero_angle)
            |> G.new_named_vertex r2 (mkX zero_angle)
            |> G.new_named_vertex r3 (mkX zero_angle)
  val (e1,g)  = g|> G.add_edge dir_edge r1 b1
  val (e2,g)  = g|> G.add_edge dir_edge b1 b2
  val (e3,g)  = g|> G.add_edge dir_edge b2 r2
  val (e4,g)  = g|> G.add_edge dir_edge r2 b3
  val (e5,g)  = g|> G.add_edge dir_edge b3 b4
  val (e6,g)  = g|> G.add_edge dir_edge r2 r3
  val (e7,g)  = g|> G.add_edge dir_edge r3 b5
  val (e8,g)  = g|> G.add_edge dir_edge b5 r3
  val (e9,g)  = g|> G.add_edge dir_edge c1 c2
  val (e10,g) = g|> G.add_edge dir_edge c2 c1
  val (e11,g) = g|> G.add_edge dir_edge c3 c3
  
  val (e12,g) = g|> G.add_edge undir_edge r2 b6
  val (e13,g) = g|> G.add_edge undir_edge b6 r3
  
  
  val wire1 = G.get_wire_containing_edge g e2
  val wire2 = G.get_wire_containing_edge g e5
  val wire3 = G.get_wire_containing_edge g e6
  val wire4 = G.get_wire_containing_edge g e7
  val wire5 = G.get_wire_containing_edge g e9
  val wire6 = G.get_wire_containing_edge g e11
  val wire7 = G.get_wire_containing_edge g e12
  val _ = G.print g
  
  val wires     = sort G.wire_ord [wire1,wire2,wire3,wire4,wire5,wire6,wire7]
  val wires'    = sort G.wire_ord (G.get_wire_list g)
  
  val _ = (Pretty.writeln o Pretty.str) "wires: "
  val _ = map (Pretty.writeln o G.pretty_wire) wires
  val _ = (Pretty.writeln o Pretty.str) "wires': "
  val _ = map (Pretty.writeln o G.pretty_wire) wires'
  
  
  fun c_wire_lists (w::ws) (w'::ws') =
    if G.wire_eq w w' then c_wire_lists ws ws'
    else raise ERROR ("wire lists differ EXPECTED: "^Pretty.string_of (G.pretty_wire w)^
                      "  GOT: "^Pretty.string_of (G.pretty_wire w'))
    | c_wire_lists [] [] = ()
    | c_wire_lists _ _   = raise ERROR "failed, wire lists of different lengths"
  
  val _ = Testing.test "G.get_wire_containing_edge - interior wire"
  (fn () => if G.wire_eq (r1, r2, V.NSet.of_list [b1, b2], E.NSet.of_list [e1, e2, e3]) wire1 then I
            else raise ERROR "interior wire incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - boundary wire"
  (fn () => if G.wire_eq (r2, b4, V.NSet.of_list [b3], E.NSet.of_list [e4, e5]) wire2 then I
            else raise ERROR "boundary wire incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - no interior wvs"
  (fn () => if G.wire_eq (r2, r3, V.NSet.empty, E.NSet.of_list [e6]) wire3 then I
            else raise ERROR "no interior wvs incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - self loop"
  (fn () => if G.wire_eq (r3, r3, V.NSet.of_list [b5], E.NSet.of_list [e7, e8]) wire4 then I
            else raise ERROR "self loop incorrect") ()
  (* c1 has the minimal end, so it should be chosen as the endpoint *)
  val _ = Testing.test "G.get_wire_containing_edge - circle1"
  (fn () => if (G.wire_eq (c1, c1, V.NSet.of_list [c2], E.NSet.of_list [e9,e10]) wire5)
            then I
            else raise ERROR ("circle1 incorrect: " ^ (Pretty.string_of o G.pretty_wire) wire5)) ()
  val _ = Testing.test "G.get_wire_containing_edge - circle2"
  (fn () => if G.wire_eq (c3, c3, V.NSet.empty, E.NSet.of_list [e11]) wire6 then I
            else raise ERROR "circle2 incorrect") ()
  val _ = Testing.test "G.get_wire_containing_edge - undirected"
  (fn () => if G.wire_eq (r2, r3, V.NSet.of_list [b6], E.NSet.of_list [e12,e13]) wire7 then I
            else raise ERROR "circle2 incorrect") ()
  val _ = Testing.test "G.get_wire_list"
  (fn () => c_wire_lists wires wires') ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR WIRE FUNCTIONS PASSED!"
end; (* ends local namespace used for testing *)


local
  open Test_MkG

  val ge = G.empty
  val _ = test_force "G.get_bboxes - empty"
    (fn () => if B.NSet.is_empty (G.get_bboxes ge) then I
            else raise ERROR "get_bboxes (empty) incorrect") ()

  val _ = test_force "G.get_bbox_rel - empty"
    (fn () => if BVBRel.is_empty (G.get_bbox_rel ge) then I
            else raise ERROR "get_bbox_rel (empty) incorrect") ()

  val _ = test_force "G.get_bbox_table - empty"
    (fn () => if B.NTab.is_name_empty (G.get_bbox_table ge) then I
            else raise ERROR "get_bbox_table (empty) incorrect") ()

  val _ = test_force "G.get_vertex_bbox_table - empty"
    (fn () => if V.NTab.is_name_empty (G.get_vertex_bbox_table ge) then I
            else raise ERROR "get_vertex_bbox_table (empty) incorrect") ()

  val _ = test_force "G.get_bboxed - empty"
    (fn () => if V.NSet.is_empty (G.get_bboxed ge) then I
            else raise ERROR "get_bboxed (empty) incorrect") ()

  val _ = test_force "G.get_unbboxed - empty"
    (fn () => if V.NSet.is_empty (G.get_unbboxed ge) then I
            else raise ERROR "get_unbboxed (empty) incorrect") ()

  val _ = test_force "G.get_empty_bboxes - empty"
    (fn () => if B.NSet.is_empty (G.get_empty_bboxes ge) then I
            else raise ERROR "get_empty_bboxes (empty) incorrect") ()

  val _ = test_force "G.get_bbox_list - empty"
    (fn () => case G.get_bbox_list ge of
                   [] => I
                 | _ => raise ERROR "get_bbox_list (empty) incorrect") ()

  val _ = test_force "G.has_bbox - empty"
    (fn () => if not (G.has_bbox ge B.default_name) then I
            else raise ERROR "has_bbox (empty) incorrect") ()

  val _ = test_force "G.get_bbox_contents - empty"
    (fn () => (G.get_bbox_contents ge B.default_name;
            raise ERROR "get_bbox_contents (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = test_force "G.get_bboxes_of - empty"
    (fn () => (G.get_bboxes_of ge V.default_name;
            raise ERROR "get_bboxes_of (empty) incorrect")
            handle G.no_such_vname_exp _ => I) ()

  val _ = test_force "G.is_bboxed - empty"
    (fn () => (G.is_bboxed ge V.default_name;
            raise ERROR "is_bboxed (empty) incorrect")
            handle G.no_such_vname_exp _ => I) ()

  val _ = test_force "G.rename_bbox - empty"
    (fn () => let
                val g' = G.rename_bbox (B.mk "B0") (B.mk "B1") ge
              in
                (* no effect *)
                if B.NSet.is_empty (G.get_bboxes g') then I
                else raise ERROR "rename_bbox (empty) incorrect"
              end) ()

  val _ = test_force "G.try_rename1_bbox - empty"
    (fn () =>
      case G.try_rename1_bbox (B.mk "B0") (B.mk "B1") ge of
           NONE =>
             raise ERROR "try_rename1_bbox (empty) incorrect [got NONE]"
         | SOME g' =>
             (* no effect *)
             if B.NSet.is_empty (G.get_bboxes g') then I
             else raise ERROR "try_rename1_bbox (empty) incorrect [added bbox]"
         ) ()

  val _ = test_force "G.delete_bbox - empty"
    (fn () => G.delete_bbox B.default_name ge) ()

  val _ = test_force "g.add_to_bbox - empty"
    (fn () => (G.add_to_bbox B.default_name (V.NSet.single V.default_name) ge;
            raise error "add_to_bbox (empty) incorrect")
            handle G.no_such_vname_exp _ => I
                 | G.no_such_bbox_exp _ => I) ()

  val _ = test_force "G.remove_from_bbox - empty"
    (fn () => (G.remove_from_bbox B.default_name (V.NSet.single V.default_name) ge;
            raise ERROR "remove_from_bbox (empty) incorrect")
            handle G.no_such_vname_exp _ => I
                 | G.no_such_bbox_exp _ => I) ()

  val _ = test_force "G.kill_bbox - empty"
    (fn () => (G.kill_bbox B.default_name ge;
            raise ERROR "kill_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = test_force "G.drop_bbox - empty"
    (fn () => (G.drop_bbox B.default_name ge;
            raise ERROR "drop_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = test_force "G.merge_bboxes - empty"
    (fn () =>
    let
      val bs = B.NSet.of_list [B.mk "B0", B.mk "B1"]
    in
      (G.merge_bboxes bs ge;
            raise ERROR "merge_bboxes (empty) incorrect")
            handle G.no_such_bbox_exp _ => I
    end) ()

  (* FIXME: test copy1_bbox *)

  val _ = test_force "G.fresh_copy1_bbox - empty"
    (fn () => (G.fresh_copy1_bbox B.default_name ge;
            raise ERROR "fresh_copy1_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  (* FIXME: test rename_apart_incl_bboxes *)

  val b0 = B.mk "B0"
  val b1 = B.mk "B1"
  val gnv = test_force "G.add_named_bbox (1)"
        (fn g =>
        let
          val bexp = b0
          val g' = G.add_named_bbox bexp g
        in
          case B.NSet.tryget_singleton (G.get_bboxes g') of
               NONE => raise ERROR "add_named_bbox did not add a !-box"
             | SOME b =>
                    if not (B.name_eq (b,bexp)) then
                      raise ERROR "add_named_bbox added the wrong name"
                    else g'
        end) ge

  val gnv = test_force "G.add_named_bbox (2)"
        (fn g =>
        let
          val bexp = b1
          val bs_old = G.get_bboxes g
          val g' = G.add_named_bbox bexp g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
        in
          case B.NSet.tryget_singleton bs_added of
               NONE => raise ERROR "add_named_bbox did not add a !-box"
             | SOME b =>
                    if not (B.name_eq (b,bexp)) then
                      raise ERROR "add_named_bbox added the wrong name"
                    else g'
        end) gnv

  val _ = test_force "G.add_named_bbox (existing !-box)"
        (fn g => (G.add_named_bbox b0 g;
                  raise error "add_named_bbox (name clash) did not throw exception")
                 handle G.bbox_already_exists_exp _ => I) gnv

  val (b2,gnv) = test_force "G.add_bbox"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.add_bbox g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
        in
          case B.NSet.tryget_singleton bs_added of
               NONE => raise ERROR "add_named_bbox did not add a !-box"
             | SOME b' =>
                    if not (B.name_eq (b,b')) then
                      raise ERROR "add_named_bbox returned the wrong name"
                    else (b,g')
        end) gnv

  val _ = test_force "G.delete_bbox (no such bb)"
        (fn g =>
        let
          val bexp = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = G.delete_bbox bexp g
          val bs_new = G.get_bboxes g'
        in
          if not (B.NSet.eq bs_old bs_new) then
            raise ERROR "delete_bbox managed to delete a !-box that didn't exist"
          else g'
        end) gnv

  val _ = test_force "G.delete_bbox (empty !-box)"
        (fn g =>
        let
          val bexp = b0
          val bs_old = G.get_bboxes g
          val g' = G.delete_bbox bexp g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val _ = if not (B.NSet.is_empty bs_added) then
                    raise ERROR "delete_bbox managed to add something"
                  else ()
          val bs_removed = B.NSet.subtract bs_old bs_new
        in
          case B.NSet.tryget_singleton bs_removed of
               NONE => raise ERROR "delete_bbox did not delete exactly one item"
             | SOME b' =>
                    if not (B.name_eq (b',bexp)) then
                      raise ERROR "delete_bbox deleted the wrong !-box"
                    else g'
        end) gnv

  val _ = test_force "G.rename_bbox (no such bb; new tgt)"
        (fn g =>
        let
          val bfrom = (B.mk "B8")
          val bto = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = G.rename_bbox bfrom bto g
          val bs_new = G.get_bboxes g'
        in
          if not (B.NSet.eq bs_old bs_new) then
            raise ERROR "rename_bbox changed the graph"
          else g'
        end) gnv

  val _ = test_force "G.rename_bbox (no such bb; existing tgt)"
        (fn g =>
        let
          val bfrom = (B.mk "B8")
          val bto = b0
          val bs_old = G.get_bboxes g
          val g' = G.rename_bbox bfrom bto g
          val bs_new = G.get_bboxes g'
        in
          if not (B.NSet.eq bs_old bs_new) then
            raise ERROR "rename_bbox changed the graph"
          else g'
        end) gnv

  val _ = test_force "G.add_to_bbox (no such verts)"
        (fn g => (G.add_to_bbox b0 (V.NSet.single V.default_name) g;
                  raise error "add_to_bbox (no such verts) incorrect")
                 handle G.no_such_vnames_exp _ => I) gnv

  val gv = gnv
  val (x1,gv) = gv |> G.add_vertex (mkX  zero_angle);
  val (x2,gv) = gv |> G.add_vertex mkH;
  val (x3,gv) = gv |> G.add_vertex (mkZ  zero_angle);
  val (x4,gv) = gv |> G.add_vertex (mkZ  (parse_angle "a + b"));
  val (_ ,gv) = gv |> G.add_edge dir_edge x1 x2
  val (_ ,gv) = gv |> G.add_edge dir_edge x2 x3
  val (_ ,gv) = gv |> G.add_edge dir_edge x3 x1
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x1
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x2
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x3

  val _ = test_force "G.add_to_bbox (no such bbox)"
        (fn g => (G.add_to_bbox (B.mk "B9") (V.NSet.single x1) g;
                  raise error "add_to_bbox (no such bbox) incorrect")
                 handle G.no_such_bbox_exp _ => I) gv

  val g_disjoint = test_force "G.add_to_bbox/G.get_bbox_contents (disjoint)"
        (fn g =>
        let
          val b0_exp = (V.NSet.single x1)
          val b1_exp = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.add_to_bbox b0 b0_exp g
          val g'' = G.add_to_bbox b1 b1_exp g'
          val bs_new = G.get_bboxes g''
          val _ = if not (B.NSet.eq bs_old bs_new) then
                    raise ERROR "add_to_bbox changed the !-box set"
                  else ()
          val _ = if not (V.NSet.eq b0_exp (G.get_bbox_contents g'' b0)) then
                    raise ERROR "add_to_bbox did not set B0 contents properly"
                  else ()
          val _ = if not (V.NSet.eq b1_exp (G.get_bbox_contents g'' b1)) then
                    raise ERROR "add_to_bbox did not set B1 contents properly"
                  else ()
        in g'' end) gv

  val g_overlap = test_force "G.add_to_bbox/G.get_bbox_contents (overlapping)"
        (fn g =>
        let
          val b0_exp = (V.NSet.of_list [x1,x2])
          val b1_exp = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.add_to_bbox b0 b0_exp g
          val g'' = G.add_to_bbox b1 b1_exp g'
          val bs_new = G.get_bboxes g''
          val _ = if not (B.NSet.eq bs_old bs_new) then
                    raise ERROR "add_to_bbox changed the !-box set"
                  else ()
          val _ = if not (V.NSet.eq b0_exp (G.get_bbox_contents g'' b0)) then
                    raise ERROR "add_to_bbox did not set B0 contents properly"
                  else ()
          val _ = if not (V.NSet.eq b1_exp (G.get_bbox_contents g'' b1)) then
                    raise ERROR "add_to_bbox did not set B1 contents properly"
                  else ()
        in g'' end) gv

  val _ = test_force "G.remove_from_bbox (no such bbox)"
        (fn g => (G.remove_from_bbox (B.mk "B9") (V.NSet.single x1) g;
                  raise error "remove_from_bbox (no such bbox) incorrect")
                 handle G.no_such_bbox_exp _ => I) gv

  val _ = test_force "G.remove_from_bbox (disjoint; 1)"
        (fn g =>
        let
          val b1_rem = (V.NSet.single x2)
          val bs_old = G.get_bboxes g
          val g' = G.remove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "remove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "remove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = test_force "G.remove_from_bbox (disjoint; 2)"
        (fn g =>
        let
          val b1_rem = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.remove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "remove_from_bbox changed the !-box set"
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "remove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = test_force "G.remove_from_bbox (disjoint; 3)"
        (fn g =>
        let
          val b1_rem = (V.NSet.of_list [x1,x2])
          val bs_old = G.get_bboxes g
          val g' = G.remove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "remove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "remove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = test_force "G.remove_from_bbox (overlapping; 1)"
        (fn g =>
        let
          val b1_rem = (V.NSet.single x2)
          val bs_old = G.get_bboxes g
          val g' = G.remove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "remove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "remove_from_bbox did not set B1 contents properly"
          val _ = if V.NSet.eq (G.get_bbox_contents g b0) (G.get_bbox_contents g' b0) then ()
                  else raise ERROR "remove_from_bbox(B1) changed B0"
        in g' end) g_overlap

  val _ = test_force "G.get_empty_bboxes (empty !-boxes)"
        (fn (g,blist) =>
        let
          val bs = G.get_empty_bboxes g
          val _ = if not (B.NSet.eq bs (B.NSet.of_list blist)) then
                    raise ERROR "get_empty_bboxes did not give correct !-boxes"
                  else ()
        in () end) (gv,[b0,b1,b2])

  val _ = test_force "G.get_empty_bboxes (disjoint)"
        (fn (g,blist) =>
        let
          val bs = G.get_empty_bboxes g
          val _ = if not (B.NSet.eq bs (B.NSet.of_list blist)) then
                    raise ERROR "get_empty_bboxes did not give correct !-boxes"
                  else ()
        in () end) (g_disjoint,[b2])

  val _ = test_force "G.get_bboxed (empty !-boxes)"
        (fn (g,vlist) =>
        let
          val vs = G.get_bboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_bboxed did not give correct verts"
                  else ()
        in () end) (gv,[])

  val _ = test_force "G.get_bboxed (disjoint)"
        (fn (g,vlist) =>
        let
          val vs = G.get_bboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_bboxed did not give correct verts"
                  else ()
        in () end) (g_disjoint,[x1,x2,x3])

  val _ = test_force "G.get_bboxed (overlapping)"
        (fn (g,vlist) =>
        let
          val vs = G.get_bboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_bboxed did not give correct verts"
                  else ()
        in () end) (g_overlap,[x1,x2,x3])

  val _ = test_force "G.get_unbboxed (empty !-boxes)"
        (fn (g,vlist) =>
        let
          val vs = G.get_unbboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_unbboxed did not give correct verts"
                  else ()
        in () end) (gv,[x1,x2,x3,x4])

  val _ = test_force "G.get_unbboxed (disjoint)"
        (fn (g,vlist) =>
        let
          val vs = G.get_unbboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_unbboxed did not give correct verts"
                  else ()
        in () end) (g_disjoint,[x4])

  val _ = test_force "G.get_unbboxed (overlapping)"
        (fn (g,vlist) =>
        let
          val vs = G.get_unbboxed g
          val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                    raise ERROR "get_unbboxed did not give correct verts"
                  else ()
        in () end) (g_overlap,[x4])

  val _ = test_force "G.get_bboxes_of (disjoint)"
        (fn g =>
        let
          val bs1 = G.get_bboxes_of g x1
          val b = case B.NSet.tryget_singleton bs1
                    of NONE => raise ERROR "get_bboxes_of did not give correct number (1) of bboxes"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,b)) then
                    raise ERROR "get_bboxes_of did not give the correct !-box"
                  else ()
          val bs4 = G.get_bboxes_of g x4
          val _ = if not (B.NSet.is_empty bs4) then
                    raise ERROR "get_bboxes_of did not give correct number (0) of bboxes"
                  else ()
        in () end) g_disjoint

  val _ = test_force "G.get_bboxes_of (overlapping)"
        (fn g =>
        let
          val bs3 = G.get_bboxes_of g x3
          val b = case B.NSet.tryget_singleton bs3
                    of NONE => raise ERROR "get_bboxes_of did not give correct number (1) of bboxes"
                     | SOME b' => b'
          val _ = if B.name_eq (b1,b) then ()
                  else raise ERROR "get_bboxes_of did not give the correct !-box"
          val bs2 = G.get_bboxes_of g x2
          val _ = if (B.NSet.cardinality bs2) = 2 then ()
                  else raise ERROR "get_bboxes_of did not give correct number (2) of bboxes"
          val _ = if B.NSet.eq (B.NSet.of_list [b0,b1]) bs2 then ()
                  else raise ERROR "get_bboxes_of did not give correct bboxes"
        in () end) g_overlap

  val _ = test_force "G.get_bbox_list"
        (fn _ =>
        let
          val bs = G.get_bbox_list g_disjoint
          val _ = if (length bs) = 3 then ()
                  else raise ERROR "get_bbox_list gave wrong length list"
          val _ = if B.NSet.eq (B.NSet.of_list bs) (G.get_bboxes g_disjoint) then ()
                  else raise ERROR "get_bbox_list gave different answer to get_bboxes"
        in () end) ()

  val _ = test_force "G.has_bbox"
        (fn _ =>
        let
          val _ = if G.has_bbox g_disjoint b0 then ()
                  else raise ERROR "has_bbox gave wrong answer for b0"
          val _ = if G.has_bbox g_disjoint b2 then ()
                  else raise ERROR "has_bbox gave wrong answer for b2"
          val _ = if not (G.has_bbox g_disjoint (B.mk "B9")) then ()
                  else raise ERROR "has_bbox gave wrong answer for 'B9'"
        in () end) ()

  val _ = test_force "G.is_bboxed"
        (fn _ =>
        let
          val _ = if G.is_bboxed g_disjoint x1 then ()
                  else raise ERROR "is_bboxed gave wrong answer for x1"
          val _ = if not (G.is_bboxed g_disjoint x4) then ()
                  else raise ERROR "is_bboxed gave wrong answer for x4"
          val _ = if G.is_bboxed g_overlap x2 then ()
                  else raise ERROR "is_bboxed gave wrong answer for x2 (overlapping)"
        in () end) ()

  val _ = test_force "G.delete_bbox (disjoint; no such bbox)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val vs_old = G.get_vnames g
          val g' = G.delete_bbox (B.mk "B9") g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
          val _ = Testing.assert "bboxes unchanged" (B.NSet.eq bs_old bs_new)
          val _ = Testing.assert "verts unchanged" (V.NSet.eq vs_old vs_new)
        in g' end) g_disjoint

  val _ = test_force "G.delete_bbox (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.delete_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val _ = if not (B.NSet.is_empty bs_added) then
                    raise ERROR "delete_bbox managed to add something"
                  else ()
          val bs_removed = B.NSet.subtract bs_old bs_new
          val b = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "delete_bbox did not delete !-box"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,b)) then
                    raise ERROR "delete_bbox did not delete the correct !-box"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "delete_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = test_force "G.rename_bbox (disjoint; existing !-box)"
        (fn g => (G.rename_bbox b0 b1 g;
                  raise error "rename_bbox (name clash) did not throw exception")
                 handle G.bbox_already_exists_exp _ => I) g_disjoint

  val _ = test_force "G.rename_bbox (disjoint)"
        (fn g =>
        let
          val bnew = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = G.rename_bbox b0 bnew g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "rename_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val brem = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "rename_bbox did not remove old !-box"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,brem)) then
                    raise ERROR "rename_bbox did not rename the correct !-box"
                  else ()
          val _ = if not (B.name_eq (bnew,badd)) then
                    raise ERROR "rename_bbox did not use the correct name"
                  else ()
          val _ = if not (V.NSet.eq (G.get_bbox_contents g b0)
                                    (G.get_bbox_contents g' bnew)) then
                    raise ERROR "rename_bbox did not preserve contents"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "rename_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = test_force "G.try_rename1_bbox (disjoint; existing !-box)"
        (fn g => (case G.try_rename1_bbox b0 b1 g
                    of NONE => I
                     | SOME _ => raise error "try_rename1_bbox (name clash) did not return NONE")
                  ) g_disjoint

  val _ = test_force "G.try_rename1_bbox (disjoint)"
        (fn g =>
        let
          val bnew = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = case G.try_rename1_bbox b0 bnew g
                     of NONE => raise ERROR "try_rename1_bbox failed"
                      | SOME g' => g'
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "try_rename1_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val brem = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "try_rename1_bbox did not remove old !-box"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,brem)) then
                    raise ERROR "try_rename1_bbox did not rename the correct !-box"
                  else ()
          val _ = if not (B.name_eq (bnew,badd)) then
                    raise ERROR "try_rename1_bbox did not use the correct name"
                  else ()
          val _ = if not (V.NSet.eq (G.get_bbox_contents g b0)
                                    (G.get_bbox_contents g' bnew)) then
                    raise ERROR "try_rename1_bbox did not preserve contents"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "try_rename1_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = test_force "G.add_named_bbox (name clash)"
        (fn g => (G.add_named_bbox b0 g;
                  raise error "add_named_bbox (name clash) did not throw exception")
                 handle G.bbox_already_exists_exp _ => I) gv

  val _ = test_force "G.add_named_bbox (new name)"
        (fn g =>
        let
          val bnew = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = G.add_named_bbox bnew g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "add_named_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = if B.NSet.is_empty bs_removed then ()
                  else raise ERROR "add_named_bbox removed a !-box"
          val _ = if not (B.name_eq (bnew,badd)) then
                    raise ERROR "add_named_bbox did not use the correct name"
                  else ()
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "add_named_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
                  else raise ERROR "add_named_bbox changed vertices"
        in g' end) gv

  val _ = test_force "G.add_bbox"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (bnew,g') = G.add_bbox g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "add_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = if B.NSet.is_empty bs_removed then ()
                  else raise ERROR "add_bbox removed a !-box"
          val _ = if not (B.name_eq (bnew,badd)) then
                    raise ERROR "add_bbox returned the wrong name"
                  else ()
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "add_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
                  else raise ERROR "add_bbox changed vertices"
        in g' end) gv

  val _ = test_force "G.doadd_bbox"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.doadd_bbox g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val bnew = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "doadd_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = if B.NSet.is_empty bs_removed then ()
                  else raise ERROR "doadd_bbox removed a !-box"
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "doadd_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
                  else raise ERROR "doadd_bbox changed vertices"
        in g' end) gv

  val _ = test_force "G.kill_bbox (no such bbox)"
        (fn g => (G.kill_bbox (B.mk "B9") g;
                  Testing.assert "kill_bbox throws exception" false)
                 handle G.no_such_bbox_exp _ => ()) g_disjoint

  val _ = test_force "G.kill_bbox (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val vs_old = G.get_vnames g
          val g' = G.kill_bbox b0 g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val _ = Testing.assert "no !-boxes added" (B.NSet.is_empty bs_added)
          val bs_removed = B.NSet.subtract bs_old bs_new
          val b = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "kill_bbox did not delete !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox killed" (B.name_eq (b0,b))
          val vs_added = V.NSet.subtract vs_new vs_old
          val _ = Testing.assert "no verts added" (V.NSet.is_empty vs_added)
          val vs_removed = V.NSet.subtract vs_old vs_new
          val _ = Testing.assert "correct verts removed"
                (V.NSet.eq vs_removed (G.get_bbox_contents g b0))
          val _ = Testing.assert "B1 unchanged"
                (V.NSet.eq (G.get_bbox_contents g b1) (G.get_bbox_contents g' b1))
        in g' end) g_disjoint

  val _ = test_force "G.kill_bbox (overlapping)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val vs_old = G.get_vnames g
          val g' = G.kill_bbox b0 g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val _ = Testing.assert "no !-boxes added" (B.NSet.is_empty bs_added)
          val bs_removed = B.NSet.subtract bs_old bs_new
          val b = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "kill_bbox did not delete !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox killed" (B.name_eq (b0,b))
          val vs_added = V.NSet.subtract vs_new vs_old
          val _ = Testing.assert "no verts added" (V.NSet.is_empty vs_added)
          val vs_removed = V.NSet.subtract vs_old vs_new
          val _ = Testing.assert "correct verts removed"
                (V.NSet.eq vs_removed (G.get_bbox_contents g b0))
          val exp_b1 = V.NSet.subtract (G.get_bbox_contents g b1) vs_removed
          val _ = Testing.assert "Correct B1 verts removed"
                (V.NSet.eq exp_b1 (G.get_bbox_contents g' b1))
        in g' end) g_overlap

  val _ = test_force "G.drop_bbox (no such bbox)"
        (fn g => (G.drop_bbox (B.mk "B9") g;
                  Testing.assert "drop_bbox throws exception" false)
                 handle G.no_such_bbox_exp _ => ()) g_disjoint

  val _ = test_force "G.drop_bbox (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.drop_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val _ = if not (B.NSet.is_empty bs_added) then
                    raise ERROR "drop_bbox managed to add something"
                  else ()
          val bs_removed = B.NSet.subtract bs_old bs_new
          val b = case B.NSet.tryget_singleton bs_removed of
                       NONE => raise ERROR "drop_bbox did not delete !-box"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,b)) then
                    raise ERROR "drop_bbox did not delete the correct !-box"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "drop_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = test_force "G.merge_bboxes (empty set)"
        (fn g => (G.merge_bboxes B.NSet.empty g;
                  Testing.assert "merge_bboxes throws exception" false)
                 handle G.merge_bbox_exp _ => ()) g_disjoint

  val _ = test_force "G.merge_bboxes (some missing !-boxes)"
        (fn g => (G.merge_bboxes (B.NSet.of_list [b0,b1,B.mk "B9"]) g;
                  Testing.assert "merge_bboxes throws exception" false)
                 handle G.no_such_bbox_exp _ => ()) g_disjoint

  val _ = test_force "G.merge_bboxes (empty)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val bs_mrg = (B.NSet.of_list [b0,b1])
          val (b,g') = G.merge_bboxes bs_mrg g
          val bs_new = G.get_bboxes g'
          val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
          val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
          val _ = Testing.assert "verts unchanged"
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val _ = Testing.assert "!-boxes empty"
                  (B.NSet.eq (G.get_empty_bboxes g') bs_new)
        in g' end) gv

  val _ = test_force "G.merge_bboxes (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val bs_mrg = (B.NSet.of_list [b0,b1])
          val (b,g') = G.merge_bboxes bs_mrg g
          val bs_new = G.get_bboxes g'
          val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
          val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
          val _ = Testing.assert "verts unchanged"
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val vexp = V.NSet.union_merge (G.get_bbox_contents g b0)
                                        (G.get_bbox_contents g b1)
          val _ = Testing.assert "correct verts"
                  (V.NSet.eq vexp (G.get_bbox_contents g' b ))
        in g' end) g_disjoint

  val _ = test_force "G.merge_bboxes (overlapping)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val bs_mrg = (B.NSet.of_list [b0,b1])
          val (b,g') = G.merge_bboxes bs_mrg g
          val bs_new = G.get_bboxes g'
          val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
          val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
          val _ = Testing.assert "verts unchanged"
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val vexp = V.NSet.union_merge (G.get_bbox_contents g b0)
                                        (G.get_bbox_contents g b1)
          val _ = Testing.assert "correct verts"
                  (V.NSet.eq vexp (G.get_bbox_contents g' b ))
        in g' end) g_overlap

  val _ = test_force "G.fresh_copy1_bbox (no such !-boxes)"
        (fn g => (G.fresh_copy1_bbox (B.mk "B9") g;
                  Testing.assert "fresh_copy1_bbox throws exception" false)
                 handle G.no_such_bbox_exp _ => ()) g_disjoint

  val _ = test_force "G.fresh_copy1_bbox (empty)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "verts unchanged"
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val _ = Testing.assert "!-boxes empty"
                  (B.NSet.eq (G.get_empty_bboxes g') bs_new)
        in g' end) gv

  val _ = test_force "G.fresh_copy1_bbox (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
          val _ = Testing.assert "b1 untouched"
                  (V.NSet.eq (G.get_bbox_contents g b1) (G.get_bbox_contents g' b1))
        in g' end) g_disjoint

  val _ = test_force "G.fresh_copy1_bbox (overlapping)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
          val _ = Testing.assert "old b1 verts remain"
                  (V.NSet.sub_set (G.get_bbox_contents g b1)
                                  (G.get_bbox_contents g' b1))
          val b1s_new = V.NSet.subtract (G.get_bbox_contents g' b1)
                                        (G.get_bbox_contents g b1)
          val b1_new = case V.NSet.tryget_singleton b1s_new
                         of NONE => raise ERROR "b1 is not correct size"
                          | SOME x => x
          val _ = Testing.assert "b1 new vert correct"
                  (G.VData.data_eq (G.get_vertex_data g' b1_new,
                                    G.get_vertex_data g x2))
        in g' end) g_overlap

  val g = G.empty
  (* circle in a !-box *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val g      = g |> G.add_named_bbox b0
  val g      = g |> G.add_to_bbox b0 (V.NSet.single v1)
  val circles_pat = g

  val _ = test_force "G.fresh_copy1_bbox (circle)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
        in g' end) circles_pat

  val _ = test_force "G.fresh_expand1_bbox (circle)"
        (fn _ =>
        let
          val g = circles_pat
          val bs_old = G.get_bboxes g
          val g' = G.fresh_expand1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val _ = Testing.assert "bboxes unchanged"
                  (B.NSet.eq bs_old bs_new)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "old bbox the same"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "correct number of verts"
                  ((V.NSet.cardinality (G.get_vnames g')) = 2)
          val _ = Testing.assert "correct number of edges"
                  ((E.NSet.cardinality (G.get_enames g')) = 2)
        in g' end) ()

  val g = G.empty
  (* RHS of spider pattern *)
  val (v1,g) = g |> G.add_vertex bvert
  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v2
  val (_ ,g) = g |> G.add_edge dir_edge v2 v3
  val g      = g |> G.add_named_bbox b0
                 |> G.add_named_bbox b1
                 |> G.add_to_bbox b0 (V.NSet.single v1)
                 |> G.add_to_bbox b1 (V.NSet.single v3)
  val spider_rhs_pat = g

  val _ = test_force "G.fresh_copy1_bbox (spider)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val _ = Testing.assert "b0 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "b1 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b1)
                             (G.get_bbox_contents g' b1))
          val vadd = case V.NSet.tryget_singleton (G.get_bbox_contents g' b) of
                       NONE => raise ERROR "new !-box has wrong number of vertices"
                     | SOME v' => v'
          val _ = Testing.assert "New !-box is connected"
                  (G.has_edges_between g' vadd v2)
        in g' end) spider_rhs_pat

  val _ = test_force "G.fresh_expand1_bbox (spider)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.fresh_expand1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val _ = Testing.assert "bboxes unchanged"
                  (B.NSet.eq bs_old bs_new)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "b0 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "b1 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b1)
                             (G.get_bbox_contents g' b1))
          val newverts = V.NSet.subtract (G.get_vnames g') (G.get_vnames g)
          val vadd = case V.NSet.tryget_singleton newverts of
                       NONE => raise ERROR "wrong number of vertices added"
                     | SOME v' => v'
          val _ = Testing.assert "New vertex is connected"
                  (G.has_edges_between g' vadd v2)
        in g' end) spider_rhs_pat
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR !-BOX FUNCTIONS PASSED!"
end; (* ends local namespace used for testing *)


local
  open Test_MkG
  val g1 = G.empty
  val (n_a_1, g1) = g1 |> G.add_vertex (mkZ zero_angle);
  val (n_b_1, g1) = g1 |> G.add_vertex mkH;
  val (_, g1) = g1 |> G.add_edge undir_edge n_a_1 n_b_1;
  val (_, g1) = g1 |> G.add_edge undir_edge n_a_1 n_b_1;

  val g2 = G.empty
  val (n_a_2, g2) = g2 |> G.add_vertex mkH;
  val (n_b_2, g2) = g2 |> G.add_vertex (mkZ zero_angle);
  val (_, g2) = g2 |> G.add_edge undir_edge n_a_2 n_b_2;
  val (_, g2) = g2 |> G.add_edge undir_edge n_a_2 n_b_2;

  val g3 = g1
  val (_, g3) = g3 |> G.add_bbox;
  val (b1_3, g3) = g3 |> G.add_bbox;
  val g3 = g3 |> G.add_to_bbox b1_3 (V.NSet.single n_a_1);

  val g4 = g2
  val (b0_4, g4) = g4 |> G.add_bbox;
  val (_, g4) = g4 |> G.add_bbox;
  val g4 = g4 |> G.add_to_bbox b0_4 (V.NSet.single n_b_2);

  fun seq_count sq =
        (case Seq.pull sq of
              NONE   => 0
            | SOME (_,sq') => 1 + (seq_count sq'));
  fun assert_iso_count 0 iso =
        (case Seq.pull iso of
              NONE   => ()
            | SOME (_,iso') => raise ERROR ("Too many isomorphisms found (" ^
            (Int.toString (seq_count iso')) ^ " too many"))
    | assert_iso_count n iso =
        (case Seq.pull iso of
              NONE   => raise ERROR ("Not enough isomorphisms found (needed " ^
              (Int.toString n) ^ " more)")
            | SOME (_,iso') => assert_iso_count (n-1) iso');

  val _ = Testing.test "GIso.test [self; no !-boxes]"
          (fn () => if GIso.test g1 g1 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [self; no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g1 g1)) ()

  val _ = Testing.test "GIso.test [g1,g2: no !-boxes]"
          (fn () => if GIso.test g1 g2 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [g1,g2: no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g1 g2)) ()

  val _ = Testing.test "GIso.test [self; disjoint !-boxes]"
          (fn () => if GIso.test g3 g3 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [self; disjoint no !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g3 g3)) ()

  val _ = Testing.test "GIso.test [g3,g4: disjoint !-boxes]"
          (fn () => if GIso.test g3 g4 then () else raise ERROR ("Should be iso")) ()
  val _ = Testing.test "GIso.get [g3,g4: disjoint !-boxes]"
          (fn () => assert_iso_count 2 (GIso.get g3 g4)) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR !-GRAPH ISOS PASSED!"
end

(* For debugging...

PolyML.exception_trace (fn () => 
  
) ;
*)
