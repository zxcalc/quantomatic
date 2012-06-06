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
  fun subst_xname_in_edata n1 n2 d = d;
  
  fun get_xnames vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun dep_vnames_of_xname vdep n = VDep.codf vdep n;
  fun dep_enames_of_xname vdep n = E.NSet.empty;

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

  fun rename_ename n1 n2 vdep = vdep;
  
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
  fun add_edge ed en vdep = vdep;

  fun delete_vname vn vdep = VDep.del_dom vn vdep;
  fun delete_ename en vdep = vdep;

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
  fun replace_edge ed en vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge en ed = empty;
  
  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = VData.subst; (* : LinratAngleExpr.T X.NTab.T; *)

  val empty_subst = VData.empty_subst; (* : X.NTab.empty; *)
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge e1 e2 u = SOME u;

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
  fun subst_in_edge u ed = ed;

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
end;


(* test some basic graph constructions *)
local

open Test_MkG;

(* a simple graph, and some basic manipulations *)
val g = G.empty;
val (n_a, g) = g |> G.add_vertex bvert;
val (n_b, g) = g |> G.add_vertex bvert;
val (e0, g) = g |> G.add_edge undir_edge n_a n_b;
val (e1, g) = g |> G.add_edge undir_edge n_a n_b;
val n_c = (V.mk "n_c");
val g = g |> G.rename_vname n_a n_c;
val e3 = (E.mk "e_c");
val g = g |> G.rename_ename e0 e3;

(* 3 CNOTs = swap example *)
val g2 = G.empty;
val (in1, g2)  = g2 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g2)  = g2 |> G.add_named_vertex (V.mk "in2") bvert;  
val (out1, g2)  = g2 |> G.add_named_vertex (V.mk "out1") bvert;
val (out2, g2)  = g2 |> G.add_named_vertex (V.mk "out2") bvert;

val (green1, g2) = g2 |> G.add_named_vertex (V.mk "green1") (mkZ zero_angle);
val (green2, g2) = g2 |> G.add_named_vertex (V.mk "green2") (mkZ zero_angle);
val (green3, g2) = g2 |> G.add_named_vertex (V.mk "green3") (mkZ zero_angle);

val (red1, g2) = g2 |> G.add_named_vertex (V.mk "red1") (mkX zero_angle);
val (red2, g2) = g2 |> G.add_named_vertex (V.mk "red2") (mkX zero_angle);
val (red3, g2) = g2 |> G.add_named_vertex (V.mk "red3") (mkX zero_angle);

val (e0, g2) = g2 |> G.add_edge undir_edge in1 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge in2 red1;
val (e0, g2) = g2 |> G.add_edge undir_edge red1 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge red1 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green1;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red3 green2;
val (e0, g2) = g2 |> G.add_edge undir_edge red2 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge red3 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge out1 green3;
val (e0, g2) = g2 |> G.add_edge undir_edge out2 red3;

(* just a swap *)

val g3 = G.empty;
val (in1, g3)  = g3 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g3)  = g3 |> G.add_named_vertex (V.mk "in2") bvert;

val (out1, g3)  = g3 |> G.add_named_vertex (V.mk "out1")  
					       bvert;
val (out2, g3)  = g3 |> G.add_named_vertex (V.mk "out2")
					       bvert;
val (e0, g3) = g3 |> G.add_edge undir_edge in1 out2;
val (e0, g3) = g3 |> G.add_edge undir_edge in2 out1;


(*  A MC pattern for a CNOT *)
val g4 = G.empty;
val (in1, g4)  = g4 |> G.add_named_vertex (V.mk "in1") bvert;
val (in2, g4)  = g4 |> G.add_named_vertex (V.mk "in2") bvert;

val (out1, g4)  = g4 |> G.add_named_vertex (V.mk "out1") bvert;
val (out2, g4)  = g4 |> G.add_named_vertex (V.mk "out2") bvert;

val (green1, g4) = g4 |> G.add_named_vertex (V.mk "green1") (mkZ zero_angle);
val (green2, g4) = g4 |> G.add_named_vertex (V.mk "green2") (mkZ zero_angle);
val (green3, g4) = g4 |> G.add_named_vertex (V.mk "green3") (mkZ zero_angle);
val (green4, g4) = g4 |> G.add_named_vertex (V.mk "green4") (mkZ zero_angle);
val (green5, g4) = g4 |> G.add_named_vertex (V.mk "green5") (mkZ zero_angle);
val (green6, g4) = g4 |> G.add_named_vertex (V.mk "green6") (mkZ zero_angle);
val (green7, g4) = g4 |> G.add_named_vertex (V.mk "green7") (mkZ zero_angle);
val (green8, g4) = g4 |> G.add_named_vertex (V.mk "green8") (mkZ zero_angle);
val (green9, g4) = g4 |> G.add_named_vertex (V.mk "green9") (mkZ zero_angle);
val (green10, g4) = g4 |> G.add_named_vertex (V.mk "green10") (mkZ zero_angle);

val (H1, g4) = g4 |> G.add_named_vertex (V.mk "H1") mkH;
val (H2, g4) = g4 |> G.add_named_vertex (V.mk "H2") mkH;
val (H3, g4) = g4 |> G.add_named_vertex (V.mk "H3") mkH;

val (e0, g4) = g4 |> G.add_edge undir_edge in1 green2;
val (e0, g4) = g4 |> G.add_edge undir_edge in2 green3;
val (e0, g4) = g4 |> G.add_edge undir_edge green3 green1;
val (e0, g4) = g4 |> G.add_edge undir_edge green2 out1;
val (e0, g4) = g4 |> G.add_edge undir_edge green2 H1;
val (e0, g4) = g4 |> G.add_edge undir_edge H1 green5;
val (e0, g4) = g4 |> G.add_edge undir_edge green5 green4;
val (e0, g4) = g4 |> G.add_edge undir_edge green5 green6;
val (e0, g4) = g4 |> G.add_edge undir_edge green3 H2;
val (e0, g4) = g4 |> G.add_edge undir_edge H2 green6;
val (e0, g4) = g4 |> G.add_edge undir_edge green6 green7;
val (e0, g4) = g4 |> G.add_edge undir_edge green7 green8;
val (e0, g4) = g4 |> G.add_edge undir_edge green7 H3;
val (e0, g4) = g4 |> G.add_edge undir_edge H3 green9;
val (e0, g4) = g4 |> G.add_edge undir_edge green9 green10;
val (e0, g4) = g4 |> G.add_edge undir_edge green9 out2;

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
    val g = G.empty;
    val (n_a, g) = g |> G.add_vertex bvert;
    val (n_b, g) = g |> G.add_vertex bvert;
    val (n_x, g) = g |> G.add_named_vertex (V.mk "with x") 
                                           (mkX (parse_angle "2/3*x"));
    val (e0, g) = g |> G.add_edge undir_edge n_a n_x;
    val (e1, g) = g |> G.add_edge undir_edge n_x n_b;
  
    val _ = Testing.test "Single variable in dependencies" (fn () => let 
      val true = X.name_eq (the (X.NSet.tryget_singleton (G.get_xnames g)), 
                            (X.mk "x"))
      in () end) ();
    
    val _ = Testing.test "vname dep is the right singleton" (fn () => let 
      val dep_v_name = the (V.NSet.tryget_singleton 
          (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "x"))); 
      val true = V.name_eq (n_x, dep_v_name); 
      in () end) ();
      
    val n_x2 = (V.mk "n_x2");
    val g = g |> G.rename_vname n_x n_x2;
    val g = g |> G.rename_ename e0 (E.mk "e_c");
    val _ = (writeln "g before renaming: "; G.print g);
    val g = g |> G.rename_xname (X.mk "x") (X.mk "y");
    val _ = (writeln "g after renaming: "; G.print g);
    
    val _ = Testing.test "vname dep is the right singleton after renaming" 
    (fn () => let 
      val dep_v_name = the (V.NSet.tryget_singleton 
          (G.Param.dep_vnames_of_xname (G.get_param g) (X.mk "y"))); 
      val true = V.name_eq (n_x2, dep_v_name); 
      in () end) ();
  in (g,n_x2) end;

  val _ = Testing.test "Single variable was renamed in dependencies" 
  (fn () => let 
    val true = X.name_eq (the (X.NSet.tryget_singleton (G.get_xnames g)), 
                          (X.mk "y")) 
    val (d,ios) = G.get_vertex g n_x2;
    val y =  X.name_eq (the (X.NSet.tryget_singleton 
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
    val (d,ios) = G.get_vertex g' x1;
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
  val g = G.empty
  val (b1,g) = g|> G.add_named_vertex (V.mk "b1") bvert
  val (b2,g) = g|> G.add_named_vertex (V.mk "b2") bvert
  val (b3,g) = g|> G.add_named_vertex (V.mk "b3") bvert
  val (b4,g) = g|> G.add_named_vertex (V.mk "b4") bvert
  val (b5,g) = g|> G.add_named_vertex (V.mk "b5") bvert
  val (b6,g) = g|> G.add_named_vertex (V.mk "b6") bvert
  
  val (c1,g) = g|> G.add_named_vertex (V.mk "c1") bvert
  val (c2,g) = g|> G.add_named_vertex (V.mk "c2") bvert
  val (c3,g) = g|> G.add_named_vertex (V.mk "c3") bvert
  
  val (r1,g) = g|> G.add_named_vertex (V.mk "r1") (mkX zero_angle)
  val (r2,g) = g|> G.add_named_vertex (V.mk "r2") (mkX zero_angle)
  val (r3,g) = g|> G.add_named_vertex (V.mk "r3") (mkX zero_angle)
  
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
  val _ = Testing.test "G.get_wire_list"
  (fn () => c_wire_lists wires wires') ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR WIRE FUNCTIONS PASSED!"
end

(* For debugging...

PolyML.exception_trace (fn () => 
  
) ;
*)
