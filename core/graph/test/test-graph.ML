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
val g2 = g2 |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex green1 (mkZ zero_angle)
            |> G.add_named_vertex green2 (mkZ zero_angle)
            |> G.add_named_vertex green3 (mkZ zero_angle)
            |> G.add_named_vertex red1 (mkX zero_angle)
            |> G.add_named_vertex red2 (mkX zero_angle)
            |> G.add_named_vertex red3 (mkX zero_angle)
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
val g3 = g3 |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.doadd_edge undir_edge in1 out2
            |> G.doadd_edge undir_edge in2 out1;

(*  A MC pattern for a CNOT *)
val g4 = G.empty;
val g4 = g4 |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex green1 (mkZ zero_angle)
            |> G.add_named_vertex green2 (mkZ zero_angle)
            |> G.add_named_vertex green3 (mkZ zero_angle)
            |> G.add_named_vertex green4 (mkZ zero_angle)
            |> G.add_named_vertex green5 (mkZ zero_angle)
            |> G.add_named_vertex green6 (mkZ zero_angle)
            |> G.add_named_vertex green7 (mkZ zero_angle)
            |> G.add_named_vertex green8 (mkZ zero_angle)
            |> G.add_named_vertex green9 (mkZ zero_angle)
            |> G.add_named_vertex green10 (mkZ zero_angle)
            |> G.add_named_vertex H1 mkH
            |> G.add_named_vertex H2 mkH
            |> G.add_named_vertex H3 mkH
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
    val g = g |> G.add_named_vertex n_a bvert
              |> G.add_named_vertex n_b bvert
              |> G.add_named_vertex n_x (mkX (parse_angle "2/3*x"))
              |> G.add_named_edge e0 undir_edge n_a n_x
              |> G.add_named_edge e1 undir_edge n_x n_b
  
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
  
  val _ = Testing.test "After renaming, node vertex data was updated" (fn () => let 
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

