structure Test_RG_Rule : RULE = Rule(Test_RG_Graph);

local
open Test_RG_MkG;
structure Rule = Test_RG_Rule;

val b1 = V.mk "b1"
val b2 = V.mk "b2"
val x1 = V.mk "x1"
val x2 = V.mk "x2"
val z1 = V.mk "z1"
val z2 = V.mk "z2"

fun test_mk_bad_rule (lhs,rhs) = 
   (Rule.mk (lhs,rhs);
    writeln "LHS:";
    G.print lhs;
    writeln "RHS:";
    G.print rhs;
    raise ERROR "Bad rule accepted")
   handle (Rule.bad_rule_exp _) => ();


val _ = Testing.test "empty graphs" Rule.mk (G.empty,G.empty);


val g = G.empty;
val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
val single_z = g;
val g = G.empty;
val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
          |> G.add_named_vertex x1 (mkX zero_angle)
          |> G.doadd_edge dir_edge z1 x1;
val z_x = g;
val _ = Testing.test "empty boundaries" Rule.mk (single_z,z_x);


val g = G.empty;
val g = g |> G.add_named_vertex b1 bvert
          |> G.add_named_vertex z1 (mkZ zero_angle)
          |> G.doadd_edge dir_edge b1 z1;
val z_counit = g;
val g = G.empty;
val g = g |> G.add_named_vertex b1 bvert
          |> G.add_named_vertex x1 (mkX zero_angle)
          |> G.doadd_edge dir_edge b1 x1;
val x_counit = g;
val _ = Testing.test "one input, no outputs" Rule.mk (z_counit,x_counit);



val g = G.empty;
val g = g |> G.add_named_vertex b1 bvert
          |> G.add_named_vertex b2 bvert
          |> G.add_named_vertex x1 (mkX zero_angle)
          |> G.add_named_vertex x2 (mkX zero_angle)
          |> G.add_named_vertex z1 (mkZ zero_angle)
          |> G.add_named_vertex z2 (mkZ zero_angle)
          |> G.doadd_edge undir_edge z1 x1
          |> G.doadd_edge undir_edge b1 x1
          |> G.doadd_edge undir_edge b2 x1
          |> G.doadd_edge undir_edge x2 z2;
val two_undir_bounds = g;
val g = G.empty;
val g = g |> G.add_named_vertex b1 bvert
          |> G.add_named_vertex z1 (mkZ zero_angle)
          |> G.add_named_vertex z2 (mkZ zero_angle)
          |> G.doadd_edge undir_edge b1 z1
val one_undir_bound = g;
val _ = Testing.test "incoherent boundaries"
    test_mk_bad_rule (two_undir_bounds,one_undir_bound);

in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR rule PASSED!";
end;
