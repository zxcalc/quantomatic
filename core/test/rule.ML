structure DB_Test_Rule 
= struct


structure V = RGGraph.BVertex;
structure E = RGGraph.Edge;

val bvert = V.BVert ();
val zero_angle = AngleExpr.zero;
val var_angle = AngleExpr.mk_var;
val add_angles = AngleExpr.add_expr;
val pi_angle = AngleExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();

(* Example of a rule that is not valid *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *)
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val rhs = g;
val _ = RGGraph.print rhs;
val _ = 
    ((Rule.mk (lhs,rhs); 
      raise ERROR "This rule is bad, it should raise an exception, but did not! :( ")
     handle (Rule.bad_rule_exp _) => 
            Pretty.writeln (Pretty.str "Bad rule was correctly judged to be bad."));


(* Example of applications of rule that does not match *)
val g = RGGraph.empty;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge n1 n3;
val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
val _ = RGGraph.print g;
val [] = map Rule.print (Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g));


(* test rewriting isometry lhs with isometry rule *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 b2;
val _ = RGGraph.print g;


val [m1,m2] = Seq.list_of (Rule.matches TheoryHack.isometry_rule g);
val [r1,r2] = map Rule.print (Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g));
val [g1,g2] = map RGGraph.print (Seq.list_of (Rule.rewrites TheoryHack.isometry_rule g));

(* Example of applications of isometry rule *)
val g = RGGraph.empty;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n5, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n1 n3;
val (e4, g) = g |> RGGraph.add_edge edge n2 n4;
val _ = RGGraph.print g;

val [r1,r2] = Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g);
val _ = map Rule.print [r1,r2];
val fused_g1 = Rule.add_rhs r1 g;
val fused_g2 = Rule.add_rhs r2 g;
val _ = RGGraph.print fused_g1;
val _ = RGGraph.print fused_g2;



(* Example of not applicable rules... *)
val g = RGGraph.empty;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n3, g) = g |> RGGraph.add_vertex bvert;
val (n4, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
val (e2, g) = g |> RGGraph.add_edge edge n2 n4;
val _ = RGGraph.print g;

val [] = Seq.list_of (Rule.rule_matches TheoryHack.frobenius_rule g);
val [] = Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g);

val L = TheoryHack.apply_all_rules g;

end;

(*
val L2 = Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g);

    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)

