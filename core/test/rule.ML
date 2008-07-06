(* local *)

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

(* Isometry *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 b2;
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
val rhs = g;
val _ = RGGraph.print rhs;
val isometry_rule = Rule.mk (lhs,rhs);
val _ = Rule.print isometry_rule;


(* Frobenius *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b3, g) = g |> RGGraph.add_vertex bvert;
val (b4, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b3 n1;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 b2;
val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val g = g |> RGGraph.new_named_vertex b3 bvert;
val g = g |> RGGraph.new_named_vertex b4 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 b3;
val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
val rhs = g;
val _ = RGGraph.print rhs;
val frobenius_rule = Rule.mk (lhs,rhs);
val _ = Rule.print frobenius_rule;


(* Compact Structure *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 b1;
val (e2, g) = g |> RGGraph.add_edge edge n2 n1;
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
val rhs = g;
val _ = RGGraph.print rhs;
val compact_rule = Rule.mk (lhs,rhs);
val _ = Rule.print compact_rule;



(* Abelian Unit 1 *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
val rhs = g;
val _ = RGGraph.print rhs;
val abelian1_rule = Rule.mk (lhs,rhs);
val _ = Rule.print abelian1_rule;

(* Abelian Unit 2: is a meta-operation and is not lifted to graph patterns *)

(* Abelian Unit 3 *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
val (n2, g) = g |> RGGraph.add_vertex (mkX (var_angle "b"));
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
val (e2, g) = g |> RGGraph.add_edge edge n2 n1;
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX (add_angles (var_angle "a") (var_angle "b")));
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
val rhs = g;
val _ = RGGraph.print rhs;
val abelian3_rule = Rule.mk (lhs,rhs);
val _ = Rule.print abelian3_rule;

(* Bialgebra 1 *)
(* lhs *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (b2, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (b3, g) = g |> RGGraph.add_vertex bvert;
val (b4, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
val (e2, g) = g |> RGGraph.add_edge edge b3 n3;
val (e3, g) = g |> RGGraph.add_edge edge b4 n4;
val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
val (n5, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
val (n6, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
val (e8, g) = g |> RGGraph.add_edge edge n5 n6; (* scalar XZ *)
val lhs = g;
val _ = RGGraph.print lhs;
(* rhs *)
val g = RGGraph.empty;
val g = g |> RGGraph.new_named_vertex b1 bvert;
val g = g |> RGGraph.new_named_vertex b2 bvert;
val g = g |> RGGraph.new_named_vertex b3 bvert;
val g = g |> RGGraph.new_named_vertex b4 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 b3;
val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
val rhs = g;
val _ = RGGraph.print rhs;
val bialgebra1_rule = Rule.mk (lhs,rhs);
val _ = Rule.print bialgebra1_rule;

(* Bialgebra 2 *)
(* lhs *)
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
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
val rhs = g;
val _ = RGGraph.print rhs;
val bialgebra2_rule = Rule.mk (lhs,rhs);
val _ = Rule.print bialgebra2_rule;

(* Bialgebra 3 *)
(* lhs *)
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
val g = g |> RGGraph.new_named_vertex b2 bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
val rhs = g;
val _ = RGGraph.print rhs;
val bialgebra3_rule = Rule.mk (lhs,rhs);
val _ = Rule.print bialgebra3_rule;

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
val [] = map Rule.print (Seq.list_of (Rule.rule_matches isometry_rule g));


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


val [m1,m2] = Seq.list_of (Rule.matches isometry_rule g);
val [r1,r2] = map Rule.print (Seq.list_of (Rule.rule_matches isometry_rule g));
val [g1,g2] = map RGGraph.print (Seq.list_of (Rule.rewrites isometry_rule g));

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

val [r1,r2] = Seq.list_of (Rule.rule_matches isometry_rule g);
val _ = map Rule.print [r1,r2];
val fused_g1 = Rule.add_rhs r1 g;
val fused_g2 = Rule.add_rhs r2 g;
val _ = RGGraph.print fused_g1;
val _ = RGGraph.print fused_g2;

(* raise ERROR "stop"; *)


(* in *)
val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.")
val rule_list_hack = [("isometry",isometry_rule),
                      ("frobenius",frobenius_rule),
                      ("compact",compact_rule),
                
                      ("abelian1",abelian1_rule),
                      (* no abelian2 rule: not needed for our rep *)
                      ("abelian3",abelian3_rule),

                      ("bialgebra1",bialgebra1_rule),
                      ("bialgebra2",bialgebra2_rule),
                      ("bialgebra3",bialgebra3_rule)
                     ];

(* end; *)





(*
    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)

