structure TheoryHack 
= struct

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
  val edge : UnitEdge.data = ();

(* Isometry *)
local 
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
in 
  val isometry_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print isometry_rule;
end;      

(* Frobenius *)
local 
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
in 
  val frobenius_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print frobenius_rule;
end;      

(* Compact Structure *)
local 
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
in 
  val compact_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print compact_rule;
end;      


(* Abelian Unit 1 *)
local 
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
in 
  val abelian1_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print abelian1_rule;
end      
  (* Abelian Unit 2: is a meta-operation and is not lifted to graph patterns *)

(* Abelian Unit 3 *)
local 
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
in
  val abelian3_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print abelian3_rule;
end;

(* Bialgebra 1 *)
local 
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
in
  val bialgebra1_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print bialgebra1_rule;
end;
(* Bialgebra 2 *)
local 
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
in
  val bialgebra2_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print bialgebra2_rule;
end;

(* Bialgebra 3 *)
local 
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
in
  val bialgebra3_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print bialgebra3_rule;
end;    

(* in *)
val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.")

val rules = [("isometry",isometry_rule),
             ("frobenius",frobenius_rule),
             ("compact",compact_rule),
             
             ("abelian1",abelian1_rule),
             (* no abelian2 rule: not needed for our rep *)
             ("abelian3",abelian3_rule),
             
             ("bialgebra1",bialgebra1_rule),
             ("bialgebra2",bialgebra2_rule),
             ("bialgebra3",bialgebra3_rule)
            ];

fun apply_all_rules g = 
    maps
      (fn (n,r) => map (fn r2 => (n,r2)) (Seq.list_of (Rule.rule_matches r g)))
      rules;

end;
