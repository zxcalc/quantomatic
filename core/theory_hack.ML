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

(* X Isometry *)
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
  val x_isometry_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_isometry_rule;
end;      

(* Z Isometry *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
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
  val z_isometry_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print z_isometry_rule;
end;      


(* frobenious is an identity in graphs with variable-nodes, 
   not added as a rewrite, here for testing only *)
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
  val x_frobenius_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_frobenius_rule;
end;      


(* X Compact Structure *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
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
  val x_compact_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_compact_rule;
end;      



(* Z Compact Structure *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
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
  val z_compact_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print z_compact_rule;
end;      



(* X Abelian Unit 1 *)
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
  val x_abelian1_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_abelian1_rule;
end;


(* Z Abelian Unit 1 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
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
  val z_abelian1_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print z_abelian1_rule;
end;

  (* Abelian Unit 2: is a meta-operation and is not lifted to graph patterns *)

(* X Abelian Unit 3 *)
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
  val x_abelian3_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_abelian3_rule;
end;



(* Z Abelian Unit 3 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (n2, g) = g |> RGGraph.add_vertex (mkZ (var_angle "b"));
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
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (add_angles (var_angle "a") (var_angle "b")));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val z_abelian3_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print z_abelian3_rule;
end;





(* (X and Z symmetric) Bialgebra 1 *)
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
(* no scalars in theory hack - they are not really important *)
(*   val (n5, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n6, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e8, g) = g |> RGGraph.add_edge edge n5 n6; (* scalar XZ *)
*)
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
  val xz_bialgebra1_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print xz_bialgebra1_rule;
end;




(* (X and Z symmetric) Bialgebra 1 : spider_extension(+1,0) *)
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
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b3 n3;
  val (e3, g) = g |> RGGraph.add_edge edge b4 n4;
  val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
  val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
  val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
(* no scalars in theory hack - they are not really important *)
(*   val (n5, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n6, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e8, g) = g |> RGGraph.add_edge edge n5 n6; (* scalar XZ *)
*)
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
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e3, g) = g |> RGGraph.add_edge edge n2 b3;
  val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val xz_bialgebra1_s1_0_rule = Rule.mk (lhs,rhs);
end;



(* (X and Z symmetric) Bialgebra 1 : spider_extension(+2,+0) *)
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
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b3 n3;
  val (e3, g) = g |> RGGraph.add_edge edge b4 n4;
  val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
  val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
  val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
(* no scalars in theory hack - they are not really important *)
(*   val (n5, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n6, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e8, g) = g |> RGGraph.add_edge edge n5 n6; (* scalar XZ *)
*)
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
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e3, g) = g |> RGGraph.add_edge edge n2 b3;
  val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val xz_bialgebra1_s2_0_rule = Rule.mk (lhs,rhs);
end;





(* Hopf-law 1_1 (derivable from bi-algebra, but needed for rewrite system) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hopf_1_1_rule = Rule.mk (lhs,rhs);
end;

(* Hopf-law 1_2 (derivable from bi-algebra, but needed for rewrite system) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (b22, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b22 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b22 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b22;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hopf_1_2_rule = Rule.mk (lhs,rhs);
end;


(* Hopf-law 2_1 (derivable from bi-algebra, but needed for rewrite system) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b11, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b11 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b11 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b11 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hopf_2_1_rule = Rule.mk (lhs,rhs);
end;

(* Hopf-law 2_2 (derivable from bi-algebra, but needed for rewrite system) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b11, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (b22, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b11 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val g = g |> RGGraph.new_named_vertex b22 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b11 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b22;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hopf_2_2_rule = Rule.mk (lhs,rhs);
end;


(* X - Bialgebra 2 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val x_bialgebra2_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print x_bialgebra2_rule;
end;


(* Z - Bialgebra 2 *)
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
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
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
  val z_bialgebra2_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print z_bialgebra2_rule;
end;


(* Z - Bialgebra 2 : spider_extension(+1,0) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val z_bialgebra2_s1_0_rule = Rule.mk (lhs,rhs);
end;

(* Z - Bialgebra 2 : spider_extension(+1,+1) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e21, g) = g |> RGGraph.add_edge edge b2 n2;
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e12, g) = g |> RGGraph.add_edge edge b2 n2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val z_bialgebra2_s1_1_rule = Rule.mk (lhs,rhs);
end;


(* X - Bialgebra 2 : spider_extension(+1,0) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val x_bialgebra2_s1_0_rule = Rule.mk (lhs,rhs);
end;



(* X - Bialgebra 2 : spider_extension(+1,+1) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e21, g) = g |> RGGraph.add_edge edge b2 n2;
(* no scalars in theory hack - they are not really important *)
(*   val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
  val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
  val (e5, g) = g |> RGGraph.add_edge edge n3 n4; (* scalar XZ *) *)
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e11, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n2;
  val (e12, g) = g |> RGGraph.add_edge edge b2 n2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val x_bialgebra2_s1_1_rule = Rule.mk (lhs,rhs);
end;





(* adj-Hadamards *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n2, g) = g |> RGGraph.add_vertex (mkH);
  val (n3, g) = g |> RGGraph.add_vertex (mkH);
  val (b4, g) = g |> RGGraph.add_vertex bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e2, g) = g |> RGGraph.add_edge edge n3 b4;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b4 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b4;
  val rhs = g;
in
  val hh_to_id_rule = Rule.mk (lhs,rhs);
end;



(* HHHX to Z - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (b3, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (h2, g) = g |> RGGraph.add_vertex (mkH);
  val (h3, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 h2;
  val (e2, g) = g |> RGGraph.add_edge edge h3 b3;
  val (e0, g) = g |> RGGraph.add_edge edge h1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge h2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 h3;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val g = g |> RGGraph.new_named_vertex b3 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 b3;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hhhx_to_z_rule = Rule.mk (lhs,rhs);
end;


(* HHHZ to X - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (b3, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (h2, g) = g |> RGGraph.add_vertex (mkH);
  val (h3, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 h2;
  val (e2, g) = g |> RGGraph.add_edge edge h3 b3;
  val (e0, g) = g |> RGGraph.add_edge edge h1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge h2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 h3;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val g = g |> RGGraph.new_named_vertex b3 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 b3;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hhhz_to_x_rule = Rule.mk (lhs,rhs);
end;




(* HXH to Z - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (h2, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge h1 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 h2;
  val (e0, g) = g |> RGGraph.add_edge edge h2 b2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 b2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hxh_to_z_rule = Rule.mk (lhs,rhs);
end;

(* HZH to X - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (h2, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge h1 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 h2;
  val (e0, g) = g |> RGGraph.add_edge edge h2 b2;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 b2;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hzh_to_x_rule = Rule.mk (lhs,rhs);
end;




(* HZ to X - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge h1 n1;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hz_to_x_rule = Rule.mk (lhs,rhs);
end;


(* HX to Z - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge h1 n1;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val hx_to_z_rule = Rule.mk (lhs,rhs);
end;



(*  *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (e1, g) = g |> RGGraph.add_edge edge b1 b1;
  val lhs = g;
  val _ = RGGraph.print lhs;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val rhs = g;
  val _ = RGGraph.print rhs;
in
  val drop_self_loop_rule = Rule.mk (lhs,rhs);
end;




 

(* in *)
val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.");

val rules = [(* ("x_isometry",x_isometry_rule),
             ("z_isometry",z_isometry_rule),
             
             (* no frobenius: it's an id in our rep *)
             
             ("x_compact",x_compact_rule),
             ("z_compact",z_compact_rule),
             *)

             (* abelian1 is special case of spider *)
             ("x_abelian1",x_abelian1_rule),
             (* no abelian2: not needed for our rep *)
             (* ("z_abelian3",z_abelian3_rule), *)
                   
             ("z_abelian1",z_abelian1_rule),
             (* no abelian2: not needed for our rep *)
             (* ("z_abelian3",z_abelian3_rule), *)

             (* basic-bialgebra laws *)
             ("xz_bialgebra1",xz_bialgebra1_rule),
             ("x_bialgebra2",x_bialgebra2_rule),
             ("z_bialgebra2",z_bialgebra2_rule),

             (* hopf laws *)
             ("hopf_1_1",hopf_1_1_rule),
             ("hopf_1_2",hopf_1_2_rule),
             ("hopf_2_1",hopf_2_1_rule),
             ("hopf_2_2",hopf_2_2_rule),

             (* hadamard rules *)
             ("hh_to_id",hh_to_id_rule),

             ("hx_to_z",hx_to_z_rule),
             ("hz_to_x",hz_to_x_rule),

             ("hxh_to_z",hxh_to_z_rule),
             ("hzh_to_x",hzh_to_x_rule),

             ("hhhx_to_z",hhhx_to_z_rule),
             ("hhhz_to_x",hhhz_to_x_rule),

             ("drop_self_loop", drop_self_loop_rule)

             (* bialgebra-spiders *)
(*              ("x_bialgebra2_s1_0",x_bialgebra2_s1_0_rule),
             ("x_bialgebra2_s1_1",x_bialgebra2_s1_1_rule),
             ("z_bialgebra2_s1_0",z_bialgebra2_s1_0_rule),
             ("z_bialgebra2_s1_1",z_bialgebra2_s1_1_rule) *)
            ];


fun match_all_rules g = 
    maps
      (fn (n,r) => let val (rmn, r2) = (Rule.rename_rule_apart g r)
                   in map (fn m => ((n,r2),m))
                          (Seq.list_of (Rule.matches r2 g))
                   end)
      rules;

fun apply_all_rules g = 
    map (fn r => ("spider", r)) 
        (Seq.list_of (Spider.mk_max_spider_rule g))
    @
    maps
      (fn (n,r) => map (fn r2 => (n,r2)) (Seq.list_of (Rule.rule_matches r g)))
      rules;

fun apply_all_rules_in vset g = 
    map (fn r => ("spider", r)) 
        (Seq.list_of (Spider.mk_max_spider_rule_within vset g))
    @
    maps
      (fn (n,r) => map (fn r2 => (n,r2)) 
                       (Seq.list_of (Rule.rule_matches_within r vset g)))
      rules;


(* Unused: matching only produces symmetries w.r.t. different namings
- our distinctness check never sees this, so sees them as really
distinct. Need a richer distinctness test, or symmetry reduction in
matching. 

(* assumes distinct_rules are all results of applying the same rule;
   i.e. have same lhs and same naming. *)
fun filter_symmetric_results (rule, distinct_rules) = 
    if List.exists (fn prevrule => 
                       RGGraph.exact_eq (Rule.get_rhs prevrule)
                                        (Rule.get_rhs rule)) 
                   distinct_rules
    then distinct_rules else rule::distinct_rules;

fun apply_distinctly_all_rules g = 
    maps
      (fn (n,r) => 
          (Seq.list_of (Rule.rule_matches r g))
            |> List.foldl filter_symmetric_results []
            |> map (fn r2 => (n,r2)))
      rules;

fun apply_distinctly_all_rules_in vset g = 
    maps
      (fn (n,r) => 
          (Seq.list_of (Rule.rule_matches_within r vset g))
            |> List.foldl filter_symmetric_results []
            |> map (fn r2 => (n,r2)))
      rules;
*)

end;
