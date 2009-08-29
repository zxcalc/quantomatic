structure TheoryHack 
= struct

(* local *)
  structure V = RGGraph.BVertex;
  structure E = RGGraph.Edge;
  val bvert = V.BVert;
  val zero_angle = AngleExpr.zero;
  val var_angle = AngleExpr.mk_var;
  fun var_iangle i v = AngleExpr.mk (0,1) [(v,i)];
  val add_angles = AngleExpr.add_expr;
  val pi_angle = AngleExpr.pi;
  fun mkZ a = V.IVert (RGVertex.Znd a);
  fun mkX a = V.IVert (RGVertex.Xnd a);
  val mkH = V.IVert RGVertex.Hnd;
  val edge : UnitEdge.data = ();

  fun colour_swap g = 
      let
        fun flip_data (V.IVert (RGVertex.Xnd e)) = V.IVert (RGVertex.Znd e)
          | flip_data (V.IVert (RGVertex.Znd e)) = V.IVert (RGVertex.Xnd e)
          | flip_data d = d
      in 
        V.NSet.fold (RGGraph.update_vertex_data flip_data) 
                    (RGGraph.get_vnames g) g
      end;

  fun swap_rule_colour r = 
      Rule.mk (colour_swap (Rule.get_lhs r),
               colour_swap (Rule.get_rhs r));

(* no scalars in theory hack - they are not really important *)
(* val (x, g) = g |> RGGraph.add_vertex (mkX zero_angle); (* scalar X *)
   val (z, g) = g |> RGGraph.add_vertex (mkZ zero_angle); (* scalar Z *)
   val (_, g) = g |> RGGraph.add_edge edge x z; (* scalar XZ *) *)

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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
  val rhs = g;
in 
  val isometry_rule = Rule.mk (lhs,rhs);
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val g = g |> RGGraph.new_named_vertex b3 bvert;
  val g = g |> RGGraph.new_named_vertex b4 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
  val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e3, g) = g |> RGGraph.add_edge edge b3 n2;
  val (e4, g) = g |> RGGraph.add_edge edge n2 b4;
  val rhs = g;
in 
  val frobenius_rule = Rule.mk (lhs,rhs);
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
  val (e1, g) = g |> RGGraph.add_edge edge n1 b2;
  val (e2, g) = g |> RGGraph.add_edge edge n2 n1;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
  val rhs = g;
in 
  val compact_rule = Rule.mk (lhs,rhs);
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
  val rhs = g;
in 
  val abelian1_rule = Rule.mk (lhs,rhs);
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (add_angles (var_angle "a") (var_angle "b")));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b2 n1;
  val rhs = g;
in
  val abelian3_rule = Rule.mk (lhs,rhs);
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
  val (e2, g) = g |> RGGraph.add_edge edge n3 b3;
  val (e3, g) = g |> RGGraph.add_edge edge n4 b4;
  val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
  val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
  val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
  val lhs = g;
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
in
  val bialgebra1_rule = Rule.mk (lhs,rhs);
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
  val (e2, g) = g |> RGGraph.add_edge edge n3 b3;
  val (e3, g) = g |> RGGraph.add_edge edge n4 b4;
  val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
  val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
  val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
  val lhs = g;
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
in
  val bialgebra1_s1_0_rule = Rule.mk (lhs,rhs);
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
  val (e2, g) = g |> RGGraph.add_edge edge n3 b3;
  val (e3, g) = g |> RGGraph.add_edge edge n4 b4;
  val (e4, g) = g |> RGGraph.add_edge edge n1 n3;
  val (e5, g) = g |> RGGraph.add_edge edge n1 n4;
  val (e6, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e7, g) = g |> RGGraph.add_edge edge n2 n4;
  val lhs = g;
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
in
  val bialgebra1_s2_0_rule = Rule.mk (lhs,rhs);
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b22 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e0, g) = g |> RGGraph.add_edge edge b22 n2;
  val (e0, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
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
in
  val hopf_2_2_rule = Rule.mk (lhs,rhs);
end;



(* Copy X(0) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge n2 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge n2 b2;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
in
  val copy_0_rule = Rule.mk (lhs,rhs);
end;

(* Copy X(pi) *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge n2 n1;
  val (e1, g) = g |> RGGraph.add_edge edge b1 n2;
  val (e2, g) = g |> RGGraph.add_edge edge n2 b2;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
in
  val copy_pi_rule = Rule.mk (lhs,rhs);
end;

(* group 1: 1 input *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ pi_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ pi_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val rhs = g;
in
  val group1_1in_rule = Rule.mk (lhs,rhs);
  val _ = writeln "group1_1in_rule";
end;


(* group 1: 2 input *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ pi_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (b2, g) = g |> RGGraph.add_vertex bvert;
  val (b3, g) = g |> RGGraph.add_vertex bvert;
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val (_, g) = g |> RGGraph.add_edge edge n2 b3;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val g = g |> RGGraph.new_named_vertex b3 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ pi_angle);
  val (n3, g) = g |> RGGraph.add_vertex (mkZ pi_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n1 n3;
  val (_, g) = g |> RGGraph.add_edge edge n2 b2;
  val (_, g) = g |> RGGraph.add_edge edge n3 b3;
  val rhs = g;
in
  val group1_2in_rule = Rule.mk (lhs,rhs);
  val _ = writeln "group1_2in_rule";
end;

(* group 2 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (n2, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (_, g) = g |> RGGraph.add_edge edge b1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n2 n1;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_iangle (~1,1) "a"));
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
in
  val group2_rule = Rule.mk (lhs,rhs);
  val _ = writeln "group2_rule";
end;

(* group 3 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (_, g) = g |> RGGraph.add_edge edge b1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
in
  val group3_rule = Rule.mk (lhs,rhs);
  val _ = writeln "group3_rule";
end;

(* group 4 *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (_, g) = g |> RGGraph.add_edge edge b1 n2;
  val (_, g) = g |> RGGraph.add_edge edge n1 n2;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX pi_angle);
  val (_, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
in
  val group4_rule = Rule.mk (lhs,rhs);
  val _ = writeln "group4_rule";
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b4 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b4;
  val rhs = g;
in
  val hh_to_id_rule = Rule.mk (lhs,rhs);
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
in
  val hhh_swap_rule = Rule.mk (lhs,rhs);
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val (e2, g) = g |> RGGraph.add_edge edge n1 b2;
  val rhs = g;
in
  val hh_swap_rule = Rule.mk (lhs,rhs);
end;

(* HX to Z - Colour duality *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (h1, g) = g |> RGGraph.add_vertex (mkH);
  val (n1, g) = g |> RGGraph.add_vertex (mkZ (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 h1;
  val (e1, g) = g |> RGGraph.add_edge edge h1 n1;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val (n1, g) = g |> RGGraph.add_vertex (mkX (var_angle "a"));
  val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
  val rhs = g;
in
  val h_swap_rule = Rule.mk (lhs,rhs);
end;


(* drop 1 self-loop *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (b1, g) = g |> RGGraph.add_vertex bvert;
  val (e1, g) = g |> RGGraph.add_edge edge b1 b1;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val rhs = g;
in
  val drop_self_loop_rule = Rule.mk (lhs,rhs);
end;


(* !-boxed, spider'd anti-loop *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (v1, g) = g |> RGGraph.add_vertex bvert;
  val (v2, g) = g |> RGGraph.add_vertex bvert;
  val (e1, g) = g |> RGGraph.add_edge edge v1 v1;
  val (e2, g) = g |> RGGraph.add_edge edge v1 v2;
  val (box1, g) = g |> RGGraph.new_box (RGGraph.Vertex.mk "Ba");
  val g = g |> RGGraph.move_to_box box1 (V.NSet.single v2);
  val lhs = g;
  (* rhs *)
  val rhs = g |> RGGraph.delete_edge e1;
in
  val spidered_antiloop_rule = Rule.mk (lhs,rhs);
end;


(* basic 1-anti-loop *)
local 
  (* lhs *)
  val g = RGGraph.empty;
  val (v1, g) = g |> RGGraph.add_vertex bvert;
  val (e1, g) = g |> RGGraph.add_edge edge v1 v1;
  val lhs = g;
  (* rhs *)
  val g = RGGraph.empty;
  val (v1, g) = g |> RGGraph.add_vertex bvert;
  val rhs = g;
in
  val antiloop_rule = Rule.mk (lhs,rhs);
end;


(* Spider Isometry *)
(* local 
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
  (* rhs *)
  val g = RGGraph.empty;
  val g = g |> RGGraph.new_named_vertex b1 bvert;
  val g = g |> RGGraph.new_named_vertex b2 bvert;
  val (e0, g) = g |> RGGraph.add_edge edge b1 b2;
  val rhs = g;
in 
  val x_isometry_rule = Rule.mk (lhs,rhs);
  val _ = Rule.print isometry_rule;
end;      
*)



(* in *)
(*val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.");*)

val bi_colour_rules = 
    [
     ("hopf_1_1",hopf_1_1_rule),
     ("hopf_1_2",hopf_1_2_rule),
     ("hopf_2_1",hopf_2_1_rule),
     ("hopf_2_2",hopf_2_2_rule),
     ("bialgebra1",bialgebra1_rule),
     ("drop_self_loop", drop_self_loop_rule),
	   ("antiloop_rule",antiloop_rule),
     ("hh_to_id",hh_to_id_rule)
    ];

val coloured_h_rules =  
    [
     ("h_swap",h_swap_rule),
     ("hh_swap",hh_swap_rule),
     ("hhh_swap",hhh_swap_rule)
    ];


val one_colour_rules = 
    bi_colour_rules
    @ (maps 
         (fn (n,r) => [("x_" ^ n, swap_rule_colour r), 
                       ("z_inv_" ^ n, Rule.symmetric r)])
         coloured_h_rules);

val swap_colour_rules = 
    [
     (* abelian1 is special case outside our spider *)
     ("abelian1", abelian1_rule),
     ("group1_1in", group1_1in_rule),
     ("group1_2in", group1_2in_rule),
     ("group2", group2_rule),
     ("group3", group3_rule),
     ("group4", group4_rule),
     ("copy_0", copy_0_rule),
     ("copy_pi", copy_pi_rule)
    ];

val default_rules = 
    one_colour_rules
    @ (maps (fn (n,r) => [("z_" ^ n, swap_rule_colour r), ("x_" ^ n, r)])
            swap_colour_rules);

val ruletab = 
    fold StrName.NTab.update default_rules StrName.NTab.empty;

val get_rule = StrName.NTab.get ruletab;

val rules = ref default_rules;
(*
fun save_rules file = 
    PrettyWriter.write_to_file file (RGTheoryHackOutputXML.output (!rules))

exception fuck;

fun load_rules file = 
    (
     rules := ((RGTheoryHackInputXML.input o XMLReader.read_from_file) file) ;
     TextIO.print "new rules:\n" ;
     TextIO.print ( PrettyWriter.write_to_string (
                                      RGTheoryHackOutputXML.output (!rules))) ;
     TextIO.print "---end of new rules---\n" 
    )
*)
fun match_all_rules g = 
    Seq.maps
      (fn (n,r) => let val (rmn, r2) = (Rule.rename_rule_apart g r)
                   in Seq.map (fn m => ((n,r2),m))
                              (Rule.matches r2 g)
                   end)
      (Seq.of_list (!rules));

(* apply rules to whole graph *)
fun apply_rules rules g = 
    Seq.append 
      (Seq.map (fn r => ("spider", r)) 
               (Spider.mk_max_spider_rule g))
      (Seq.maps
         (fn (n,r) => Seq.map (fn r2 => (n,r2)) (Rule.rule_matches r g))
         (Seq.of_list rules));

(* apply rules to select vertices (vset) *)
fun apply_rules_in rules vset g = 
    Seq.append 
      (Seq.map (fn r => ("spider", r)) 
               (Spider.mk_max_spider_rule_within vset g))
      (Seq.maps
         (fn (n,r) => Seq.map (fn r2 => (n,r2)) 
                              (Rule.rule_matches_within r vset g))
         (Seq.of_list rules));

fun apply_all_rules g = 
    let val r = !rules
    in 
      apply_rules r g
    end;

fun apply_all_rules_in vset g = 
    let val r = !rules
    in 
      apply_rules_in r vset g
    end;

fun apply_a_rule_in vset g = 
    let val r = !rules
    in 
      case Seq.pull (apply_all_rules_in vset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h
    end;

fun apply_all_rules_in' vset g = Seq.list_of (apply_all_rules_in vset g);


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
