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
 

(* in *)
val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.")

val rules = [("x_isometry",x_isometry_rule),
             ("z_isometry",z_isometry_rule),
             
             (* no frobenius: it's an id in our rep *)
             
             ("x_compact",x_compact_rule),
             ("z_compact",z_compact_rule),

             ("x_abelian1",x_abelian1_rule),
             (* no abelian2: not needed for our rep *)
             ("z_abelian3",z_abelian3_rule),
                   
             ("z_abelian1",z_abelian1_rule),
             (* no abelian2: not needed for our rep *)
             ("z_abelian3",z_abelian3_rule),
             
             ("xz_bialgebra1",xz_bialgebra1_rule),
             ("x_bialgebra2",x_bialgebra2_rule),
             ("z_bialgebra2",z_bialgebra2_rule)
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
