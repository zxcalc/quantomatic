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
val _ = writeln "Ex1";


(* Example of applications of rule that does not match *)
val g = RGGraph.empty;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge n1 n3;
val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
val _ = RGGraph.print g;
val [] = map Rule.print (Seq.list_of (Rule.rule_matches TheoryHack.x_isometry_rule g));
val _ = writeln "Ex2";


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
val _ = writeln "Ex3";


val [m1,m2] = Seq.list_of (Rule.matches TheoryHack.x_isometry_rule g);
val [r1,r2] = map Rule.print (Seq.list_of (Rule.rule_matches TheoryHack.x_isometry_rule g));
val [g1,g2] = map RGGraph.print (Seq.list_of (Rule.rewrites TheoryHack.x_isometry_rule g));
val _ = writeln "Ex4";


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
val _ = writeln "Ex5";

val [r1,r2] = Seq.list_of (Rule.rule_matches TheoryHack.x_isometry_rule g);
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
val _ = writeln "Ex6";

val [] = Seq.list_of (Rule.rule_matches TheoryHack.x_isometry_rule g);

(* bialgebra3 *)
val [r1,r2] = TheoryHack.apply_all_rules g;


(* Example of not applicable rules... *)
val g = RGGraph.empty;
val (n1, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex bvert;
val (n4, g) = g |> RGGraph.add_vertex bvert;
val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
val (e2, g) = g |> RGGraph.add_edge edge n2 n4;
val _ = RGGraph.print g;
val _ = writeln "Ex7";

(* bialgebra2 *)
val [r1,r2] = TheoryHack.apply_all_rules g;


(* big bialgebra1 rule matches it's own lhs in 4 ways *)
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
val _ = writeln "Ex8";

val L = TheoryHack.apply_all_rules g;
val [r1,r2,r3,r4] = map snd L;
val _ = writeln "big bialgebra1 rule matches it's own lhs in 4 ways";


(* X isometry + stuff should still be matched by isometry  *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n4, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 n3;
val (e4, g) = g |> RGGraph.add_edge edge n3 n4;
val _ = RGGraph.print g;
val _ = writeln "Ex9";

(* isometry and abelian both apply. *)
val [m1,m2,m3,m4] = TheoryHack.apply_all_rules g
(* val [s1,m1,m2,m3,m4] = TheoryHack.apply_all_rules g *)
val _ = writeln " X isometry + stuff should still be matched by isometry";


(* X isometry + matches x_isometry with instantiated boundary node  *)
val g = RGGraph.empty;
val (b1, g) = g |> RGGraph.add_vertex bvert;
val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
val (e0, g) = g |> RGGraph.add_edge edge b1 n1;
val (e1, g) = g |> RGGraph.add_edge edge n1 n2;
val (e2, g) = g |> RGGraph.add_edge edge n1 n2;
val (e3, g) = g |> RGGraph.add_edge edge n2 n3;
val _ = RGGraph.print g;
val _ = writeln "Ex10";

(* isometry and abelian both apply. *)
val [m1,m2,m3] = TheoryHack.apply_all_rules g;
(* val [s1,m1,m2,m3] = TheoryHack.apply_all_rules g; *)



end;



(* 

PolyML.exception_trace (fn () => TheoryHack.apply_all_rules DB_Test_Rule.g);

val rule = TheoryHack.x_isometry_rule;

val tgt = DB_Test_Rule.g;
val (rn,rule') = (Rule.rename_rule_apart tgt rule);
val ms = (Rule.matches rule' tgt);

fun match_all_rules g = 
    maps
      (fn (n,r) => let val (rmn, r2) = (Rule.rename_rule_apart g r)
                   in map (fn m => ((n,r2),m))
                          (Seq.list_of (Rule.matches r2 g))
                   end)
      TheoryHack.rules;

val L = match_all_rules tgt;
val [_,_,((rn,r),m3)] = L;

Rule.print r;
RGGraph.print tgt;
Match.print m3;

val [m1,m2] = Seq.list_of ms;

MatchState.print_match
Rule.print rule';
Rule.inst_rule rule' m3;

Rule.inst_rule rule' m1;
*)



(*
TheoryHack.apply_all_rules (DB_Test_Rule.g);

Rule.print TheoryHack.z_bialgebra2_rule;

val tgt = DB_Test_Rule.g;
val rule = TheoryHack.x_isometry_rule;
val (rn,rule) = (DB_Rule.rename_rule_apart tgt rule);

Rule.print rule;
RGGraph.print tgt;

Seq.list_of (MatchState.match (Rule.get_lhs rule) tgt MatchState.Vertex.NTab.empty);


val rhs1 = (Rule.get_rhs (snd DB_Test_Rule.m1))
val rhs2 = (Rule.get_rhs (snd DB_Test_Rule.m2))
RGGraph.print rhs1;
RGGraph.print rhs2;

exact_eq rhs1 rhs2;

    map (fn (s,r) => (writeln (s ^ ":"); Rule.print r)) DB_Test_Rule.L;

    RGGraphDotWriter.output_to_file "foo.dot" g;

    val L = TheoryHack.apply_all_rules g
        handle e => (writeln ("exception! : " ^ PolyML.makestring e); []);

    raise ERROR "stop!";

    PolyML.exception_trace (fn () =>     );
*)


