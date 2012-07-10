(* FIXME: remove *)
structure DB_Test_GraphSubst = GraphSubstFun(Test_Match);
structure Test_GraphSubst : GRAPH_SUBST = DB_Test_GraphSubst;
Test_GraphSubst.Log.level_ref := 4;

structure DB_Test_OGraphSubst = OGraphSubst(Test_MatchState);
structure Test_OGraphSubst : GRAPH_SUBST = DB_Test_OGraphSubst;
Test_OGraphSubst.Log.level_ref := 4;

structure DB_Test_BangGraphSubst = BangGraphSubst(Test_BGMatchState);
structure Test_BangGraphSubst : GRAPH_SUBST = DB_Test_BangGraphSubst;
Test_BangGraphSubst.Log.level_ref := 4;

local
open Test_MkG;
structure M = Test_ConcreteMatcher;
structure MS = Test_MatchState;
structure GSub = Test_OGraphSubst;

(* some useful names *)
val in1 = V.mk "in1"
val in2 = V.mk "in2"
val out1 = V.mk "out1"
val out2 = V.mk "out2"
val x1 = V.mk "x1"
val x2 = V.mk "x2"
val z1 = V.mk "z1"
val z2 = V.mk "z2"
val z3 = V.mk "z3"
val z4 = V.mk "z4"

in

val _ = Testing.test "rule1: simple spider" (fn () => let
  val g = G.empty
  (* LHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge in2 x2
            |> G.doadd_edge dir_edge x2 out2
            |> G.doadd_edge dir_edge x1 x2
  val lhs = g

  val g = G.empty
  (* RHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge x1 out2
  val rhs = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.doadd_edge dir_edge z1 x1
            |> G.doadd_edge dir_edge z2 x1
            |> G.doadd_edge dir_edge x1 z3
            |> G.doadd_edge dir_edge x1 z4
  val exp = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.doadd_edge dir_edge z1 x1
            |> G.doadd_edge dir_edge x1 z3
            |> G.doadd_edge dir_edge z2 x2
            |> G.doadd_edge dir_edge x2 z4
            |> G.doadd_edge dir_edge x1 x2
  val tgt = g

  val mseq = M.match lhs tgt
  val SOME (ms,_) = Seq.pull mseq

  val result = GSub.do_rewrite tgt lhs ms rhs
  val result' = G.minimise result
  val _ = if GIso.test exp result' then ()
          else (
            writeln "Result:";
            G.print result';
            writeln "Expected:";
            G.print exp;
            raise ERROR "Result was not iso to exp"
          )

  in writeln "success" end) ();

val _ = Testing.test "rule2: simple spider with data subst" (fn () => let
  val g = G.empty
  (* LHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX (var_angle "a"))
            |> G.add_named_vertex x2 (mkX (var_angle "b"))
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge in2 x2
            |> G.doadd_edge dir_edge x2 out2
            |> G.doadd_edge dir_edge x1 x2
  val lhs = g

  val g = G.empty
  (* RHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX (parse_angle "a+b"))
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge x1 out2
  val rhs = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX (add_angles (var_angle "c") pi_angle))
            |> G.doadd_edge dir_edge z1 x1
            |> G.doadd_edge dir_edge z2 x1
            |> G.doadd_edge dir_edge x1 z3
            |> G.doadd_edge dir_edge x1 z4
  val exp = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX (var_angle "c"))
            |> G.add_named_vertex x2 (mkX pi_angle)
            |> G.doadd_edge dir_edge z1 x1
            |> G.doadd_edge dir_edge x1 z3
            |> G.doadd_edge dir_edge z2 x2
            |> G.doadd_edge dir_edge x2 z4
            |> G.doadd_edge dir_edge x1 x2
  val tgt = g

  val mseq = M.match lhs tgt
  val SOME (ms,_) = Seq.pull mseq

  val result = GSub.do_rewrite tgt lhs ms rhs
  val result' = G.minimise result
  val _ = if GIso.test exp result' then ()
          else (
            writeln "Result:";
            G.print result';
            writeln "Expected:";
            G.print exp;
            raise ERROR "Result was not iso to exp"
          )

  in writeln "success" end) ();

val _ = Testing.assert_no_failed_tests();
end



(* !-box rewriting *)
local
open Test_MkG;
structure M = Test_GreedyMatcher;
structure MS = Test_BGMatchState;
structure GSub = Test_BangGraphSubst;

(* some useful names *)
val in1 = V.mk "in1"
val in2 = V.mk "in2"
val out1 = V.mk "out1"
val out2 = V.mk "out2"
val x1 = V.mk "x1"
val x2 = V.mk "x2"
val z1 = V.mk "z1"
val z2 = V.mk "z2"
val z3 = V.mk "z3"
val z4 = V.mk "z4"
val z5 = V.mk "z5"
val z6 = V.mk "z6"
val z7 = V.mk "z7"
val z8 = V.mk "z8"
val b1 = B.mk "B1"
val b2 = B.mk "B2"
val b3 = B.mk "B3"
val b4 = B.mk "B4"

in

val _ = Testing.test "rule1: spider" (fn () => let
  val g = G.empty
  (* LHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX (var_angle "a"))
            |> G.add_named_vertex x2 (mkX (var_angle "b"))
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge in2 x2
            |> G.doadd_edge dir_edge x2 out2
            |> G.doadd_edge dir_edge x1 x2
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.add_named_bbox b3
            |> G.add_named_bbox b4
            |> G.add_to_bbox b1 (V.NSet.single in1)
            |> G.add_to_bbox b2 (V.NSet.single in2)
            |> G.add_to_bbox b3 (V.NSet.single out1)
            |> G.add_to_bbox b4 (V.NSet.single out2)
  val lhs = g

  val g = G.empty
  (* RHS of spider pattern *)
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex out2 bvert
            |> G.add_named_vertex x1 (mkX (parse_angle "a+b"))
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge x1 out1
            |> G.doadd_edge dir_edge x1 out2
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.add_named_bbox b3
            |> G.add_named_bbox b4
            |> G.add_to_bbox b1 (V.NSet.single in1)
            |> G.add_to_bbox b2 (V.NSet.single in2)
            |> G.add_to_bbox b3 (V.NSet.single out1)
            |> G.add_to_bbox b4 (V.NSet.single out2)
  val rhs = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex z5 (mkZ zero_angle)
            |> G.add_named_vertex z6 (mkZ zero_angle)
            |> G.add_named_vertex z7 (mkZ zero_angle)
            |> G.add_named_vertex z8 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX (add_angles (var_angle "c") pi_angle))
            |> G.doadd_edge dir_edge z1 x1
            |> G.doadd_edge dir_edge z2 x1
            |> G.doadd_edge dir_edge z3 x1
            |> G.doadd_edge dir_edge x1 z4
            |> G.doadd_edge dir_edge x1 z5
            |> G.doadd_edge dir_edge x1 z6
            |> G.doadd_edge dir_edge x1 z7
  val exp = g

  val g = G.empty
  val g = g |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_vertex z3 (mkZ zero_angle)
            |> G.add_named_vertex z4 (mkZ zero_angle)
            |> G.add_named_vertex z5 (mkZ zero_angle)
            |> G.add_named_vertex z6 (mkZ zero_angle)
            |> G.add_named_vertex z7 (mkZ zero_angle)
            |> G.add_named_vertex z8 (mkZ zero_angle)
            |> G.add_named_vertex x1 (mkX (var_angle "c"))
            |> G.add_named_vertex x2 (mkX pi_angle)
            |> G.doadd_edge dir_edge z1 x2
            |> G.doadd_edge dir_edge z2 x2
            |> G.doadd_edge dir_edge z3 x2
            |> G.doadd_edge dir_edge x1 z4
            |> G.doadd_edge dir_edge x1 z5
            |> G.doadd_edge dir_edge x1 z6
            |> G.doadd_edge dir_edge x2 z7
            |> G.doadd_edge dir_edge x1 x2
  val tgt = g

  val mseq = M.match lhs tgt
  val SOME (ms,_) = Seq.pull mseq

  val result = GSub.do_rewrite tgt lhs ms rhs
  val result' = G.minimise result
  val _ = if GIso.test exp result' then ()
          else (
            writeln "Result:";
            G.print result';
            writeln "Expected:";
            G.print exp;
            raise ERROR "Result was not iso to exp"
          )

  in writeln "success" end) ();

val _ = Testing.assert_no_failed_tests();
end
