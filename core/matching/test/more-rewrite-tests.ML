(* code for testing simple match search *)

local
  open Test_MkG; (* RGGraph *)
  structure S = Test_SimpleMatchSearch;
  structure Match = Test_Match;
  fun match g1 g2 = 
      let val (rn,g2') = G.rename_apart g1 g2;
      in Seq.list_of (S.match no_extra_names g1 g2')
      end;
in val _ =  let 

(* Graph: (-x) *)
val _ = writeln "************ TEST 1 *****************";
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (e0, g) = g |> G.add_edge undir_edge b1 n1;
val x1 = g;

(* Graph: (-x-) *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (b2, g) = g |> G.add_vertex bvert;
val (e0, g) = g |> G.add_edge undir_edge b1 n1;
val (e1, g) = g |> G.add_edge undir_edge n1 b2;
val x2 = g;

val [] = match x1 x2;
val [] = match x2 x1;

val (_,x2') = G.rename_apart x2 x2;
val _ = G.print x2;
val _ = V.NSet.print (G.get_boundary x2);
val _ = G.print x2';

val ms = match x2 x2';
val _ = map Match.print ms;
val [m1,m2] = ms;

val _ = writeln "************ TEST 2 *****************";
(* x-z= *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge undir_edge n2 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n2;
val (_, g) = g |> G.add_edge undir_edge b2 n2;
val g_x0_z0 = g;

(* x-z(\pi)= *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX pi_angle);
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge undir_edge n2 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n2;
val (_, g) = g |> G.add_edge undir_edge b2 n2;
val g_xPi_z0 = g;

(* x-z(a)= *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge undir_edge n2 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n2;
val (_, g) = g |> G.add_edge undir_edge b2 n2;
val g_xVa_z0 = g;

(* *)
val [m1,m2] = match g_xPi_z0 g_xPi_z0;

val [] = match g_x0_z0 g_xVa_z0;
val [] = match g_xPi_z0 g_xVa_z0;
val [m1,m2] = match g_xVa_z0 g_xPi_z0;
val [m1,m2] = match g_xVa_z0 g_x0_z0;

val _ = writeln "************ TEST 3 *****************";

(* a circle *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (_, g) = g |> G.add_edge undir_edge b1 b1;
val circle = g;


(* single self-loop on X *)
val g = G.empty;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val x_1loop = g;

val [m1,m2] = match x2 x_1loop;
val _ = Match.print m1;
val _ = Match.print m2;

val _ = writeln "************ TEST 4 *****************";
(* X spider, self-loop lhs *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val (bbn, g) = g |> G.add_named_bbox (B.mk "B0");
val g = g |> G.move_to_bbox bbn (V.NSet.single b1);
val x_1loop_spider = g;

val [m1,m2] = match x_1loop_spider x_1loop;
val _ = Match.print m1;
val _ = Match.print m2;

val _ = writeln "************ TEST 5 *****************";
(* self-loop on X(b) and a dangling edge *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "b"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val xVb_loop_and_edge = g;

val (ms as [m1,m2]) = match x_1loop_spider xVb_loop_and_edge;
val _ = map Match.print ms;

val _ = writeln "************ TEST 6 *****************";
(* two self-loops on x *)
val g = G.empty;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val x_2loop = g;

val ms = match x_1loop_spider x_2loop;
val _ = map Match.print ms;
val 8 = length ms; 

(* one unfolding of bangbox...*)
(* 
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "b"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val (_, g) = g |> G.add_edge undir_edge b2 n1;
val tgt = g;

val (rn,pat) = (G.rename_apart tgt pat);
val _ = G.print pat;
val _ = G.print tgt;

val [m] = Seq.list_of (MatchSearch.match pat tgt V.NTab.empty);
val _ = Match.print m;

PolyML.exception_trace (fn () => );

*)
in
 Pretty.writeln (Pretty.str "Tests passed: graph matching.")
end  (* let *)
handle e => 
       (Pretty.writeln (Pretty.str "Test Failed: graph matching."); raise e);
end; (* local *)

