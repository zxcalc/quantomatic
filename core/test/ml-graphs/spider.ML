structure Test_Spider = 
struct

val g = Rule.get_lhs (TheoryHack.isometry_rule);
val L = Seq.list_of (Spider.mk_max_spider_rule g);

val _ = writeln "Spider tests done. ";
end;

(* 
(* local *)
  structure V = RGGraph.BVertex;
  structure E = RGGraph.Edge;
  val bvert = V.BVert ();
  val zero_angle = LinratExpr.zero;
  val var_angle = LinratExpr.mk_var;
  val add_angles = LinratExpr.add_expr;
  val pi_angle = LinratExpr.pi;
  fun mkZ a = V.IVert (RGVData.Znd a);
  fun mkX a = V.IVert (RGVData.Xnd a);
  val mkH = V.IVert RGVData.Hnd;
  val edge : UnitEData.data = ();



(* X - Bialgebra 2 : spider_extension(+1,+1) *)
  (* lhs *)
  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e2, g) = g |> RGGraph.add_edge edge n3 n1;

val L = Seq.list_of (Spider.mk_max_spider_rule g);


  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n3, g) = g |> RGGraph.add_vertex (mkZ zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e2, g) = g |> RGGraph.add_edge edge n3 n1;

val L = Seq.list_of (Spider.mk_max_spider_rule g);


  val g = RGGraph.empty;
  val (n1, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n2, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (n4, g) = g |> RGGraph.add_vertex (mkX zero_angle);
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e0, g) = g |> RGGraph.add_edge edge n1 n2;
  val (e1, g) = g |> RGGraph.add_edge edge n2 n3;
  val (e2, g) = g |> RGGraph.add_edge edge n3 n1;

val L = Seq.list_of (Spider.mk_max_spider_rule g);
*)

(* 
open Spider;

val SOME st0 = start_spider_match (RGGraph.get_vnames g, g);
val st = st0; print_st st0;
val (sol,SOME st) = expand_spider st;
val st1 = st; print_st st1;
val (sol,SOME st) = expand_spider st;
val st2 = st; print_st st2;
val (sol,SOME st) = expand_spider st;
val st3 = st; print_st st3;
*)

(*
     PolyML.exception_trace (fn () =>     );
*)
