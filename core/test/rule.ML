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

val [] = Seq.list_of (Rule.rule_matches TheoryHack.isometry_rule g);

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

val L = TheoryHack.apply_all_rules g;
val [r1,r2,r3,r4] = map snd L;

val L = TheoryHack.apply_distinctly_all_rules g

end;




local open RGGraph in 

(* two graphs are exactly the same, including data, edge direction and
   names of vertices and edges. *) 
fun exact_eq g1 g2 =
    let 
      val e1s = get_edges g1
      val e2s = get_edges g2
      val v1s = get_vertices g1
      val v2s = get_vertices g2
    in
      (* check same vertex names *)
      (Vertex.NSet.eq (Vertex.NTab.get_names v1s)
                      (Vertex.NTab.get_names v2s)) andalso 
      (* check same data, and same incident edges structure *)
      (Vertex.NTab.forall 
         (fn (n,(d,(fromes,toes))) => 
             (case Vertex.NTab.lookup v2s n of 
                NONE => false orelse (writeln "Missing in snd"; false)
              | SOME (d2,(fromes2,toes2)) => 
                ((Vertex.data_eq (d,d2))
                 orelse (writeln "Different data"; false)) andalso 
                (* now check structure *)
                (Edge.NSet.eq fromes fromes2
                 orelse (writeln "Different fromes"; false)
                ) andalso
                (Edge.NSet.eq toes toes2)
                orelse (writeln "Different toes"; false)) 
             orelse (writeln ("; different for Vertex: " ^ n); false))
         v1s) andalso 
      (* now check same edge data *)
      (Edge.NTab.forall (* note that we dont re-check structure *)
         (fn (e,(ed,_)) => 
             (* from above, we know we have same edge names: could use get *)
             (case Edge.NTab.lookup e2s e of 
                NONE => false
              (* now check have same edge data *)
              | SOME (ed2,_) => Edge.data_eq (ed,ed2))
             orelse (writeln ("missing or different data; for Edge: " ^ e);
                     false))
         e1s)
    end;

end;


(*
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


