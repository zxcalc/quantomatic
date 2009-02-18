local 
open MkG;
in val _ = let

(* Pattern: (B - G) *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (e0, g) = g |> G.add_edge edge b1 n1;
val g0 = g;


(* Pattern: (B - G - B) *)
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (b2, g) = g |> G.add_vertex bvert;
val (e0, g) = g |> G.add_edge edge b1 n1;
val (e1, g) = g |> G.add_edge edge n1 b2;
val g1 = g;

val ms = Seq.list_of (MatchState.match g1 g0 RGVertex.NTab.empty);
val _ = map Match.print ms;
val [] = ms;


val (_,g2) = G.rename_apart g1 g;
val _ = G.print g1;
val _ = V.NSet.print (G.get_leaf_vnames g1);
val _ = G.print g2;

val ms = Seq.list_of (MatchState.match g1 g2 RGVertex.NTab.empty);
val _ = map Match.print ms;
val [m1,m2] = ms;




val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge edge n2 n1;
val (_, g) = g |> G.add_edge edge b1 n2;
val (_, g) = g |> G.add_edge edge b2 n2;
val pat1 = g;

val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX pi_angle);
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge edge n2 n1;
val (_, g) = g |> G.add_edge edge b1 n2;
val (_, g) = g |> G.add_edge edge b2 n2;
val pat2 = g;

val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
val (_, g) = g |> G.add_edge edge n2 n1;
val (_, g) = g |> G.add_edge edge b1 n2;
val (_, g) = g |> G.add_edge edge b2 n2;
val tgt = g;

val pat = pat1;
val (rn,pat) = (G.rename_apart tgt pat);
val [] = Seq.list_of (MatchState.match pat1 tgt RGVertex.NTab.empty);
val [] = Seq.list_of (MatchState.match pat2 tgt RGVertex.NTab.empty);

val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (_, g) = g |> G.add_edge edge b1 b1;
val pat = g;

val g = G.empty;
val (n1, g) = g |> G.add_vertex (mkX zero_angle);
val (_, g) = g |> G.add_edge edge n1 n1;
val (_, g) = g |> G.add_edge edge n1 n1;
val tgt = g;

val (rn,pat) = (G.rename_apart tgt pat);
val _ = G.print pat;
val _ = G.print tgt;

val [m] = Seq.list_of (MatchState.match pat tgt RGVertex.NTab.empty);
val _ = Match.print m;

in
 Pretty.writeln (Pretty.str "Tests passed: graph matching.")
end  (* let *)
handle e => 
       (Pretty.writeln (Pretty.str "Test Failed: graph matching."); raise e);

end; (* local *)


(* For debugging... *)
(*
    RGGraphDotWriter.output_to_file "foo.dot" g;
    PolyML.exception_trace (fn () =>     );
*)


(* debug stuff *)
(* 

raise ERROR "stop";


open DB_MatchState;

val initmatching = Match.empty;
val pat = g1;
G.print pat;
val tgt = g2;
G.print tgt;
val initagenda = V.NTab.empty;
val init_st = (init initmatching pat tgt initagenda);
val st = init_st;
print st;

(* starting point for matching; first vertex *)
val SOME pn = (V.NSet.get_local_bot (get_unm_pat_vs st));
V.print_name pn;

(* possible things it can match *)
val tns = Seq.list_of (get_tgt_vnseq st);
map V.print_name tns;

val Progress sq = do_next_match_choices st;
val l = Seq.list_of sq;
map print l;

val st = hd l;


val [st1,st2,st3,st4] = l;

val st = st1;
val SOME ((pn,tn),st) = pop_agenda st;
V.print_name pn;
V.print_name tn;
print st;

val pnv = (pn,pd);
val tnv = (tn,td);

val (n,V.IVert d) = pnv;
val (tn,V.IVert td) = tnv;

val m = Match.get_mctxt (get_match st);
val V.Xnd a1 = d;
val V.Xnd a2 = td;
AngleExpr.match a1 a2 m;

MCtxt.print m;

AngleExpr.print a1;
AngleExpr.print a2;
AngleExpr.eq (AngleExpr.parse "a") a2;
val (c1,t1) = a1;
val (c2,t2) = a2;
val (a3 as (c3,t3)) = (AngleExpr.parse "a");

VName.NTab.list_of t1;
VName.NTab.list_of t2;
VName.NTab.list_of t3;

(AngleExpr.parse "a")


MCtxt.vertex_idata_match d td (Match.get_mctxt m)

val [tn1,tn2,tn3] = Seq.list_of (get_tgt_vnseq st);

val tn = tn2;
val SOME pn = (Vertex.NSet.get_local_bot (get_unm_pat_vs st));
V.print_name pn;
V.print_name tn;


(* LOOP *)


val SOME ((pn,tn),st) = pop_agenda st2;
V.print_name pn;
V.print_name tn;
print st;

val st = st |> match_lfvertex pn tn;
print st;
val [st2,st3] = Seq.list_of (edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns) (ltes,(plfs,st)));


val SOME st2 = (match_vertex (pn,pd) (tn,td) st)

val [(ltes,(plfs,st))] = (ltes,(plfs,st2))
                            |> match_self_loops pd tn lpes 
                            |> Seq.list_of;

edgematch_adj_verticies (pn,pd) (tn,td) (pns,tns)

val from_pd = pd;
val from_tn = tn;

val SOME ((teh, more_tehs)) = Edge.Ehn.NSet.pull_local_bot left_over_tehs;
E.Ehn.print_name teh;

raise ERROR "stop";

pop_agenda st;



val SOME Va = (Vertex.NSet.get_local_bot (get_unm_pat_vs st));
val [Vd, Ve, Vf] = Seq.list_of (get_tgt_vnseq st);

val n = Va;
val tn = Ve;

val SOME st2 = match_vertex (n,d) (tn,td) st;

match_2vertices_with_edges 

(* ERROR: only gives two results, should be three! *)
val Progress sq = do_next_match_choices st;
val l = Seq.list_of sq;
map print l;

raise ERROR "stop";

val st = hd l;
val Progress sq = do_next_match_choices st;


val SOME ((n,tn),st2) = pop_agenda st;
print st2;
val st = st2;

val (d,edgens) = Graph.get_vertex pat n;
val (td,tedgens) = Graph.get_vertex tgt tn;

val BVertex.IVert d = d;
val BVertex.IVert td = td;

val SOME st2 = match_vertex (n,d) (tn,td) st;

val l = Seq.list_of (MatchState.match g1 g2 []);

(Seq.list_of (match_2vertices_with_edges pat tgt n st2 tn));


val n = "Va";
val tn = "Vd";

print st;

val l = Seq.list_of (MatchState.match g1 g2 []);
*)
