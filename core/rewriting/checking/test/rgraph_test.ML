(* Quick test of the features implemented by rgraph *)

local
open Test_MkG;
structure Rule = Test_Rule;
structure Ruleset = Test_Ruleset;
structure RulesetRewrites = Test_RulesetRewrites;
structure Match = RulesetRewrites.Match;
structure RewriteGraph = Test_RGraph;
(*structure RewriteGraph = RewriteGraph(Test_RulesetRewrites);*)
in

fun list_of_rule_matches r g = 
    let val (r', matches) = RulesetRewrites.rule_matches r g;
    in (r', Seq.list_of matches) end;

(* Create a dummy graph, a dummy rule and a dummy match *)
val (_,g) = G.empty |> G.add_vertex (mkZ zero_angle);
val r = Rule.mk (g,g);
val (r, matches) = list_of_rule_matches r g;
val m = List.hd matches 


(* First test that the implementation of the edge classification of a graph is
 * correct *)

val rg = RewriteGraph.init;

(* Create 7 graphs that will be the vertices of our graph *)
val g1 = G.empty;
val g2 = G.empty;
val g3 = G.empty;
val g4 = G.empty;
val g5 = G.empty;
val g6 = G.empty;
val g7 = G.empty;

val (n1,rg) = RewriteGraph.add_vertex g1 rg;
val (n2,rg) = RewriteGraph.add_vertex g2 rg;
val (n3,rg) = RewriteGraph.add_vertex g3 rg;
val (n4,rg) = RewriteGraph.add_vertex g4 rg;
val (n5,rg) = RewriteGraph.add_vertex g5 rg;
val (n6,rg) = RewriteGraph.add_vertex g6 rg;
val (n7,rg) = RewriteGraph.add_vertex g7 rg;

(* Now we want our graph to look like 
 *
 *       ______>1
 *      |      /   \
 *    b |     2    / 3
 *      \    |    /
 *       \__ |   / c
 *          _4<_/
 *        /  |
 *       |   |
 *     f |   5
 *       | /   \ 
 *       6      7
 *
 *  b is a back edge; f is a forward edge; c is a cross edge; the others are
 *  tree edges
 *)

val ed = (R.mk "a", (r, m));
val l = "other";
(* First the tree edges *)
val (e1,rg) = rg |> RewriteGraph.add_edge l ((n1, n2), ed);
val (e2,rg) = rg |> RewriteGraph.add_edge l ((n1, n3), ed);
val (e3,rg) = rg |> RewriteGraph.add_edge l ((n2, n4), ed);
val (e4,rg) = rg |> RewriteGraph.add_edge l ((n4, n5), ed);
val (e5,rg) = rg |> RewriteGraph.add_edge l ((n5, n6), ed);
val (e6,rg) = rg |> RewriteGraph.add_edge l ((n5, n7), ed);
(* b *)
val (e7,rg) = rg |> RewriteGraph.add_edge l ((n4, n1), ed);
(* f *)
val (e8,rg) = rg |> RewriteGraph.add_edge l ((n4, n6), ed);
(* c *)
val (e9,rg) = rg |> RewriteGraph.add_edge l ((n3, n4), ed);

(* Run the algorithm and check if the labels are correct *)
val () = rg |> RewriteGraph.print
val rg = rg |> RewriteGraph.treeify n1
val () = rg |> RewriteGraph.print
(*TODO: Make the compilation fail the edge types are not correct *)


(* Now test unification *)
val rg = RewriteGraph.init;
val g1 = G.empty |> G.add_vertex (mkZ zero_angle)
                 |> snd;
val g2 = G.empty |> G.add_vertex (mkZ zero_angle)
                 |> snd
                 |> G.add_vertex (mkZ zero_angle)
                 |> snd;
val g3 = G.empty |> G.add_vertex (mkZ zero_angle)
                 |> snd
                 |> G.add_vertex (mkZ zero_angle)
                 |> snd
                 |> G.add_vertex (mkZ zero_angle)
                 |> snd;
(* A graph with 4 vertices, g1 appears twice *)
val (n1, rg) = RewriteGraph.add_vertex g1 rg;
val (n2, rg) = RewriteGraph.add_vertex g1 rg;
val (n3, rg) = RewriteGraph.add_vertex g3 rg;
val (n4, rg) = RewriteGraph.add_vertex g4 rg;

val (e1,rg) = rg |> RewriteGraph.add_edge l ((n1,n3), ed);
val (e2,rg) = rg |> RewriteGraph.add_edge l ((n3,n2), ed);
val (e3,rg) = rg |> RewriteGraph.add_edge l ((n1,n4), ed);
val (e4,rg) = rg |> RewriteGraph.add_edge l ((n2,n1), ed);

val () = rg |> RewriteGraph.treeify n1
            |> RewriteGraph.print;
val rg = rg |> RewriteGraph.unify;
val () = rg |> RewriteGraph.treeify n1
            |> RewriteGraph.print;

end;
