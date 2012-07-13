(* Test that the rewrite tree module behaves as expected *)

local
structure RTree = Test_RTree;
structure Ruleset = RTree.Ruleset;
structure Rule = RTree.Rule;
structure G = RTree.G;
structure RGraph = RTree.RewriteGraph;

structure IVData = RG_InternVData;
structure OVData = RTree.G.OVData;

in

 (* First test : create one rule b - X - X - b => b - X - b
  * and make it rewrite : X - X - X - X *) 

 val bvert = OVData.EVert;
 val X = OVData.IVert (IVData.Xnd LinratAngleExpr.zero);
 val Z = OVData.IVert (IVData.Znd LinratAngleExpr.zero);
 val undir_edge = (G.UnDirected, ());

 val (nl1,l) = G.empty |> G.add_vertex bvert;
 val (nl2,l) = G.add_vertex X l;
 val (nl3,l) = G.add_vertex X l;
 val (nl4,l) = G.add_vertex bvert l;

 val (el1, l) = G.add_edge undir_edge nl1 nl2 l; 
 val (el2, l) = G.add_edge undir_edge nl2 nl3 l; 
 val (el3, l) = G.add_edge undir_edge nl3 nl4 l; 

 val (nr1,r) = G.empty |> G.add_named_vertex nl1 bvert;
 val (nr2,r) = G.add_vertex X r;
 val (nr3,r) = G.add_named_vertex nl4 bvert r;

 val (er1, r) = G.add_edge undir_edge nr1 nr2 r; 
 val (er2, r) = G.add_edge undir_edge nr2 nr3 r; 

 val rule = Rule.mk (l,r); 

 val ruleset = Ruleset.empty |> Ruleset.add_fresh_rule (R.mk "r1", rule)
                             |> uncurry Ruleset.activate_rule ;

 val (n1, g) = G.empty |> G.add_vertex X;
 val (n2, g) = G.add_vertex X g;
 val (n3, g) = G.add_vertex X g;
 val (n4, g) = G.add_vertex X g;
 val (n5, g) = G.add_vertex X g;

 val (e1, g) = G.add_edge undir_edge n1 n2 g;
 val (e2, g) = G.add_edge undir_edge n2 n3 g;
 val (e3, g) = G.add_edge undir_edge n3 n4 g;
 val (e4, g) = G.add_edge undir_edge n4 n5 g;

  val rtree = RTree.init g ruleset; 
  fun rewrite (true, rt) = rewrite (RTree.next rt)
    | rewrite (false, rt) = rt 

  val rgraph = RGraph.print (RTree.get_rgraph (rewrite (true,rtree)));


 (* Now add one more rule : b - X - Z - X - b =>  b - Z - X - b*)
 val (nl1,l) = G.empty |> G.add_vertex bvert;
 val (nl2,l) = G.add_vertex X l;
 val (nl3,l) = G.add_vertex Z l;
 val (nl5,l) = G.add_vertex X l;
 val (nl4,l) = G.add_vertex bvert l;

 val (el1, l) = G.add_edge undir_edge nl1 nl2 l; 
 val (el2, l) = G.add_edge undir_edge nl2 nl3 l; 
 val (el3, l) = G.add_edge undir_edge nl3 nl5 l; 
 val (el4, l) = G.add_edge undir_edge nl5 nl4 l; 

 val (nr1,r) = G.empty |> G.add_named_vertex nl1 bvert;
 val (nr2,r) = G.add_vertex Z r;
 val (nr4,r) = G.add_vertex X r;
 val (nr3,r) = G.add_named_vertex nl4 bvert r;

 val (er1, r) = G.add_edge undir_edge nr1 nr2 r; 
 val (er2, r) = G.add_edge undir_edge nr2 nr4 r; 
 val (er3, r) = G.add_edge undir_edge nr4 nr3 r; 
 
 val rule' = Rule.mk (l,r);
 val ruleset = ruleset |> Ruleset.add_fresh_rule (R.mk "r2", rule')
                       |> uncurry Ruleset.activate_rule;


 val (n1,g) = G.empty |> G.add_vertex X;                      
 val (n2,g) = G.add_vertex X g;
 val (n3,g) = G.add_vertex X g;
 val (n4,g) = G.add_vertex Z g;
 val (n5,g) = G.add_vertex X g;
 val (n6,g) = G.add_vertex Z g;
 val (n7,g) = G.add_vertex X g;
 val (n8,g) = G.add_vertex X g;
 val (n9,g) = G.add_vertex X g;
 val (n10,g) = G.add_vertex X g;

 val (_,g) = G.add_edge undir_edge n1 n2 g;
 val (_,g) = G.add_edge undir_edge n2 n3 g;
 val (_,g) = G.add_edge undir_edge n3 n4 g;
 val (_,g) = G.add_edge undir_edge n4 n5 g;
 val (_,g) = G.add_edge undir_edge n5 n6 g;
 val (_,g) = G.add_edge undir_edge n6 n7 g;
 val (_,g) = G.add_edge undir_edge n7 n8 g;
 val (_,g) = G.add_edge undir_edge n8 n9 g;
 val (_,g) = G.add_edge undir_edge n9 n10 g;

 val rtree = RTree.init g ruleset; 
 fun rewrite (true, rt) = rewrite (RTree.next rt)
    | rewrite (false, rt) = rt 
 val rgraph = RGraph.print (RTree.get_rgraph (rewrite (true,rtree)));
end;
