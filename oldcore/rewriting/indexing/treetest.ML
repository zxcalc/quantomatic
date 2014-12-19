structure TreeTester
= struct

  structure G = RGGraph;
  structure LS = RGGraphLoaderSaver;
  structure H = Histogram;
  structure HTree = HistTree;
  structure AList = AdjacencyList;
  structure ATree = AdjacencyTree;
  structure NSet = V.NSet;
  structure NTab = V.NTab;
  structure Lib = Library;
  structure RG = RandomGraph;
  structure MSt = MatchState;
  structure M = Match;
  
   
  (*result generators*)
  
  (*outputs 5-tuple : (number of rules, leaves, bags (non-empty), max depth,
  			graphs - to ensure none are duplicated*)
  fun test_atree patgen ruleset_range scale = map (fn n => 
  				let 
  				val pats = patgen (n*scale)
  				val atree= ATree.create_atree_from_graphlist pats
  				val (n1,n2,n3,n4) = ATree.test_tree atree
  				in
  				(n*scale,n1,n2,n3,n4)
  				end) ruleset_range;
  (*outputs 5-tuple : (number of rules, leaves, max depth, av depth, graphs*)				
  fun test_htree patgen ruleset_range scale = map (fn n => 
  				let 
  				val pats = patgen (n*scale)
  				val htree= HTree.make_htree pats
  				val (n1,n2,n3,n4) = HTree.test_tree htree
  				in
  				(n*scale,n1,n2,n3,n4)
  				end) ruleset_range;
  				
  fun std_atree_test min max = test_atree RG.std_pattern_set (min upto max) 20;
  fun std_htree_test min max = test_htree RG.std_pattern_set (min upto max) 20;
  fun simple_htree_test min max = test_htree RG.simple_pattern_set (min upto max) 20;
  
  
  fun varied_atree_test min max = test_atree RG.varied_pattern_set (min upto max) 20;
  (*output wrapper*)
  
 local 
 open TextIO
 in
 fun output results filename =
 	let 
 	val dir = "indexing/test/results/" ^ filename
 	val ostream = openAppend dir
 	val show = Int.toString
 	val showreal = Real.toString
 	in
 	ostream;
 	map (fn (nrules, nleaves, n3, n4, ngraphs) =>
 		outputSubstr (ostream, Substring.full (
 					(show nrules) ^ "\t" ^
 					(show nleaves) ^ "\t" ^
 					(show n3) ^ "\t" ^
 					(show n4) ^ "\t" ^
 					(showreal ngraphs) ^ "\n")
 					)
 			     ) results;
 	closeOut ostream
 	end
 
 fun test_out thing name reps = 
 	map (fn n => output (thing ()) name) (1 upto reps)
 	
 end;  
  
end;
