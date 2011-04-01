


structure MyTester
= struct

  structure G = RGGraph;
  structure LS = RGGraphLoaderSaver;
  structure H = Histogram;
  structure HTree = HistTree;
  structure AList = AdjacencyList;
  structure ATree = AdjacencyTree;
  structure NSet = G.RGVertex.NSet;
  structure NTab = G.RGVertex.NTab;
  structure Lib = Library;
  structure RG = RandomGraph;
  structure MSt = MatchState;


  fun load (file : string) = LS.load_graph 
	((OS.FileSys.getDir ()) ^ "/indexing/test/" ^ file);
	
  fun save (name : string) graph = LS.save_graph 
  		((OS.FileSys.getDir ()) ^ "/indexing/test/saved/" ^ name) graph
	
  val ruleset = 
	["histtest/rule1.xml",
	 "histtest/rule2.xml",
	 "histtest/rule3.xml",
	 "histtest/rule4.xml",
	 "histtest/rule5.xml",
	 "histtest/rule6.xml"];
	 
  fun load_rule_number n = load ("rules/rule" ^ (Int.toString n) ^ ".xml");
  
  fun load_graph_with_name n  = 
  	(load ("rules/rule" ^ (Int.toString n) ^ ".xml"), 
  		"rule" ^ (Int.toString n));
  
  fun zipwith _ [] _ = []
    | zipwith _ _ [] = []
    | zipwith f (x::xs) (y::ys) = (f x y) :: zipwith f xs ys;
    
  
  val empty_names = (NSet.empty,Edge.NSet.empty,NSet.empty)
  
  fun pull_all xs seq =
  	case Seq.pull seq of
  	  NONE   => xs
  	| SOME (x,seqn) => pull_all (x::xs) seqn;
  	
  val test_compat = (fn () =>
  	let 
  	val pats = map load_graph_with_name (1 upto 9)
  	val adjs = map (fn (g,name) => (AList.get_adjacency g,name)) pats
  	in
  	map (fn (adj,name) => 
  		(adjs
  		|> map (fn (adj2,name2) => 
  			(zipwith AList.compatible adj adj2,name2)))
  		|> filter (fn (bools,name2) =>
  			List.foldr (fn (b1,b2) => b1 andalso b2) true bools)
  		|> map (fn (_,name2) => (name,name2))
  	    ) adjs
  	end
  );

end;


