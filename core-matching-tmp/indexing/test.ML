


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
  	
  (*------------------------------------------------------------------------*)
  (*testing random graph generator, trying to get a feel for how sizes affect 
    number of matches etc.*)
  	
    
  fun match1 nr nv nb ne=
  	let
  	val pattern_set = RG.std_pattern_set nr
  	val tgt = RG.std_rgraph nv nb ne
  	in
  	map (pull_all []) 
  		(map (fn pat => MSt.match empty_names pat tgt NTab.empty) pattern_set)
  	end;
  	
  	
  fun match2 nr mxv mxb =
  	(fn tgt =>
  	let
  	val mxv' = if mxv < 3 then 3 else mxv
  	val mxb' = if mxb < 2 then 2 else mxb
  	val pattern_set = RG.std_pattern_set nr
  	in
  	map (pull_all []) 
  		(map (fn pat => MSt.match empty_names pat tgt NTab.empty) pattern_set)
  	end);
  
  	
  fun matched xs = List.length (filter (fn x => case x of [] => false | _ => true) xs);
  fun matches xs = List.length (Lib.flat xs);
 
  fun test xs = (matched xs,matches xs);
  
  fun av_test xs = (fold (fn (md,ms) => fn n => md + n) (map test xs) 0) div (List.length xs) 
  
  
  
  fun test_tgt_size n rep =
  	map (fn m => 
  		map (fn _ => match1 30 (m*50) (m*2) 0) (1 upto rep)) 
  			(1 upto n)
  	|> map av_test;
  
  fun test_ruleset_size n rep =
  	map (fn m => 
  		map (fn _ => match1 (m*10) 500 10 0) (1 upto rep)) 
  			(1 upto n)
  	|> map av_test;
	
  fun test_extra_size n rep =
  	map (fn m => 
  		map (fn _ => match1 100 500 10 m) (1 upto rep)) 
  			(1 upto n)
  	|> map av_test;
  	
  fun test_rule_size nr mxv rep tgtsize =
  	let
  	val tgt = RG.std_rgraph tgtsize (tgtsize div 20) 0
  	in
  	map (fn m =>
  		map (fn _ => match2 30 m (m div 10) tgt) (1 upto rep))
  			(3 upto mxv)
  	|> map av_test
  	end;
  
  (*--------------------------------------------------------------------------*)
  (*testing time library*)
  
  fun test_timer' pat tgt = 
  	let
  	val cput = Timer.startCPUTimer ()
  	val rt = Timer.startRealTimer ()
  	val answer = (fn _ => pull_all [] (MSt.match empty_names pat tgt NTab.empty))
  	in
  	
  	answer ();
  	(Timer.checkCPUTimes cput ,Timer.checkRealTimer rt)
  	
  	end;
  	
  fun time_wrapper thing =
  	let
  	val cput = Timer.startCPUTimer ()
  	val rt = Timer.startRealTimer ()
  	val answer = (fn () => thing)
  	in
  	(answer (),Timer.checkCPUTimes cput ,Timer.checkRealTimer rt)
  	end;
  	
  fun time_wrapper2 thing =
  	let
  	val cput = Timer.startCPUTimer ()
  	val rt = Timer.startRealTimer ()
  	val answer = (fn _ => fn _ => fn _ => thing) cput rt
  	in
  	answer ();
  	(Timer.checkCPUTimes cput ,Timer.checkRealTimer rt)
  	end;
  	
  val test_timer = (fn () =>
  	let
  	val pat = RG.std_rgraph 3 1 0
  	val tgt = RG.std_rgraph 500 20 0
  	in
  	test_timer' pat tgt
  	end);
  	
  fun time_wrapper3 (thing : unit -> 'a) =
  	let
  	val cput = Timer.startCPUTimer ()
  	val rt = Timer.startRealTimer ()
  	in
  	cput;
  	rt;
  	(thing (),Timer.checkCPUTimes cput ,Timer.checkRealTimer rt)
  	end;

end;


