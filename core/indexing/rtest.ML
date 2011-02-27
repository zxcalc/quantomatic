structure Tester
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
  structure M = Match;
 
  
(*general utilities*)
  fun time_wrapper m n (thing : unit -> 'a) =
  	let
  	val cput = Timer.startCPUTimer ()
  	val rt = Timer.startRealTimer ()
  	in
  	cput;
  	rt;
  	(thing (), m (*number of rules*),n (*tgt size*),
  		Timer.checkCPUTimes cput ,Timer.checkRealTimer rt)
  	end;

  val empty_names = (NSet.empty,Edge.NSet.empty,NSet.empty)
  
  fun pull_all xs seq =
  	case Seq.pull seq of
  	  NONE   => xs
  	| SOME (x,seqn) => pull_all (x::xs) seqn;
  	
  fun matched xs = List.length (filter (fn x => case x of [] => false | _ => true) xs);
  
  fun matches xs = List.foldr op+ 0 (map List.length xs);
  
  
  fun make_time (cput as {
                        nongc : {
                          usr : Time.time,
                          sys : Time.time
                        },
                        gc : {
                          usr : Time.time,
                          sys : Time.time
                        }
                      }) rt = 	
  	let
  	val tot_cput = #usr (#nongc cput) + #sys (#nongc cput) +
  			#usr (#gc cput) + #sys (#gc cput)
  	in 
  	
  	case Time.compare (tot_cput,rt) of
  	    GREATER => tot_cput
  	   | _      => rt
  	end;
  
  fun process_results xs = 
  	map 
  		(map (fn (matchlist,nrules,tgtsize,cputime,rtime) => 
  			(matched matchlist,matches matchlist,nrules,tgtsize,
  				make_time cputime rtime
  				))) xs;	
  
 
  (*get internal vertex names from a graph*)
  fun get_inames graph = 
  	let
  	val vnames = G.get_vnames graph
  	val bnames = G.get_boundary graph
  	in
  	NSet.remove_set bnames vnames
  	end;
  				
 (*output to file*)
 local 
 open TextIO
 in
 fun output results filename =
 	let 
 	val dir = "indexing/test/results/" ^ filename
 	val ostream = openAppend dir
 	val show = Int.toString
 	in
 	ostream;
 	map (fn (num_matched, num_matches, nrules, tgtsize, time) =>
 		outputSubstr (ostream, Substring.full (
 					(show num_matched) ^ "\t" ^
 					(show num_matches) ^ "\t" ^
 					(show nrules) ^ "\t" ^
 					(show tgtsize) ^ "\t" ^
 					(show (Time.toMilliseconds time)) ^ "\n")
 					)
 			     ) results;
 	closeOut ostream
 	end
 
 fun test_out thing name reps = 
 	Seq.map (fn n => output (Lib.flat (thing ())) name) (Seq.of_list (1 upto reps))
 	|> pull_all [];
 end;
 
 
 
(*current Matching algorithm*)
  
  (*generates a ruleset and matches it against the given tgt graph*)
  fun matching pats tgt =
  	map (pull_all []) 
  		(map (fn pat => MSt.match empty_names pat tgt NTab.empty) pats)
  
  
  fun match_test (gg as graph_generator) 
  	(*ranges of values to try*)ruleset_range tgt_range
  	(*scaling factors for each range*)ruleset_scale tgt_scale=  
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.pattern_set gg (n*ruleset_scale)
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = gg (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					matching pats tgt
  					)
  			end) tgt_range
  		end) ruleset_range)
  	|> process_results;
  	
  		
  
  val test_simple = match_test RG.simple_rgraph;
  val test_std = match_test RG.std_rgraph;
  
  (*histogram*)
  fun hist_matching tgt htree =
  	let
  	val pats = HTree.lookup_htree tgt htree
  	in
  	map (pull_all []) 
  		(map (fn pat => MSt.match empty_names pat tgt NTab.empty) pats)
  	end;
  	
  
  fun hist_match_test (gg as graph_generator) 
  	(*ranges of values to try*)ruleset_range tgt_range
  	(*scaling factors for each range*)ruleset_scale tgt_scale=  
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.pattern_set gg (n*ruleset_scale)
  		val htree = HTree.make_htree pats
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = gg (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					hist_matching tgt htree
  					)
  			end) tgt_range
  		end) ruleset_range)
  	|> process_results;
  	
  	
  (*adjacency*)
  
  fun adj_matching tgt atree =
  	let
  	val names = (NSet.list_of o get_inames) tgt
  	val ktable = AList.get_kind_table tgt
  	in
  	map (pull_all []) 
  		(Lib.flat (map (fn name => 
  			let
  			val prematches = ATree.lookup_atree tgt name atree ktable
  			in
  			map (fn (pat,pre_agenda) =>
  				MSt.match empty_names pat tgt pre_agenda) prematches
  			end) names))
  	end;
  	
  fun adj_match_test (gg as graph_generator) 
  	(*ranges of values to try*)ruleset_range tgt_range
  	(*scaling factors for each range*)ruleset_scale tgt_scale=  
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.pattern_set gg (n*ruleset_scale)
  		val atree = ATree.create_atree_from_graphlist pats
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = gg (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					adj_matching tgt atree
  					)
  			end) tgt_range
  		end) ruleset_range)
  	|> process_results;
  	
  (*comparing two approaches tests. random graphs are generated only once and shared between algorithms*)
  
  fun adj_vs_match (gg as graph_generator)
  	(*ranges of values to try*) ruleset_range tgt_range
  	(*scaling factors for each range*) ruleset_scale tgt_scale= 
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.pattern_set gg (n*ruleset_scale)
  		val atree = ATree.create_atree_from_graphlist pats
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = gg (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			[time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					adj_matching tgt atree
  					),
  			 time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					matching pats tgt
  					)]
  			end) tgt_range
  		end) ruleset_range)
  	|> map process_results;
  	
  fun varied_adj_vs_match
  	(*ranges of values to try*) ruleset_range tgt_range
  	(*scaling factors for each range*) ruleset_scale tgt_scale= 
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.varied_pattern_set (n*ruleset_scale)
  		val atree = ATree.create_atree_from_graphlist pats
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = RG.std_rgraph (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			[time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					adj_matching tgt atree
  					),
  			 time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					matching pats tgt
  					)]
  			end) tgt_range
  		end) ruleset_range)
  	|> map process_results;
  	
  fun hist_vs_match (gg as graph_generator)
  	(*ranges of values to try*) ruleset_range tgt_range
  	(*scaling factors for each range*) ruleset_scale tgt_scale= 
  	(map (fn n (*vary pattern set size*) =>
  		let 
  		val pats = RG.pattern_set gg (n*ruleset_scale)
  		val htree = HTree.make_htree pats
  		in 
  		map (fn m (*vary tgt size*) =>
  			let
  			val tgt = gg (m*tgt_scale) (m*(tgt_scale div 10)) 0
  			in
  			[time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					hist_matching tgt htree
  					),
  			 time_wrapper (n*ruleset_scale) (m*tgt_scale)
  				(fn () =>
  					matching pats tgt
  					)]
  			end) tgt_range
  		end) ruleset_range)
  	|> map process_results;
  			
  	
  (*standard tests*)
  
  fun std_match min max = match_test RG.std_rgraph (min upto max) (min upto max) 10 50;
  fun simple_match min max = match_test RG.simple_rgraph (min upto max) (min upto max) 10 50;
  
  fun std_hist min max = hist_match_test RG.std_rgraph (min upto max) (min upto max) 10 50;
  fun simple_hist min max = hist_match_test RG.simple_rgraph (min upto max) (min upto max) 10 50;
  
  fun std_adj min max = adj_match_test RG.std_rgraph (min upto max) (min upto max) 10 50;
  fun simple_adj min max = adj_match_test RG.simple_rgraph (min upto max) (min upto max) 10 50;
  
  (*ruleset vs constant target size*)
  
  fun std_rule_match min max = match_test RG.std_rgraph (min upto max) (Lib.replicate 20 1) 5 500;
  fun std_rule_adj min max = adj_match_test RG.std_rgraph (min upto max) (Lib.replicate 20 1) 5 500;
  fun std_rule_hist min max = hist_match_test RG.std_rgraph (min upto max) (Lib.replicate 20 1) 5 500;
  
  (*constant ruleset vs target size*)
  
  fun std_tgt_match min max = match_test RG.std_rgraph (Lib.replicate 20 1) (min upto max) 100 50;
  fun std_tgt_adj min max = adj_match_test RG.std_rgraph (Lib.replicate 20 1) (min upto max) 100 50;
  fun std_tgt_hist min max = hist_match_test RG.std_rgraph (Lib.replicate 20 1) (min upto max) 100 50;
  
  (*vs tests*)
  
  fun std_adj_vs_match min max = adj_vs_match RG.std_rgraph (min upto max) (min upto max) 40 400;
  
  fun std_hist_vs_match min max = hist_vs_match RG.std_rgraph (min upto max) (min upto max) 10 100;
  
 local 
 open TextIO
 fun output results filename =
 	let 
 	val dir = "indexing/test/results/" ^ filename
 	val ostream = openAppend dir
 	val show = Int.toString
 	val showreal = Real.toString
 	in
 	ostream;
 	map (fn [(num_matched1, num_matches1, nrules1, tgtsize1, time1),
 		 (num_matched2, num_matches2, nrules2, tgtsize2, time2)]
 		 =>
 		outputSubstr (ostream, Substring.full (
 					(show num_matched1) ^ "\t" ^
 					(show num_matches1) ^ "\t" ^
 					(show nrules1) ^ "\t" ^
 					(show tgtsize1) ^ "\t" ^
 					(show (Time.toMilliseconds time1)) ^ "\t" ^
 					(show num_matched2) ^ "\t" ^
 					(show num_matches2) ^ "\t" ^
 					(show nrules2) ^ "\t" ^
 					(show tgtsize2) ^ "\t" ^
 					(show (Time.toMilliseconds time2)) ^ "\n")
 					)
 			     ) results;
 	closeOut ostream
 	end
 in
 
 fun test_out_vs thing name reps = 
 	Seq.map (fn n => output (Lib.flat (thing ())) name) (Seq.of_list (1 upto reps))
 	|> pull_all [];
 end;
  	
  
 
  				
end;
