
signature MYTESTER
= sig
  structure G : GRAPH
  structure LS : LOADSAVE
  structure H : HISTOGRAM
  structure HTree : HISTTREE
  
  val ruleset : G.T list
  val target_hist : H.T
end;


structure MyTester : MYTESTER
= struct

  structure G = RGGraph;
  structure LS = RGGraphLoaderSaver;
  structure H = Histogram;
  structure HTree = HistTree;


  fun load (file : string) = LS.load_graph 
	("test/" ^ file);
	
  fun save (name : string) graph = LS.save_graph 
  		("test/" ^ name) graph
	
  val ruleset = map load
	["rule1.xml",
	 "rule2.xml",
	 "rule3.xml",
	 "rule4.xml",
	 "rule5.xml",
	 "rule6.xml"];
	
  val target_hist = H.create_hist (load "tgt1.xml");
  
  fun get_names list = map (fn (name,_) => name : string);
	 

end;


