structure AdjacencyTree
= struct

  structure AList = AdjacencyList;
  structure G = RGGraph;
  structure V = G.RGVertex;
  structure Vk = VKind;
  structure NSet = V.NSet;
  structure NTab = V.NTab;
  structure R = Rule;
  type N = V.name;
  
  (*graph with rule data not necessary for processing atree*)
  type GData = N;
  type GraphWithData = G.T * GData;
  type Bag = GraphWithData list;
  
  datatype T = Br of Bag * ((AList.Entry * T) list)
  	     | Lf of Bag
  	     | Empty;
  	     
  fun prepare_rule name (graph,(startv,rule)) =
  	(NTab.doadd (startv,name) NTab.empty, rule);
  	     
  
  fun insert_into_atree (graphwd,[]) Empty = Lf [graphwd]
    | insert_into_atree (graphwd,e::es) Empty = 
  			Br([],
  				[(e,insert_into_atree (graphwd,es) Empty)]
  			   )
    | insert_into_atree (graphwd,[]) (Lf bag) = Lf (graphwd::bag)
    | insert_into_atree (graphwd,e::es) (Lf bag) = 
  	     		Br (bag,
    				[(e,insert_into_atree (graphwd,es) Empty)]
    			   )
    | insert_into_atree (graphwd,[]) (Br (bag,brs)) =
    			Br ((graphwd::bag),brs)
    | insert_into_atree (graphwd,e::es) (Br (bag,brs)) =
    			let
    			fun insert_into_list [] = 
    				(e, insert_into_atree (graphwd,es) Empty)::[]
    			  | insert_into_list ((t_entry,br)::xs) =
    			  	case AList.entry_ord (e, t_entry) of
    			  	EQUAL => (t_entry, insert_into_atree (graphwd,es) br) :: xs
    			  	|LESS => (t_entry, br) :: insert_into_list xs
    			  	|GREATER => (e, insert_into_atree (graphwd,es) Empty) :: (t_entry,br)  :: xs
    			in
    			Br (bag,insert_into_list brs)
    			end;
    				
  fun create_atree_from_ruleset graphs_with_data =
  	(graphs_with_data
  	|> map (fn (graph,startv) => 
  		((graph,startv),
  			AList.build_adj_list graph startv))
  	|> fold insert_into_atree) Empty;
  (*	
  fun create_atree_from_ruleset ruleset =
  	let
  	val rule_list = NTab.list_of ruleset
  	val graphs_with_data = map (fn (rname,(rule,flag)) =>
  		let
  		val lhs = R.get_lhs rule
  		in
  		(lhs, (AList.select_biggest lhs, (rname,rule)))
  		end) rule_list
  	in
  	create_atree_from_ruleset' graphs_with_data
  	end;
  *)	
  fun create_atree_from_graphlist graphs =
  	(graphs
  	|> map (fn graph => 
  		((graph,AList.select_biggest graph),
  			AList.get_adjacency graph))
  	|>fold insert_into_atree) Empty;
  
  	
  (*Improve: target adjacencylists/contours are generated on the fly. Implement 
    them as a lazy list? or calculate them beforehand based on the max depth of 
    the tree?*)
  fun prune_atree tgt_graph tgt_ktable vnames Empty = Empty
    | prune_atree tgt_graph tgt_ktable vnames (Lf bag) = Lf bag
    | prune_atree tgt_graph tgt_ktable vnames (Br (bag,brs)) =
    	if NSet.is_empty vnames 
    	then Br (bag,brs)
    	else
    		let
    		val tgt_entry = AList.make_entry vnames tgt_ktable
    		val (graph2,vnames2) = AList.pull_next_layer (tgt_graph,vnames)
    		in
    		Br (bag,
    			map (fn (pat_entry,br) =>
    			    if AList.compatible tgt_entry pat_entry
    			    then (pat_entry, prune_atree graph2 tgt_ktable vnames2 br)
    			    else (tgt_entry,Lf [])) brs)
    		end;
    			  
    					
  fun get_graphs Empty = []
    | get_graphs (Lf bag) = bag
    | get_graphs (Br (bag,brs)) =
    	bag @ maps (fn (pat_entry,br) => get_graphs br) brs;
    	
  fun lookup_atree graph name atree ktable =
  	let
  	val init_nset = NSet.single name
  	in
  	prune_atree graph ktable init_nset atree
  	|> get_graphs
  	|> map (fn (graph,startv) => (graph,NTab.doadd (startv,name) NTab.empty))
  	end;
  

(*information about tree*)	
  fun leaves Empty = 0
    | leaves (Lf _) = 1
    | leaves (Br (_,brs)) = 
    	brs
    	|> map (fn (_,br) => leaves br)
    	|> List.foldr op+ 0;
    	
   fun bags Empty = 0
     | bags (Lf bag) = 
     	(case bag of
     	 [] => 0
     	| _ => 1)
     | bags (Br (bag,brs)) =
     	(case bag of
     	 [] => 0
     	| _ => 1 ) + (brs 
     		|> map (fn (_,br) => bags br)
     		|> List.foldr op+ 0);
     		
   fun graphs Empty = 0
     | graphs (Lf bag) = List.length bag
     | graphs (Br (_,brs)) =
     	brs
    	|> map (fn (_,br) => graphs br)
    	|> List.foldr op+ 0;
    	
   fun graphs' Empty = 0
     | graphs' (Lf bag) = List.length bag
     | graphs' (Br (bag,brs)) =
     	(brs
    	|> map (fn (bag,br) => graphs' br)
    	|> List.foldr op+ 0) + List.length bag;
     		
   fun depth Empty = 0
     | depth (Lf _ ) = 0
     | depth (Br (bag,brs)) =
     	brs
     	|> map (fn (_,br) => 1 + depth br)
     	|> List.foldr (fn (n, m) =>
     			if n > m then n else m) 0;
     			
    fun first_width Empty = 0
      | first_width (Lf bag) = 0
      | first_width (Br (bag,brs)) =
      	List.length brs
      	
    fun avdepth Empty = 0.0
      | avdepth (Lf bag) = 0.0
      | avdepth (Br (bag,brs)) =
      	let
      	val length = List.length brs
      	val sum = List.foldr op+ 0.0
      	fun av xs = (sum xs) / (Real.fromInt length)
      	in
      	av (map (fn (entry,br) => 1.0 + avdepth br) brs)
      	end;
      	
    fun balance atree = (Real.fromInt (depth atree)) - (avdepth atree);
      	
     			
    fun test_tree atree =
    	(leaves atree,
    	 depth atree,
    	 first_width atree,
    	 balance atree
    	 )
   
end;
