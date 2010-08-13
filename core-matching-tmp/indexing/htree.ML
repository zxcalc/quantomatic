

signature HISTTREE
= sig

  structure H : HISTOGRAM
  structure G : GRAPH
  
  type T
  type Bag

  val make_htree : Bag -> T
  val lookup_htree : G.T -> T -> Bag
  val test_tree : T -> int * int * int * int
end;

structure HistTree : HISTTREE
= struct
  
  structure H = Histogram;
  structure G = RGGraph;
  structure ITab = NameTabFun(IntName);
  
  type GraphWithData = G.T 
  type Bag = G.T list;
  
  datatype HTree = Br of (VKind.T * HTree ITab.T)
  		 | Lf of Bag
  		 | Empty;
  		
  type T = HTree;
         	
         	
  (*build/alter htree in incremental fashion. The Empty type should only be encountered
    on the first pass of a build. ITab.ins shouldn't raise exceptions as it's only used
    in situations where the vertex kind isn't in the tree. The one exception is the
    EQUALS case below, but it's been guarded against there. Zero branches are created
    whenever a new vertex kind is added to the tree*)
    
  fun insert_into_htree (graph,[]) Empty          = Lf [graph]
    (*if htree and histogram are empty, create a leaf*)
    
    | insert_into_htree (graph,(vkh,n)::hs) Empty = 
    		Br (vkh,
    			ITab.ins (0,Lf []) (*assume zero cannot appear in hist*)
    			(ITab.ins (n,
    				insert_into_htree (graph,hs) Empty )
    				ITab.empty))
    (*if htree is empty, we know vkh is not in the tree*)
  
    | insert_into_htree (graph,[]) (Lf bag) 	  = Lf (graph::bag)
    (*if hist is empty and htree is a leaf, add graph to bag*)
    
    | insert_into_htree (graph,(vkh,n)::hs) (Lf bag)
    						  =
    		Br (vkh,
    			ITab.ins (0, Lf bag)
    			(ITab.ins (n,
    				insert_into_htree (graph,hs) (Lf [])) 
    				ITab.empty))
    (*if htree is a leaf but hist is not empty, we need to expand the htree.
      all remaining entries in the hist are not in the tree*)
      
    | insert_into_htree (graph,[]) (Br (vkt,tab)) =
    		Br (vkt, 
    			ITab.map_entry (insert_into_htree (graph, [])) 0 tab
    		   )
    (*if hist is empty, the remainder of the htree cannot rule out this graph
      in a query, so propagate it to the zero branch at all remaining levels*)
    				
    | insert_into_htree (graph, hist)
    			(branch as (Br (vkt, tab)))
    			   	  		  =
    		let
    		val (vkh,n) = hd hist
    		in
    		case VKind.kind_ord (vkh,vkt) of
    		  EQUAL => Br (vkt,
    		  		update_table (graph, tl hist) n tab)
    		(*see below for update_table function.
    		  if necessary it creates a new table entry for this amount of
    		  the current vkind, and propagates the graph to all branches
    		  corresponding to this number or greater*)
    		  
    		| LESS  => Br (vkt,
    				ITab.map_entry (insert_into_htree (graph, hist)) 0 tab
    			      )
    		(*this means that vkt is not in the current hist, and therefore
    		  provides no information about this graph for query purposes. 
    		  We propagate graph to the zero branch of the next layer*)
    		  
    		|GREATER=> 
    			let 
    			val branch2 = insert_into_htree (graph, tl hist) (Lf [])
    			in
    			Br (vkh, ITab.ins (n,branch2) 
    				(ITab.ins (0, branch) ITab.empty))
    			end
    		(*vkh is not in tree, insert a new "layer" into the tree
    		  note we've inserted a zero entry*)
    		end
  
  and update_table gh num tab =
  	case ITab.lookup tab num of
  	  NONE => ITab.ins (num, insert_into_htree gh (Lf []) ) tab
  	  (*table has no entry with this number of the current vkind*)
  	| SOME branch => ITab.update (num,insert_into_htree gh branch) tab; 
  	
  fun make_htree bag = 
  	(bag	
  	|> map (fn graph => (graph,H.create_hist graph))
  	|> (fold insert_into_htree)) 
  	Empty;
  

  (*Zero branch always exists, so this shouldn't throw exceptions*)	
  fun prune_htree hist Empty = Empty
    | prune_htree hist (Lf bag) = Lf bag
    | prune_htree [] (Br (vkt,tab)) =
    	Br (vkt,
    			ITab.ins (0, prune_htree [] (ITab.get tab 0)) 
    			ITab.empty)
    		
    | prune_htree hist (Br (vkt, tab)) =
    	let
    	val (vkh,n) = hd hist
    	in
    	case VKind.kind_ord (vkh,vkt) of
    	EQUAL =>
    		Br (vkt,
    			ITab.map_all' (fn m => fn br =>
    				if n >= m 
    				then prune_htree (tl hist) br
    				else Lf []
    				) tab
    		)
    	(*LESS => vkt not in hist, all graphs that require more than 0 of vkt 
    	  will not match and so only zero branch should be kept*)
    	|LESS =>
    		Br (vkt,
    			ITab.ins (0, prune_htree (tl hist) (ITab.get tab 0)) 
    			ITab.empty)
    	(*GREATER => vkh not in htree, it therefore provides no information about
    	  rules, so ignore it*)
    	|GREATER=>
    		prune_htree (tl hist) (Br (vkt,tab))
    			
    	end;
   
    	
  fun get_graphs Empty = []
    | get_graphs (Lf bag) = bag
    | get_graphs (Br (_,tab)) =
    	(tab
    	|> ITab.fold (fn (_,br) => fn l =>
    		 (get_graphs br) @ l)
    	) [];
  
  	
  fun lookup_htree graph htree =
  	(get_graphs ( prune_htree (H.create_hist graph) htree))
    			

(*information about htree*)	

  fun leaves Empty = 0
    | leaves (Lf _) = 1
    | leaves (Br (_,tab)) =
    	(tab
    	|> ITab.map_all' (fn _ => fn br =>
    			leaves br)
    	|> ITab.fold (fn (name,num1) => fn num2 =>
    			num1 + num2)) 0;
    			
  fun depth Empty = 0
    | depth (Lf _) = 0
    | depth (Br (_,tab)) =
    	(tab
    	|> ITab.map_all' (fn _ => fn br =>
    			1 + depth br)
    	|> ITab.fold (fn (name,num1) => fn num2 =>
    			if num1 > num2 then num1 else num2)) 0;
    			
  fun av_depth htree =
  	let
  	fun all_depths Empty = [0]
  	  | all_depths (Lf _) = [0]
  	  | all_depths (Br (_,tab)) =
  	  	(tab
  	  	|> ITab.map_all' (fn _ => fn br =>
  	  			map (fn m => m + 1) (all_depths br))
  	  	|> ITab.fold (fn (_,l1) => fn l2 => l1 @ l2)) []
  	val num_of_leaves = leaves htree
  	in
  	(List.foldr op+ 0 (all_depths htree)) div num_of_leaves
  	end;
  	
  fun graphs Empty = 0
    | graphs (Lf bag) = List.length bag
    | graphs (Br (_ , tab)) =
    	(tab
    	|> ITab.map_all' (fn _ => fn br => graphs br)
    	|> ITab.fold (fn (_,num1) => fn num2 =>
    			num1 + num2)) 0;
  	
  fun test_tree htree =
  	(leaves htree,
  	 depth htree,
  	 av_depth htree,
  	 graphs htree);
  	 
  	  	
  	
    			
   
    			
  
 
  		
  
				
end;
