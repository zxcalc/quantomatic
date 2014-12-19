

signature HISTTREE
= sig

  structure H : HISTOGRAM
  structure G : GRAPH
  
  type T
  type Bag

  val make_htree : G.T list -> T
  val lookup_htree : H.T -> T -> G.T list
end;

structure HistTree : HISTTREE
= struct
  
  structure H = Histogram;
  structure G = RGGraph;
  structure ITab = NameTabFun(IntName);
  
  type GraphWithData = G.T 
  type Bag = (G.T ref) list;
  
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
    				insert_into_htree (graph,hs) (Lf bag)) 
    				ITab.empty))
    (*if htree is a leaf but hist is not empty, we need to expand the htree.
      all remaining entries in the hist are not in the tree*)
      
    | insert_into_htree (graph,[]) (Br (vkt,tab)) =
    		Br (vkt,
    			ITab.map_all 
    			(fn br => insert_into_htree (graph, []) br)
    			tab
    		   )
    (*if hist is empty, the remainder of the htree cannot rule out this graph
      in a query, so propagate it to all leaves reachable from current branch*)
    				
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
    				ITab.map_all
    				(fn br => insert_into_htree (graph, hist) br)
    					tab)
    		(*this means that vkt is not in the current hist, and therefore
    		  provides no information about this graph for query purposes. 
    		  We propagate graph to all branches in next layer*)
    		  
    		|GREATER=> 
    			let 
    			val branch2 = insert_into_htree (graph, tl hist) branch
    			in
    			Br (vkh, ITab.ins (n,branch2) 
    				(ITab.ins (0, branch) ITab.empty))
    			end
    		(*vkh is not in tree, insert a new "layer" into the tree
    		  note we've inserted a zero entry*)
    		end
  
  and update_table gh num tab =
  	let
  	val (smaller,branch) = the (ITab.next_smaller tab num)
  	(*should never raise an exception, all things going to plan: there is 
  	  always a zero branch in each table, and zeroes cannot appear in
  	  histograms (or at least shouldn't)*)
  	in
  	(case ITab.lookup tab num of
  	  NONE => ITab.ins (num, branch) tab
  	  (*table has no entry with this number of the current vkind, 
  	  copy the branch from the next smallest entry*)
  	| SOME _ => tab) |>
  	ITab.map_all' (fn n => fn br =>
  		 if num <= n
  		 then insert_into_htree gh br
  		 else br)
  		 (*continue to insert graph into all subtrees corresponding to 
  		 the same or greater number of the current vkind*)
  	end
  	
  			
  
  fun make_htree bag = 
  	(bag	
  	|> map (fn graph => (ref graph,H.create_hist graph))
  	|> (fold insert_into_htree)) 
  	Empty;
  
  (*get functions should never throw exceptions, there is always a zero branch*)	
  fun lookup_htree' hist Empty	 	= []
    (*should never happen*)
    | lookup_htree' hist (Lf bag) 	= bag
    
    | lookup_htree' [] (Br (_,tab)) 	= lookup_htree' [] (ITab.get tab 0)
    (*hist is empty, move to zero branch at all remaining nodes*)
    | lookup_htree' ((vkh,num)::hs) 
    		   (Br (vkt, tab)) 	=
    
    	case VKind.kind_ord (vkh,vkt) of
    	 EQUAL  => (
    		   case ITab.lookup tab num of
    		     NONE => let
    			     val (_,branch) = the (ITab.next_smaller tab num)
    			     in
    			     lookup_htree' hs branch
    			     end
    		   | SOME branch => lookup_htree' hs branch
    		   )
    	(*follow path corresponding to num or the next smallest key*)
    		 
    	| LESS  => lookup_htree' ((vkh,num)::hs) (ITab.get tab 0)
    	(*vkt does not appear in hist, move to zero branch*)
    	
    	|GREATER=> lookup_htree' hs (Br (vkt,tab));
    	(*vkh does not appear in tree, move to next hist entry*)
    			

   fun lookup_htree hist htree = 
   	lookup_htree' hist htree
   	|> map (fn graphref => ! graphref)
  
 
  		
  
				
end;
