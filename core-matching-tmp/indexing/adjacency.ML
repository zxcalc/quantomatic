

structure AdjacencyList
= struct
  
  structure G = RGGraph;
  structure V = G.RGVertex;
  structure Vk = VKind;
  structure NSet = V.NSet;
  structure NTab = V.NTab;
  
  
  (*Starting at a vertex we generate an ordered list of Vkinds connected to it.
    we then consider the initial vertex and its adjacents as a subgraph and find
    the vertices connected to this subgraph but not inside it (vertices are actually deleted
    from the graph in practice). This proceeds iteratively until we have what we need or the 
    graph is covered by the alist.*)
  type Entry = Vk.T list
  type T = Entry list;
  
  fun zipwith _ [] _ = []
    | zipwith _ _ [] = []
    | zipwith f (x::xs) (y::ys) = (f x y) :: zipwith f xs ys;
  
  fun ordered_insert v [] 	= [v]
    | ordered_insert v (e::es) 	=
    	case Vk.kind_ord (v,e) of
    	  LESS 	=> e::(ordered_insert v es)
    	| _	=> v::e::es;
  
  
  fun get_kind_table graph = 
  		G.get_vertices graph
  		|> NTab.map_all' (fn vname => fn vdata =>
  				  VKind.get_kind (vname,vdata));
  
  (*Given a set of vertex names and a table mapping names to kinds, returns a list ordered 
    by vertex kinds*)		
  fun make_entry vnames ktable =
  		(NSet.list_of vnames
  		|> map (fn vname =>
  			NTab.get ktable vname)
  		|> fold ordered_insert 
  		) []; 
  
  		
  (*Defines an ordering of alist entries, simple since the entries themselves are ordered lists*)
  fun entry_ord ([] : Entry, [] : Entry) = EQUAL
    | entry_ord ([],_) 			 = LESS
    | entry_ord (_,[]) 			 = GREATER
    | entry_ord ((vk1::vk1s), (vk2::vk2s)) =
    	case VKind.kind_ord (vk1,vk2) of
    	  EQUAL => entry_ord (vk1s, vk2s)
    	| lt_or_gt => lt_or_gt;

		
  fun entry_eq (e1,e2) = (entry_ord (e1,e2) = EQUAL);
  
  (*defines an ordering on alists, used primarily for selecting the "biggest" vertex in a pattern. When there are
    a number of candidate choices for biggest vertex (all with the same kind), the "bigger" the alist, the faster 
    it will cover the graph*)
  fun alist_ord ([] : T, [] : T) = EQUAL
    | alist_ord ([],_)		  = LESS
    | alist_ord (_,[])		  = GREATER
    | alist_ord (e1::e1s, e2::e2s)=
    	case entry_ord (e1,e2) of
    	  EQUAL => alist_ord (e1s, e2s)
    	| lt_or_gt => lt_or_gt;
    	
  fun alist_eq (al1,al2) = (alist_ord (al1,al2) = EQUAL);
   				  
  (*Given a set of vertex names nameset, returns the vertices connected to this set (but not in it) 
    and the graph with the vertices in nameset deleted*)				  
  fun pull_next_layer (graph, vnames) =
  	let
  	val vnames2 = G.incident_vertices graph vnames
  	val graph2 = NSet.fold G.delete_vertex vnames graph
  	in (graph2,vnames2)
  	end;
  
  (*Given a  graph and a nameset, gets the next Alist entry, along with the modified
    graph and nameset for next layer*)	
  fun get_next_layer (graph,vnames) =
  	if not (NSet.is_empty vnames) then
  		let	
  		val (graph2,vnames2) = pull_next_layer (graph,vnames)
  		in
  		SOME (graph2,vnames2)
  		end
  	else NONE;
  		
  
  (*Given a graph and a vertex name, constructs the adjlist centered at that vertex
    consumes the entire graph, intended to process patterns, not target*) 
  fun build_adj_list graph vname =
  	let
  	val init_vset = NSet.single vname
  	val ktable = get_kind_table graph
  	fun build (graph,vnames) =
  		case get_next_layer (graph,vnames) of
  		  NONE 			=> []
  		| SOME (graph2,vnames2) => 
  			(make_entry vnames ktable:: build (graph2,vnames2))
  	in
  	build (graph,init_vset)
  	end;
  
  (*Returns the name of the biggest vertex kind whose adjacency list gives the "shortest/fastest", 
    coverage the graph (latter criteria used only in tie-break situations). Used for processing pattern graphs
    only. See comment for Alist_ord function. 
    assumes graph is non-empty*)	
  fun select_biggest graph =
  	let
  	val ((name,vkind),tab) = 
  		  get_kind_table graph
  		  |> NTab.pull_elem
  		  |> the
  	in
  	NTab.fold (fn (vname1,vk1) => fn (vname2,vk2) =>
  		      case VKind.kind_ord (vk1,vk2) of
  		        LESS => (vname2,vk2)
  		      | GREATER => (vname1,vk1)
  		      | EQUAL =>
  		      	(let
  		      	val adj1 = build_adj_list graph vname1
  		      	val adj2 = build_adj_list graph vname2
  		      	in
  		      	case alist_ord (adj1,adj2) of
  		      	  LESS  => (vname2,vk2)
  		      	| _     => (vname1,vk1)
  		      	end)) tab (name,vkind)
  	|> Library.fst
  	end;
  	
  fun get_adjacency graph = build_adj_list graph (select_biggest graph);
  	
  (*Checks compatibility of 2 Alist entries. Must be the same length, 
    and interior kinds in tgt_entry must be matched with either an identical type in pat_entry,
    or the boundary kind.
    Compatibility is used for looking up atrees. It means that the 
    Vkinds in this "layer" (i.e. entry of the Alist) of the tree can be matched to the
    Vkinds in the corresponding "layer" of the pattern graph
    IMPORTANT! this function assumes that graphs are in minimal form, i.e ALL boundary vertices 
    have arity 1*)
  fun compatible [] []    = true
    | compatible entry [] = true
    | compatible [] entry = false
    | compatible (tgt_entry as t::ts) (pat_entry as p::ps) =
    		case VKind.kind_ord (t,p) of
    		  EQUAL => compatible ts ps
    		(*LESS => both t and p are Iverts. Since the adjlists are sorted in
    		  descending order, p cannot be matched by any remaining kind in ts
    		  therefore not compatible*)
    		| LESS  => false
    		(*GREATER => t is an interior kind, and all remaining kinds in pat_entry 
    		  are either boundary or smaller than t. match t to the first boundary in pat_entry,
    		  if no such boundary exists match fails (not right away, but soon)*)
    		|GREATER=> 
    			let
    			fun remove_next_boundary []	     = []
    			  | remove_next_boundary (y::ys)=
    				if VKind.is_boundary y then ys 
    				else y::remove_next_boundary ys
    			in
    			compatible ts (remove_next_boundary pat_entry)
    			end;	

end;
