
signature VKIND
= sig
  structure V : RG_VERTEX
  structure NSet : BASIC_NAME_SET
  type T
  
  (*get kind of vertex passed, format (name,(data,(in,out))) comes from	*)
  (* result of get_vertex_list						*)
  val get_kind : (V.name * (V.data *(NSet.T * NSet.T))) 
  					-> T option
  (*give ordering on vertex kinds*)
  val kind_ord : T * T -> order
  val kind_eq : T * T -> bool
  
end;

(* type of vertex kinds: X/Z/H and arity*)
structure VKind : VKIND
= struct
  structure V = Vertex;
  structure NSet = Edge.NTab.NSet;
  
  
  type data = V.IData.data;
  type T = data * int; 	(*quantum node type and arity*)
  
  
  fun get_kind (name,(idata,(inedges,outedges))) =
  	let 
  	   val arity = NSet.cardinality inedges + NSet.cardinality outedges
  	in case idata of
  	   	 V.IVert qdata 	=> SOME (qdata,arity)
  	   	|V.BVert	=> NONE
  	end;
  
  (*arities are compared first*)
  fun kind_ord ((kind1 as (d1,a1)),(kind2 as (d2,a2))) =
  	case Int.compare (a1,a2) of
    	  EQUAL => V.IData.data_ord (d1,d2) 
    	| LESS	=> LESS
    	| GREATER=>GREATER;
    	
  fun kind_eq (k1,k2) = (kind_ord (k1,k2) = EQUAL);

end;

signature HISTOGRAM
= sig
  structure G : GRAPH
  structure V : RG_VERTEX
  
  type Entry
  type T
  
  val insert_into_hist : VKind.T -> T ->  T
  val create_hist : G.T ->  T
  val entry_ord : Entry -> Entry -> order
  val max_entry : Entry -> Entry -> Entry
end;

structure Histogram : HISTOGRAM
= struct
  structure G = RGGraph;
  structure V = G.RGVertex;
  
  type Entry = (VKind.T * int)
  type T = Entry list;
  
  fun insert_into_hist kind []			= [(kind,1)]
    | insert_into_hist kind1 ((kind2,n)::ks)	=
    		case VKind.kind_ord (kind1,kind2) of
    		  EQUAL 	=> (kind2,n+1)::ks
    		| LESS  	=> (kind2,n):: insert_into_hist kind1 ks
    		| GREATER 	=> (kind1,1)::(kind2,n)::ks;
  
  fun create_hist graph =
  	(
  	G.get_vertex_list graph
  	|> maps (fn vdata => case VKind.get_kind vdata of
  				 SOME kind => [kind]
  				|NONE	   => [])
  	|> fold insert_into_hist
  	) [];
  
  (*order goes : Vertex kind then number of them*)	
  fun entry_ord (kind1,n1) (kind2,n2) =
  	case VKind.kind_ord (kind1,kind2) of
  	  EQUAL => Int.compare (n1,n2)
  	| LESS  => LESS
  	| GREATER => GREATER;
  
  fun max_entry e1 e2 =
  	case entry_ord e1 e2 of
  	  EQUAL => e1
  	| LESS  => e2
  	|GREATER=> e1;
  	
end;
