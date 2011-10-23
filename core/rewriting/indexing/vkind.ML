
signature VKIND
= sig
  structure V : RG_VERTEX
  structure NSet : BASIC_NAME_SET
  type T
  
  (*get kind of vertex passed, format (name,(data,(in,out))) comes from	*)
  (* result of get_vertex_list						*)
  val get_kind : (V.name * (V.data *(NSet.T * NSet.T))) 
  					-> T
  (*give ordering on vertex kinds*)
  val kind_ord : T * T -> order
  val kind_eq : T * T -> bool
  
  val is_boundary : T -> bool
  
end;


(* type of vertex kinds: X/Z/H and arity, also includes boundary type*)
structure VKind : VKIND
= struct
  structure V = Vertex;
  structure NSet = E.NTab.NSet;
  
  
  type data = V.IData.data;
  datatype T = Kind of (data * int)	(*quantum node type and arity*)
  	     | Bound;			(*boundary vertex*)
  	 
  
	 
  fun get_kind (name,(idata,(inedges,outedges))) =
  	let 
  	   val arity = NSet.cardinality inedges + NSet.cardinality outedges
  	in case idata of
  	   	 V.IVert qdata 	=> Kind (qdata,arity)
  	   	|V.BVert	=> Bound
  	end;
  
  (*arities are compared first*)
  fun kind_ord (Bound,Bound) 	= EQUAL
    | kind_ord (Bound,_) 		= LESS
    | kind_ord (_, Bound)		= GREATER
    | kind_ord (Kind (d1,a1),Kind (d2,a2)) =
  	case Int.compare (a1,a2) of
    	  EQUAL => V.IData.data_ord (d1,d2) 
    	| LESS	=> LESS
    	| GREATER=>GREATER;
    	
  fun kind_eq (k1,k2) = (kind_ord (k1,k2) = EQUAL);
  
  fun is_boundary k = kind_eq (k,Bound)

end;
