

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
  
  
  (*A histogram of a graph is a list of pairs of vertex kinds and the number of 
    times they appear. It's ordered by vertex kinds, and all functions below
    assume this. The boundary vertex kind is also assumed absent.*)
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
  	|> map (fn vdata => VKind.get_kind vdata)
  	(*filter out boundaries*)
  	|> filter (fn vkind => not (VKind.is_boundary vkind))
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
