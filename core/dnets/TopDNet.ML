signature TOP_DNET =
sig
  type T
  type tree
  structure G : BANG_GRAPH
  structure CL : CONTOUR_LIST
  
  val mk : G.T GraphName.NTab.T -> T
  val prune : G.T -> T -> T
  val fold : (G.T -> 'a -> 'a) -> 'a -> T -> T

  val empty : T
  val add_cl_to_dnet : T -> CL.T -> GraphName.name -> T
  val add_cl_list_to_dnet : T -> CL.T list -> GraphName.name list -> T

  val fprintf : tree list -> string

  val is_node : tree -> bool
  val get_contour : tree -> CL.C.T
  val get_children : tree -> tree list
  val get_graph : tree -> GraphName.name
end


functor Top_DNet ( G: BANG_GRAPH ) : TOP_DNET =

struct
	
	structure G = G
	structure CL = Contour_List(G)

	datatype tree = Node of {contour : CL.C.T, children : tree list}
	              | Leaf of {graph : GraphName.name}
	type T = tree

    val empty = Node{contour=CL.C.empty, children=[]}

     (* Helper functions to get the type of node *)
    fun is_node (Node{contour=contour, children = children}) = true
      | is_node (Leaf{graph=graph}) = false

    (* Helper functions to retrieve values in the nodes *)
    fun get_contour(Node{contour=contour, children = children}) = contour

    fun get_children(Node{contour=contour, children=children}) = children

    fun get_graph(Leaf{graph=graph}) = graph

    (* Helper function to compare contours *)
    fun equal(Node{contour=contour, children=children}, c) =
       if CL.C.equiv(contour,c) then true else false

    (* Helper function to check if a contour is inside the list of contours associated with a node *)
	  fun is_contained(c,[]) = false
  	  | is_contained(c,Node{contour=contour, children=children}::[]) = if CL.C.equiv(contour,c) then true else false
  	  | is_contained(c,Leaf{graph=graph}::[]) = false
  	  | is_contained(c,Node{contour=contour, children=children}::cl) = if CL.C.equiv(contour,c) then true else is_contained(c,cl)
  	  | is_contained(c,Leaf{graph=graph}::cl) = is_contained(c,cl)

    (* Helper function to retrieve a specific node from a list*)
    fun get_node(x,c::[]) = c
      | get_node(x,c::cl) = 
          if is_node(c)
          then if equal(c,x) then c else get_node(x,cl)
          else get_node(x,cl)
  
    fun get_remaining(x,c::[]) = []
      | get_remaining(x,c::cl) = 
          if is_node(c)
          then if equal(c,x) then cl else c::get_remaining(x,cl)
          else c::get_remaining(x,cl)


    fun add_cl_to_dnet (Node{contour=contour, children=children}) (c::[]) gn =
            if is_contained(c,children)
              then 
                let
                  val n = get_node(c,children)
                  val r = get_remaining(c,children)
                  val new_leaf = Leaf{graph=gn}
                  val new_node = Node{contour=get_contour(n),children=new_leaf::get_children(n)}
                in
                  Node{contour=contour, children=new_node::r}
                end
              else
                let
               	  val new_leaf = Leaf{graph=gn}
                  val new_node = Node{contour=c,children=new_leaf::[]}
                in
                  Node{contour=contour, children=new_node::children}
                end
      | add_cl_to_dnet (Node{contour=contour, children=children}) (c::cs) gn =
            if is_contained(c,children)
              then
                let
                  val n = get_node(c,children)
                  val r = get_remaining(c,children)
                in  
                  Node{contour=contour, children=(add_cl_to_dnet n cs gn)::r}
                end
              else
                let
                  val n = Node{contour=c, children =[]}
                in
                  Node{contour=contour, children = (add_cl_to_dnet n cs gn)::children}
                end


    fun add_cl_list_to_dnet d (cl::[]) (gn::[]) = add_cl_to_dnet d cl gn
      | add_cl_list_to_dnet d (cl::cll) (gn::gns) = add_cl_list_to_dnet (add_cl_to_dnet d cl gn) cll gns

	(* build a tree given a set of pattern graphs *)
	fun mk(tab) =

		let
		  val g_list = GraphName.NTab.values tab
		  val g_names = GraphName.NTab.keys tab
		  val cl_list =  map CL.mk g_list
		  val e = empty
		in
		  add_cl_list_to_dnet e cl_list g_names
		end  
				
		
	(* prune a tree given the tree and the graph *)
	fun prune g d = d

	(* compute the list of all the graphs not pruned *)
	fun fold f a b = b



  fun fprintf ([]) = ""
    | fprintf (t::[]) = 
      if (is_node(t)) then CL.C.fprintf(get_contour(t)) ^ " " ^ fprintf(get_children(t))
      else GraphName.string_of_name(get_graph(t))
    | fprintf (t::ts) = 
      if (is_node(t)) then CL.C.fprintf(get_contour(t)) ^ " " ^ fprintf(ts) ^ " " ^ fprintf(get_children(t))
      else GraphName.string_of_name(get_graph(t)) ^ " " ^ fprintf(ts)

end

(*
val dnet = mk pattern_graphs;

val dnet' = prune target_graph dnet;

val matches = fold (fn r => fn lst => (match r g)@lst) dnet' [];*)