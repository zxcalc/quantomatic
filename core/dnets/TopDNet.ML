signature TOP_DNET =
sig
  type T
  type tree
  structure G : BANG_GRAPH
  structure CL : CONTOUR_LIST
  
  (* CONSTRUCTORS *)
  val empty : T
  val mk : G.T GraphName.NTab.T -> T
  val add_cl_to_dnet : T -> CL.T -> GraphName.name -> T
  val add_cl_list_to_dnet : T -> CL.T list -> GraphName.name list -> T

  (* PRUNING FUNCTIONS *)
  val prune : V.name -> G.T -> T -> T
  val graphs : T -> GraphName.name list

  (* FOLD FUNCTION *)
  val fold : (G.T -> 'a -> 'a) -> 'a -> T -> T

  (* GETTERS *)
  val is_node : tree -> bool
  val get_contour : tree -> CL.C.T
  val get_children : tree -> tree list
  val get_graph : tree -> GraphName.name

  (* COMPARISON FUNCTION *)
  val is_eq_graphs : GraphName.name list * GraphName.name list -> bool 

  (* PRINT FUNCTION*)
  val printout : tree list -> string
  
end


functor Top_DNet ( G: BANG_GRAPH ) : TOP_DNET =

struct
	
	structure G = G
	structure CL = Contour_List(G)

	datatype tree = Node of {contour : CL.C.T, children : tree list}
	              | Leaf of {graph : GraphName.name}
	type T = tree

    val empty = Node{contour=CL.C.empty, children=[]}

    (* This function checks if a tree node is a Node or a Leaf *)
    fun is_node (Node{contour=contour, children = children}) = true
      | is_node (Leaf{graph=graph}) = false

    (* Getters for a tree *)
    fun get_contour(Node{contour=contour, children = children}) = contour

    fun get_children(Node{contour=contour, children=children}) = children

    fun get_graph(Leaf{graph=graph}) = graph



    
    (* This function checks is a contour c is contained in a list of Node or Leaf *)
	  fun is_equiv_contained(c,[]) = false
  	  | is_equiv_contained(c,Node{contour=contour, children=children}::[]) = if CL.C.equiv(contour,c) then true else false
  	  | is_equiv_contained(c,Leaf{graph=graph}::[]) = false
  	  | is_equiv_contained(c,Node{contour=contour, children=children}::cl) = if CL.C.equiv(contour,c) then true else is_equiv_contained(c,cl)
  	  | is_equiv_contained(c,Leaf{graph=graph}::cl) = is_equiv_contained(c,cl)

    (* This function checks if a contour c is equivalent with the contour stored in a Node *)
    fun equiv(Node{contour=contour, children=children}, c) =
       if CL.C.equiv(contour,c) then true else false

    (* This function retrieves a node x from a contour list cl, given that x is contained in cl*)
    (* PRECONDITION: x is contained in cl *)
    fun get_equiv_node(x,c::[]) = c
      | get_equiv_node(x,c::cl) = 
          if is_node(c)
          then if equiv(c,x) then c else get_equiv_node(x,cl)
          else get_equiv_node(x,cl)
  
    (* This function retrieves a list of literals not equiv to x from a contour list cl, given that x is contained in cl*)
    (* PRECONDITION: x is contained in cl *)
    fun get_equiv_remaining(x,c::[]) = []
      | get_equiv_remaining(x,c::cl) = 
          if is_node(c)
          then if equiv(c,x) then cl else c::get_equiv_remaining(x,cl)
          else c::get_equiv_remaining(x,cl)



    (* This function add a contour list cl associated with the graph named gn to the tree *)
    fun add_cl_to_dnet (Node{contour=contour, children=children}) (c::[]) gn =
            if is_equiv_contained(c,children)
              then 
                let
                  val n = get_equiv_node(c,children)
                  val r = get_equiv_remaining(c,children)
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
            if is_equiv_contained(c,children)
              then
                let
                  val n = get_equiv_node(c,children)
                  val r = get_equiv_remaining(c,children)
                in  
                  Node{contour=contour, children=(add_cl_to_dnet n cs gn)::r}
                end
              else
                let
                  val n = Node{contour=c, children =[]}
                in
                  Node{contour=contour, children = (add_cl_to_dnet n cs gn)::children}
                end

  (* This function add a list of contour lists cls together with the list of graph named gns to the tree *)
  fun add_cl_list_to_dnet d (cl::[]) (gn::[]) = add_cl_to_dnet d cl gn
    | add_cl_list_to_dnet d (cl::cll) (gn::gns) = add_cl_list_to_dnet (add_cl_to_dnet d cl gn) cll gns

	(* This function builds a tree given a set of pattern graphs tab *)
	fun mk(tab) =
		let
		  val g_list = GraphName.NTab.values tab
		  val g_names = GraphName.NTab.keys tab
		  val cl_list =  map CL.mk g_list
		  val e = empty
		in
		  add_cl_list_to_dnet e cl_list g_names
		end  
				

  (* HELPER FUNCTIONS to prune a tree *)
  (* This functions removes from a list of nodes ts all the nodes which are not leaves *)
  fun remove_not_leaf([]) = []
    | remove_not_leaf(t::[]) = if is_node(t) then [] else t::[]
    | remove_not_leaf(t::ts) = if is_node(t) then remove_not_leaf(ts) else t::remove_not_leaf(ts)

  (* This functions filters the children of a node according to the strong compatibility with a contour c *)
  fun strong_filter_children(c,[]) = []
    | strong_filter_children(c,t::[]) =
      if (is_node(t))
      then if CL.C.check_strong_compatibility(c,get_contour(t))
           then Node{contour=get_contour(t), children=get_children(t)}::[]
           else []
      else t::[]
    | strong_filter_children(c,t::ts) =
      if (is_node(t))
      then if CL.C.check_strong_compatibility(c,get_contour(t))
           then Node{contour=get_contour(t), children=get_children(t)}::strong_filter_children(c,ts)
           else strong_filter_children(c,ts)
      else t::strong_filter_children(c,ts)

  (* This functions filters the children of a node according to the weak compatibility with a contour c *)
  fun weak_filter_children(c,[]) = []
    | weak_filter_children(c,t::[]) =
      if (is_node(t))
      then if CL.C.check_weak_compatibility(c,get_contour(t))
           then Node{contour=get_contour(t), children=get_children(t)}::[]
           else []
      else t::[]
    | weak_filter_children(c,t::ts) =
      if (is_node(t))
      then if CL.C.check_weak_compatibility(c,get_contour(t))
           then Node{contour=get_contour(t), children=get_children(t)}::weak_filter_children(c,ts)
           else weak_filter_children(c,ts)
      else t::weak_filter_children(c,ts)  

  (* This function recursively prunes the tree t using strong compatibility*)
  fun strong_prune_trees([],_) = []
    | strong_prune_trees(t::[],c::[]) =
        if (is_node(t))
        then
          let
            val children = get_children(t)
            val new_children = strong_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=new_children} :: []
          end
        else t::[]
    | strong_prune_trees(t::[],c::cs) = 
        if (is_node(t))
        then 
          let
            val children = get_children(t)
            val new_children = strong_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=strong_prune_trees(new_children,cs)} :: []
          end
        else t::[]
    | strong_prune_trees(t::ts,c::[]) =
        if (is_node(t))
        then
          let 
            val children = get_children(t)
            val new_children = strong_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=new_children} :: strong_prune_trees(ts,c::[])
          end
        else t::strong_prune_trees(ts,c::[])
    | strong_prune_trees(t::ts,c::cs) =
        if (is_node(t))
        then 
          let
            val children = get_children(t)
            val new_children = strong_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=strong_prune_trees(new_children,cs)} :: strong_prune_trees(ts,c::cs)
          end
        else t::strong_prune_trees(ts,c::cs)

  (* This function recursively prunes the tree t using weak compatibility*)
  fun weak_prune_trees([],_) = []
    | weak_prune_trees(t::[],c::[]) =
        if (is_node(t))
        then
          let
            val children = get_children(t)
            val new_children = weak_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=new_children} :: []
          end
        else t::[]
    | weak_prune_trees(t::[],c::cs) = 
        if (is_node(t))
        then 
          let
            val children = get_children(t)
            val new_children = weak_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=weak_prune_trees(new_children,cs)} :: []
          end
        else t::[]
    | weak_prune_trees(t::ts,c::[]) =
        if (is_node(t))
        then
          let 
            val children = get_children(t)
            val new_children = weak_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=new_children} :: weak_prune_trees(ts,c::[])
          end
        else t::weak_prune_trees(ts,c::[])
    | weak_prune_trees(t::ts,c::cs) =
        if (is_node(t))
        then 
          let
            val children = get_children(t)
            val new_children = weak_filter_children(c,children)
          in
            Node{contour=get_contour(t),children=weak_prune_trees(new_children,cs)} :: weak_prune_trees(ts,c::cs)
          end
        else t::weak_prune_trees(ts,c::cs)

  (* This function perform the first step of the pruning of the tree t at the root *)
  fun prune_tree(t,[]) = t
    | prune_tree(t,c::[]) =
      if (is_node(t))
      then 
        let
          val children = get_children(t)
          val new_children = strong_filter_children(c,children)
        in
          Node{contour=get_contour(t),children=new_children}
        end  
      else t
    | prune_tree(t,c::cs) =
      if (is_node(t))
      then
        let
          val children = get_children(t)
          val new_children = strong_filter_children(c,children)
        in
          Node{contour=get_contour(t),children=strong_prune_trees(new_children,cs)}
        end
      else t      
		
	(* This function prunes a tree given a tree t, a graph g and a starting node-vertex v to build the contour list of g*)
	fun prune v g t =
    let
      val target_contour_list = CL.mk_from g v
    in
      prune_tree(t,target_contour_list)
    end



  (* This function traverses the tree t to get to the leaves *)
  fun explore_branches([]) = []
    | explore_branches(t::[]) =
      if (is_node(t)) then explore_branches(get_children(t))
      else get_graph(t) :: []
    | explore_branches(t::ts) =
      if (is_node(t)) then explore_branches(get_children(t)) @ explore_branches(ts)
      else get_graph(t) :: explore_branches(ts)

  (* This function returns the list of the names of all the graphs encoded in the tree t *)
  fun graphs t =
    if (is_node(t)) then explore_branches(get_children(t))
    else get_graph(t)::[]



  (* This function checks if two graphs g1 and g2 are equal *)
  (* TRUE iff the graphs have the same name *)
  fun graphname_eq(g1,g2) = if (GraphName.string_of_name(g1) = GraphName.string_of_name(g2)) then true else false
 
  (* This function removes the graph g from the list of graphs gl, given that g is contained in gl*)
  (* PRECONDITION: g is contained in gl*)
  fun remove_eq_graph(g,gl::[]) = []
    | remove_eq_graph(g,gl::gls) = if graphname_eq(g,gl) then gls else gl::remove_eq_graph(g,gls) 

  (* This function checks if a graph g is contained in the graph list gl*)
  fun find_eq_graph(g,[]) = false
    | find_eq_graph(g,gl::[]) = if graphname_eq(g,gl) then true else false
    | find_eq_graph(g,gl::gls) = if graphname_eq(g,gl) then true else find_eq_graph(g,gls)

  (* This function checks if two graph lists g1s and g2s are equal *)
  fun is_eq_graphs([],[]) = true
    | is_eq_graphs([],g2::[]) = false
    | is_eq_graphs(g1::[],[]) = false
    | is_eq_graphs([],g2::g2s) = false
    | is_eq_graphs(g1::g1s,[]) = false
    | is_eq_graphs(g1::[],g2::[]) = if graphname_eq(g1,g2) then true else false
    | is_eq_graphs(g1::g1s,g2::[]) = false 
    | is_eq_graphs(g1::[],g2::g2s) = false 
    | is_eq_graphs(g1::g1s,g2::g2s) = 
      if graphname_eq(g1,g2) then is_eq_graphs (g1s,g2s)
      else if find_eq_graph(g1,g2s) then is_eq_graphs(g1s,g2::remove_eq_graph(g1,g2s)) else false



	(* Fold function *)
	fun fold f a b = b



  (* This function prints the tree t*)
  fun printout ([]) = ""
    | printout (t::[]) = 
      if (is_node(t)) then CL.C.printout(get_contour(t)) ^ " " ^ printout(get_children(t))
      else GraphName.string_of_name(get_graph(t))
    | printout (t::ts) = 
      if (is_node(t)) then CL.C.printout(get_contour(t)) ^ " " ^ printout(ts) ^ " " ^ printout(get_children(t))
      else GraphName.string_of_name(get_graph(t)) ^ " " ^ printout(ts)

end

(*
val dnet = mk pattern_graphs;

val dnet' = prune target_graph dnet;

val matches = fold (fn r => fn lst => (match r g)@lst) dnet' [];*)