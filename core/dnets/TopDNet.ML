signature TOP_DNET =
sig
  type T
  type tree
  structure G : BANG_GRAPH
  structure CL : CONTOUR_LIST
  sharing G.SharingOGraph = CL.G.SharingOGraph
  
  (* CONSTRUCTORS *)
  val empty : T
  val mk : G.T GraphName.NTab.T -> T
  val add_cl_to_dnet : T -> CL.T -> GraphName.name -> T
  val add_cl_list_to_dnet : T -> CL.T list -> GraphName.name list -> T
  val add_graph : (GraphName.name * G.T) -> T -> T

  (* PRUNING FUNCTIONS *)
  val extended_pruning : T -> G.T -> (V.name * GraphName.name list) list
  val standard_pruning : T -> G.T -> (V.name * GraphName.name list) list
  val extended_prune : V.name -> G.T -> T -> T
  val standard_prune : V.name -> G.T -> T -> T
  val graphs : T -> GraphName.name list
  val get_match_candidates : T -> G.T -> GraphName.NSet.T

  (* FOLD FUNCTION *)
  val fold : (('a * 'b) -> G.T GraphName.NTab.T -> G.T GraphName.NTab.T) -> 'a list -> 'b list -> G.T GraphName.NTab.T -> G.T GraphName.NTab.T

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
  structure C = CL.C

	datatype tree = Node of {contour : C.T, children : tree list}
	              | Leaf of {graph : GraphName.name}
	type T = tree

    val empty = Node{contour=C.empty, children=[]}

    (* This function checks if a tree node is a Node or a Leaf *)
    fun is_node (Node{contour=contour, children = children}) = true
      | is_node (Leaf{graph=graph}) = false

    (* Getters for a tree *)
    fun get_contour(Node{contour=contour, children = children}) = contour
      | get_contour _ = raise ERROR "Not a contour"

    fun get_children(Node{contour=contour, children=children}) = children
      | get_children _ = raise ERROR "Not a contour"

    fun get_graph(Leaf{graph=graph}) = graph
      | get_graph _ = raise ERROR "Not a graph"



    
    (* This function checks is a contour c is contained in a list of Node or Leaf *)
	  fun is_equiv_contained(c,[]) = false
  	  | is_equiv_contained(c,Node{contour=contour, children=children}::[]) = if C.equiv(contour,c) then true else false
  	  | is_equiv_contained(c,Leaf{graph=graph}::[]) = false
  	  | is_equiv_contained(c,Node{contour=contour, children=children}::cl) = if C.equiv(contour,c) then true else is_equiv_contained(c,cl)
  	  | is_equiv_contained(c,Leaf{graph=graph}::cl) = is_equiv_contained(c,cl)

    (* This function checks if a contour c is equivalent with the contour stored in a Node *)
    fun equiv(Node{contour=contour, children=children}, c) =
       if C.equiv(contour,c) then true else false
      | equiv _ = false

    (* This function retrieves a node x from a contour list cl, given that x is contained in cl*)
    (* PRECONDITION: x is contained in cl *)
    fun get_equiv_node(_,c::[]) = c
      | get_equiv_node(x,c::cl) = 
          if is_node(c)
          then if equiv(c,x) then c else get_equiv_node(x,cl)
          else get_equiv_node(x,cl)
      | get_equiv_node(_,[]) = raise ERROR "empty contour list"
  
    (* This function retrieves a list of literals not equiv to x from a contour list cl, given that x is contained in cl*)
    (* PRECONDITION: x is contained in cl *)
    fun get_equiv_remaining(x,c::[]) = []
      | get_equiv_remaining(x,c::cl) = 
          if is_node(c)
          then if equiv(c,x) then cl else c::get_equiv_remaining(x,cl)
          else c::get_equiv_remaining(x,cl)
      | get_equiv_remaining(_,[]) = raise ERROR "empty contour list"



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
      | add_cl_to_dnet _ [] _ = raise ERROR "Empty contour list"
      | add_cl_to_dnet _ _ _ = raise ERROR "Not a contour"

  (* This function add a list of contour lists cls together with the list of graph named gns to the tree *)
  fun add_cl_list_to_dnet d (cl::[]) (gn::[]) = add_cl_to_dnet d cl gn
    | add_cl_list_to_dnet d (cl::cll) (gn::gns) = add_cl_list_to_dnet (add_cl_to_dnet d cl gn) cll gns
    | add_cl_list_to_dnet d _ _ = raise ERROR "Contour and graph list lengths differ"

  fun add_graph (n,g) dnet = add_cl_to_dnet dnet (CL.mk g) n

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
				

  
  (* This function recursively prunes the tree t using weak compatibility*)
  fun weak_prune_trees([],[]) = []
    | weak_prune_trees(t::[],[]) = t::[]
    | weak_prune_trees([],c::[]) = []    
    | weak_prune_trees(t::[],c::[]) =
        if (is_node(t)) then
          if (C.check_weak_compatibility(c,get_contour(t))) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),[])} :: []
          else []
        else t :: []        
    | weak_prune_trees([],c::cs) = []
    | weak_prune_trees(t::ts,[]) = 
        if (is_node(t)) then weak_prune_trees(ts,[])
        else t :: weak_prune_trees(ts,[])
    | weak_prune_trees(t::[],c::cs) = 
        if (is_node(t)) then
          if (C.check_weak_compatibility(c,get_contour(t))) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),cs)} :: []
          else []
        else t :: []
    | weak_prune_trees(t::ts,c::[]) =
        if (is_node(t)) then
          if (C.check_weak_compatibility(c,get_contour(t))) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),[])} :: weak_prune_trees(ts,c::[])
          else weak_prune_trees(ts,c::[])
        else t :: weak_prune_trees(ts,c::[])
    | weak_prune_trees(t::ts,c::cs) =
        if (is_node(t)) then
          if (C.check_weak_compatibility(c,get_contour(t))) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),cs)} :: weak_prune_trees(ts,c::cs)
          else weak_prune_trees(ts,c::cs)
        else t :: weak_prune_trees(ts,c::cs)


  (* This function recursively prunes the tree t using strong compatibility*)
  fun extended_strong_prune_trees([],[]) = []
    | extended_strong_prune_trees(t::[],[]) = t::[]
    | extended_strong_prune_trees([],c::[]) = []    
    | extended_strong_prune_trees(t::[],c::[]) =
        if (is_node(t))
        then
          let
            val is_weak = C.contains_boundary(get_contour(t))
            val is_saved = if (is_weak) then C.check_weak_compatibility(c,get_contour(t)) else C.check_strong_compatibility(c,get_contour(t))
          in
            if (is_saved) then
              if (is_weak) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),[])} :: []
              else Node{contour=get_contour(t),children=extended_strong_prune_trees(get_children(t),[])} :: []
            else []
          end
        else
          t :: []
    | extended_strong_prune_trees([],c::cs) = []
    | extended_strong_prune_trees(t::ts,[]) = 
        if (is_node(t)) then extended_strong_prune_trees(ts,[])
        else t :: extended_strong_prune_trees(ts,[])
    | extended_strong_prune_trees(t::[],c::cs) = 
        if (is_node(t))
        then
          let
            val is_weak = C.contains_boundary(get_contour(t))
            val is_saved = if (is_weak) then C.check_weak_compatibility(c,get_contour(t)) else C.check_strong_compatibility(c,get_contour(t))
          in
            if (is_saved) then
              if (is_weak) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),cs)} :: []
              else Node{contour=get_contour(t),children=extended_strong_prune_trees(get_children(t),cs)} :: []
            else []
          end
        else
          t :: []
    | extended_strong_prune_trees(t::ts,c::[]) =
        if (is_node(t))
        then
          let
            val is_weak = C.contains_boundary(get_contour(t))
            val is_saved = if (is_weak) then C.check_weak_compatibility(c,get_contour(t)) else C.check_strong_compatibility(c,get_contour(t))
          in
            if (is_saved) then
              if (is_weak) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),[])} :: extended_strong_prune_trees(ts,c::[])
              else Node{contour=get_contour(t),children=extended_strong_prune_trees(get_children(t),[])} :: extended_strong_prune_trees(ts,c::[])
            else extended_strong_prune_trees(ts,c::[])
          end
        else
          t :: extended_strong_prune_trees(ts,c::[])
    | extended_strong_prune_trees(t::ts,c::cs) =
        if (is_node(t))
        then
          let
            val is_weak = C.contains_boundary(get_contour(t))
            val is_saved = if (is_weak) then C.check_weak_compatibility(c,get_contour(t)) else C.check_strong_compatibility(c,get_contour(t))
          in
            if (is_saved) then
              if (is_weak) then Node{contour=get_contour(t),children=weak_prune_trees(get_children(t),cs)} :: extended_strong_prune_trees(ts,c::cs)
              else Node{contour=get_contour(t),children=extended_strong_prune_trees(get_children(t),cs)} :: extended_strong_prune_trees(ts,c::cs)
            else extended_strong_prune_trees(ts,c::cs)
          end
        else
          t :: extended_strong_prune_trees(ts,c::cs)


  (* This function perform the first step of the extended pruning of the tree t at the root *)
  fun extended_prune_tree(t,[]) = t
    | extended_prune_tree(t,c::[]) =
      if (is_node(t))
      then 
        let
          val children = get_children(t)
        in
          Node{contour=get_contour(t),children = extended_strong_prune_trees(children,c::[])}
        end  
      else t
    | extended_prune_tree(t,c::cs) =
      if (is_node(t))
      then
        let
          val children = get_children(t)
        in
          Node{contour = get_contour(t), children = extended_strong_prune_trees(children,c::cs)}
        end
      else t 

  (* This function perform the first step of the standard pruning of the tree t at the root *)
  fun standard_prune_tree(t,[]) = t
    | standard_prune_tree(t,c::[]) =
      if (is_node(t))
      then 
        let
          val children = get_children(t)
        in
          Node{contour=get_contour(t),children = weak_prune_trees(children,c::[])}
        end  
      else t
    | standard_prune_tree(t,c::cs) =
      if (is_node(t))
      then
        let
          val children = get_children(t)
        in
          Node{contour = get_contour(t), children = weak_prune_trees(children,c::cs)}
        end
      else t      
		
	(* This function use the extended algorithm to prune a tree given a tree t, a graph g and a starting node-vertex v to build the contour list of g*)
	fun extended_prune v g t =
    let
      val target_contour_list = CL.mk_from g v
    in
      extended_prune_tree(t,target_contour_list)
    end

  (* This function use the standard algorithm to prune a tree given a tree t, a graph g and a starting node-vertex v to build the contour list of g*)
  fun standard_prune v g t =
    let
      val target_contour_list = CL.mk_from g v
    in
      standard_prune_tree(t,target_contour_list)
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

  (* This function returns the list of the names of all the graphs encoded in a list of discrimination trees t::ts *)
  fun collect_graphs [] = []
    | collect_graphs (t::[]) = (graphs t) :: []
    | collect_graphs (t::ts) = (graphs t) :: (collect_graphs ts)

  (* This function receives a discrimination tree and executes the extended pruning algorithm using all possible contour list built from a target and a starting node-vertex n *)
  fun do_extended_pruning tree target [] = []
    | do_extended_pruning tree target (n::[]) = (extended_prune n target tree) :: []
    | do_extended_pruning tree target (n::ns) = (extended_prune n target tree) :: (do_extended_pruning tree target ns)

  (* This function receives a discrimination tree and executes the standard pruning algorithm using all possible contour list built from a target and a starting node-vertex n *)
  fun do_standard_pruning tree target [] = []
    | do_standard_pruning tree target (n::[]) = (standard_prune n target tree) :: []
    | do_standard_pruning tree target (n::ns) = (standard_prune n target tree) :: (do_standard_pruning tree target ns)

  (* This function recevives a discrimination tree and a target graph and starts the extended pruning algorithm *)
  fun extended_pruning tree target =
    let
      val nodelist = G.get_unbboxed target
      val nodelist = V.NSet.list_of nodelist
      val treelist = do_extended_pruning tree target (rev nodelist)
      val graphslist = collect_graphs (rev treelist)
    in
      nodelist ~~ graphslist
    end

  (* This function recevives a discrimination tree and a target graph and starts the standard pruning algorithm *)
  fun standard_pruning tree target =
    let
      val nodelist = G.get_unbboxed target
      val nodelist = V.NSet.list_of nodelist
      val treelist = do_standard_pruning tree target (rev nodelist)
      val graphslist = collect_graphs (rev treelist)
    in
      nodelist ~~ graphslist
    end

  fun get_match_candidates dnet target =
    fold GraphName.NSet.add (flat (map snd (extended_pruning dnet target))) GraphName.NSet.empty

  (* This function checks if two graphs g1 and g2 are equal *)
  (* TRUE iff the graphs have the same name *)
  fun graphname_eq(g1,g2) = if (GraphName.string_of_name(g1) = GraphName.string_of_name(g2)) then true else false
 
  (* This function removes the graph g from the list of graphs gl, given that g is contained in gl*)
  (* PRECONDITION: g is contained in gl*)
  fun remove_eq_graph(g,gl::[]) = []
    | remove_eq_graph(g,gl::gls) = if graphname_eq(g,gl) then gls else gl::remove_eq_graph(g,gls) 
    | remove_eq_graph(_,[]) = raise ERROR "Empty graph list"

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
	fun fold _ [] [] tab = tab
    | fold f (gn::gns) (g::gs) tab = f (gn,g) (fold f gns gs tab)
    | fold _ _ _ _ = raise ListPair.UnequalLengths;



  (* This function prints the tree t*)
  fun printout ([]) = ""
    | printout (t::[]) = 
      if (is_node(t)) then C.printout(get_contour(t)) ^ " " ^ printout(get_children(t))
      else GraphName.string_of_name(get_graph(t))
    | printout (t::ts) = 
      if (is_node(t)) then C.printout(get_contour(t)) ^ " " ^ printout(ts) ^ " " ^ printout(get_children(t))
      else GraphName.string_of_name(get_graph(t)) ^ " " ^ printout(ts)

end
