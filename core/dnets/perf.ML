
  open Test_MkG
  open Test_MatchUtil

  
  structure LIB = TDNet_Library
  structure TD = Top_DNet(G)
  structure CL = TD.CL
  structure C = CL.C
  structure L = C.L

  val timestamp = Time.toString (Time.now())
  val out = TextIO.openOut ("output"^timestamp^".txt")

  fun printout msg =
    TextIO.outputSubstr(out, Substring.substring (msg,0,size msg))

  
   




  fun generate_random_nodes n =
    if (n = 1) then V.mk ("v" ^ string_of_int(n)) :: []
    else V.mk ("v" ^ string_of_int(n)) :: generate_random_nodes (n-1)

  fun add_nodes g [] k = g
    | add_nodes g (n::[]) k = g |> G.add_named_vertex n (LIB.one_of k)
    | add_nodes g (n::ns) k = add_nodes (g |> G.add_named_vertex n (LIB.one_of k)) ns k

  fun add_edges g n e =
    if (e=1) then
      let
        val node1 = LIB.one_of n
        val n_minus = LIB.rm_element (V.name_eq) node1 n
        val node2 = LIB.one_of n_minus
        val edge = E.mk ("e" ^ string_of_int(e))
      in
        g |> G.add_named_edge edge dir_edge node1 node2
      end
    else
      let
        val node1 = LIB.one_of n
        val n_minus = LIB.rm_element (V.name_eq) node1 n
        val node2 = LIB.one_of n_minus
        val edge = E.mk ("e" ^ string_of_int(e))
      in
        add_edges (g |> G.add_named_edge edge dir_edge node1 node2) n (e-1)
      end  


  fun get_adj g n = V.NSet.list_of (G.adj_vnames g n)

  fun get_connected_nodes g [] num_nodes bb_nodes = bb_nodes
    | get_connected_nodes g (n::[]) num_nodes bb_nodes  = 
      if (num_nodes=1) then n::bb_nodes
      else
        let
          val nodelist = LIB.maps3 get_adj g (n::bb_nodes)
          val nodelist = LIB.rm_duplicates (V.name_eq) nodelist
          val nodelist = LIB.sub_x_y (V.name_eq) nodelist bb_nodes
        in
          get_connected_nodes g nodelist (num_nodes-1) (n::bb_nodes)
        end
   | get_connected_nodes g (n::ns) num_nodes bb_nodes =
      if (num_nodes=1) then (LIB.one_of (n::ns)) :: bb_nodes
      else
        let
          val node = LIB.one_of(n::ns)
          val nodelist = LIB.maps3 get_adj g (n::bb_nodes)
          val nodelist = LIB.rm_duplicates V.name_eq nodelist
          val nodelist = LIB.sub_x_y (V.name_eq) nodelist bb_nodes
        in
          get_connected_nodes g nodelist (num_nodes-1) (node::bb_nodes)
        end 

  fun add_bbox g bb [] = g
    | add_bbox g bb (n::[]) = g |> G.doadd_to_bbox bb (V.NSet.single n)
    | add_bbox g bb (n::ns) = add_bbox (g |> G.doadd_to_bbox bb (V.NSet.single n)) bb ns

  fun add_bboxes g n b npbb =
    if (b=1) then
      let
        val num_nodes =  LIB.random_range 1 npbb
        val bb_nodes : V.name list = []
        val nodes = get_connected_nodes g n num_nodes bb_nodes
        val (bb,g) = g |> G.add_bbox
      in
        add_bbox g bb nodes       
      end
    else
      let
        val num_nodes =  LIB.random_range 1 npbb
        val bb_nodes : V.name list = []
        val nodes = get_connected_nodes g n num_nodes bb_nodes
        val n = LIB.sub_x_y (V.name_eq) n nodes
        val (bb,g) = g |> G.add_bbox
      in
        add_bboxes (add_bbox g bb nodes) n (b-1) npbb
      end 

  fun get_boundaries g [] = []
    | get_boundaries g (n::[]) = 
        let
          val input_arity = Arity.get_in (G.get_arity g n)
          val output_arity = Arity.get_out (G.get_arity g n)
        in
          if (input_arity = 0) orelse (output_arity = 0) then n::[] else []
        end
    | get_boundaries g (n::ns) =
        let
          val input_arity = Arity.get_in (G.get_arity g n)
          val output_arity = Arity.get_out (G.get_arity g n)
        in
          if (input_arity = 0) orelse (output_arity = 0) then n::(get_boundaries g ns) else (get_boundaries g ns)
        end

  fun boundarize g k [] = g
    | boundarize g k (n::[]) = 
      let
        val b = L.boundary
        val dice = LIB.random_range 1 k
      in
        if (dice=1) then G.set_vertex_data b n g else g
      end
    | boundarize g k (n::ns) = 
      let
        val b = L.boundary
        val dice = LIB.random_range 1 k
      in
        if (dice=1) then boundarize (G.set_vertex_data b n g) k ns else boundarize g k ns
      end

  fun add_boundaries g k =
    let
      val nodes = G.get_unbboxed g
      val nodes = V.NSet.list_of nodes
      val nodes = get_boundaries g nodes
    in
      boundarize g k nodes
    end

  
  fun instantiate_graph (v,e,b,npb,k) =
    let
      val k1 = (mkX zero_angle)
      val k2 = (mkX (LinratAngleExpr.parse "1"))
      val k3 = (mkX (LinratAngleExpr.parse "a"))

      val num_nodes = v
      val num_edges = e
      val num_bbox = b
      val max_num_nodes_per_bbox = npb
      val bound_k = k
      val kinds = k1 :: k2 :: k3 :: []

      val graph = G.empty

      val _ = printout ("Instantiating " ^ string_of_int(num_nodes) ^ " nodes..\n")
      val nodes = generate_random_nodes num_nodes
      
      val _ = printout ("Instantiating " ^ string_of_int(num_edges) ^ " edges..\n")
      val graph = add_nodes graph nodes kinds

      val _ = printout ("Wiring nodes through edges..\n")
      val graph = if (num_edges > 0) then add_edges graph nodes num_edges else graph
     
      val _ = printout ("Adding " ^ string_of_int(num_bbox) ^ " bbox each containing at most " ^ string_of_int(max_num_nodes_per_bbox) ^ "..\n")
      val graph = if (num_bbox > 0) then add_bboxes graph nodes num_bbox max_num_nodes_per_bbox else graph
 
      val _ = printout ("Boundarizing input/output nodes with probability: 1/" ^ string_of_int(bound_k) ^ "..\n\n")
      val graph = if (bound_k > 0) then add_boundaries graph bound_k else graph
    in
      graph
    end


  fun instantiate_graphs n v e b npb k =
    if (n=1) then instantiate_graph(v,e,b,npb,k) :: []
    else instantiate_graph(v,e,b,npb,k) :: (instantiate_graphs (n-1) v e b npb k)

  fun instantiate_graph_names n =
    if (n=1) then (GraphName.mk ("g"^string_of_int(n))) :: []
    else (GraphName.mk ("g"^string_of_int(n))) :: instantiate_graph_names (n-1)
  
  fun enumerate_graphs [] = 0
    | enumerate_graphs (g::[]) = length (snd g)
    | enumerate_graphs (g::gs) = length (snd g) + (enumerate_graphs gs)

  fun get_pretty (g::[]) = (Pretty.str_of(G.pretty g))
    | get_pretty (g::gs) = (Pretty.str_of(G.pretty g)) ^ " \n " ^ (get_pretty gs)



  val num_pattern_graph = 100
  val num_nodes = 1
  val num_edges = 0
  val num_bbox = 0
  val max_num_nodes_per_bbox = 0
  val bound_k = 0

  val _ = printout ("Generating " ^ string_of_int(num_pattern_graph) ^ " graphs..\n\n")
  val graphs = instantiate_graphs num_pattern_graph num_nodes num_edges num_bbox max_num_nodes_per_bbox bound_k
  
  val _ = printout ("Generating " ^ string_of_int(num_pattern_graph) ^ " graph names..\n\n")
  val gnames = instantiate_graph_names num_pattern_graph

  val pretty_graphs = get_pretty graphs
  (*val _ = printout pretty_graphs*)


  val tab = GraphName.NTab.empty
  val tab = TD.fold GraphName.NTab.doadd gnames graphs tab
  val _ = printout ("\n\nBuilding the discrimination tree..\n\n")
  val tree = TD.mk tab
  (*val _ = printout (TD.printout (tree::[]))*)


  val num_target_graph = 1
  val num_nodes = 1
  val num_edges = 0
  val num_bbox = 0
  val max_num_nodes_per_bbox = 0
  val bound_k = 0

  val _ = printout ("Generating a target graph..\n\n")
  val targets = instantiate_graphs num_target_graph num_nodes num_edges num_bbox max_num_nodes_per_bbox bound_k
  val target = nth targets 0

  val _ = printout ("Pruning the tree using the target graph..\n\n")
  val graph_by_vertex = TD.extended_pruning tree target

  val _ = printout ("Evaluating performances..\n")
  val num_matchings = enumerate_graphs graph_by_vertex
  val _ = printout ("Number of times to exec matching alg WITHOUT dnets: " ^ string_of_int(num_pattern_graph*num_nodes) ^ "\n")
  val _ = printout ("Number of times to exec matching alg WITH dnets: " ^ string_of_int(num_matchings) ^ "\n\n")


  val _ = TextIO.closeOut out

