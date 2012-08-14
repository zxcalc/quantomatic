
  open Test_MkG
  open Test_MatchUtil

  
  structure LIB = TDNet_Library
  structure TD = Top_DNet(G)
  structure CL = TD.CL
  structure C = CL.C
  structure L = C.L

  val timestamp = Time.toString (Time.now())
  val out = TextIO.openOut ("output"^timestamp^".txt")

  val k1 = (mkX zero_angle)
  val k2 = (mkX (LinratAngleExpr.parse "1"))
  val k3 = (mkX (LinratAngleExpr.parse "a"))

  val iterations = 100
  val pattern_params = {kinds = k1 :: k2 :: k3 :: [],

                        num_graphs = 100,
                        num_nodes = 5,
                        num_edges = 5,
                        num_bbox = 2,
                        max_num_nodes_per_bbox = 1,
                        bound_k = 2 }

  val target_params = {kinds = k1 :: k2 :: k3 :: [],

                       num_graphs = 1,
                       num_nodes = 20,
                       num_edges = 20,
                       num_bbox = 0,
                       max_num_nodes_per_bbox = 0,
                       bound_k = 0 }




  fun printout msg =
    TextIO.outputSubstr(out, Substring.substring (msg,0,size msg))



  fun generate_random_nodes n =
    if (n = 1) then V.mk ("v" ^ string_of_int(n)) :: []
    else V.mk ("v" ^ string_of_int(n)) :: generate_random_nodes (n-1)

  fun add_nodes g [] k = g
    | add_nodes g (n::[]) k = g |> G.add_named_vertex n (one_of k)
    | add_nodes g (n::ns) k = add_nodes (g |> G.add_named_vertex n (one_of k)) ns k

  fun generate_random_edges e =
    if (e = 1) then E.mk ("e" ^ string_of_int(e)) :: []
    else E.mk ("e" ^ string_of_int(e)) :: generate_random_edges (e-1)

  fun add_edges g n [] = g
    | add_edges g n (e::[]) = 
        let
          val node1 = one_of n
          val n_minus = LIB.rm_element (V.name_eq) node1 n
          val node2 = one_of n_minus
        in
          g |> G.add_named_edge e dir_edge node1 node2
        end
    | add_edges g n (e::es) =
        let
          val node1 = one_of n
          val n_minus = LIB.rm_element (V.name_eq) node1 n
          val node2 = one_of n_minus
        in
          add_edges (g |> G.add_named_edge e dir_edge node1 node2) n es
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
      if (num_nodes=1) then (one_of (n::ns)) :: bb_nodes
      else
        let
          val node = one_of(n::ns)
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
        val num_nodes =  random_range 1 npbb
        val bb_nodes : V.name list = []
        val nodes = get_connected_nodes g n num_nodes bb_nodes
        val (bb,g) = g |> G.add_bbox
      in
        add_bbox g bb nodes       
      end
    else
      let
        val num_nodes =  random_range 1 npbb
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
        val dice = random_range 1 k
      in
        if (dice=1) then G.set_vertex_data b n g else g
      end
    | boundarize g k (n::ns) = 
      let
        val b = L.boundary
        val dice = random_range 1 k
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

  
  fun instantiate_graph params =
    let
      val graph = G.empty

      val num_nodes = #num_nodes params
      (*val _ = printout ("Instantiating " ^ string_of_int(num_nodes) ^ " nodes..\n")*)
      val nodes = generate_random_nodes num_nodes
      
      val kinds = #kinds params
      (*val _ = printout ("Adding the nodes to the graph selecting 1 out of "^ string_of_int(length kinds) ^" possible kinds..\n")*)
      val graph = add_nodes graph nodes kinds 

      val num_edges = #num_edges params
      (*val _ = printout ("Instantiating " ^ string_of_int(num_edges) ^ " edges..\n")*)
      val edges = if (num_edges > 0) then generate_random_edges num_edges else []

      (*val _ = printout ("Wiring nodes through edges..\n")*)
      val graph = if (num_edges > 0) then add_edges graph nodes edges else graph
      
      val num_bbox = #num_bbox params
      val max_num_nodes_per_bbox = #max_num_nodes_per_bbox params
      (*val _ = printout ("Adding " ^ string_of_int(num_bbox) ^ " bbox each containing at most " ^ string_of_int(max_num_nodes_per_bbox) ^ "..\n")*)
      val graph = if (num_bbox > 0) then add_bboxes graph nodes num_bbox max_num_nodes_per_bbox else graph
 
      val bound_k = #bound_k params
      (*val _ = if (bound_k > 0) then printout ("Boundarizing input/output nodes with probability: 1/" ^ string_of_int(bound_k) ^ "..\n\n")
              else printout ("Boundarizing input/output nodes with probability: 0..\n\n")*)
      val graph = if (bound_k > 0) then add_boundaries graph bound_k else graph
    in
      graph
    end


  fun iterate_instantiate_graph i params =
    if (i=1) then (instantiate_graph params) :: []
    else (instantiate_graph params) :: (iterate_instantiate_graph (i-1) params)

  fun iterate_instantiate_graph_name i =
    if (i=1) then (GraphName.mk ("g"^string_of_int(i))) :: []
    else (GraphName.mk ("g"^string_of_int(i))) :: (iterate_instantiate_graph_name (i-1))
    
  
  fun enumerate_graphs [] = []
    | enumerate_graphs (g::[]) = length (snd g) :: []
    | enumerate_graphs (g::gs) = length (snd g) :: (enumerate_graphs gs)

  fun sum [] = 0
    | sum (n::[]) = n
    | sum (n::ns) = n + sum ns

  fun get_pretty (g::[]) = (Pretty.str_of(G.pretty g))
    | get_pretty (g::gs) = (Pretty.str_of(G.pretty g)) ^ " \n " ^ (get_pretty gs)


  

  fun stats pattern_params target_params =
    let
      (*val _ = printout ("Generating " ^ string_of_int(#num_graphs pattern_params) ^ " graphs..\n\n")*)
      val pattern_graphs = iterate_instantiate_graph (#num_graphs pattern_params) pattern_params
  
      (*val _ = printout ("Generating " ^ string_of_int(#num_graphs pattern_params) ^ " graph names..\n\n")*)
      val pattern_gnames = iterate_instantiate_graph_name (#num_graphs pattern_params)

      (*val pretty_graphs = get_pretty graphs*)
      (*val _ = printout pretty_graphs*)

      val tab = GraphName.NTab.empty
      val tab = TD.fold GraphName.NTab.doadd pattern_gnames pattern_graphs tab
      (*val _ = printout ("\n\nBuilding the discrimination tree..\n\n")*)
      val tree = TD.mk tab
      (*val _ = printout (TD.printout (tree::[]))*)

      (*val _ = printout ("Generating a target graph..\n\n")*)
      val target_graphs = iterate_instantiate_graph (#num_graphs target_params) target_params
      val target_graph = nth target_graphs 0

      (*val _ = printout ("Pruning the tree using the target graph..\n\n")*)
      val ext_graph_by_vertex = TD.extended_pruning tree target_graph
      val std_graph_by_vertex = TD.standard_pruning tree target_graph

      (*val _ = printout ("Evaluating performances..\n")*)
      val ext_matchings = enumerate_graphs ext_graph_by_vertex
      val std_matchings = enumerate_graphs std_graph_by_vertex
      val ext_num_matchings = sum ext_matchings
      val std_num_matchings = sum std_matchings
      (*val _ = printout ("Number of times to exec matching alg WITHOUT dnets: " ^ string_of_int((#num_graphs pattern_params)*(#num_nodes target_params)) ^ "\n")
      val _ = printout ("Number of times to exec matching alg WITH dnets: " ^ string_of_int(num_matchings) ^ "\n\n")*)
    in
      (ext_num_matchings,std_num_matchings)
    end

  fun iterate_stats i pattern_params target_params =
    if (i = 1) then (stats pattern_params target_params) :: []
    else (stats pattern_params target_params) :: (iterate_stats (i-1) pattern_params target_params)



  fun compute_num_svr [] m = 0.0
    | compute_num_svr (x::[]) m = (x-m) * (x-m)
    | compute_num_svr (x::xs) m = ((x-m) * (x-m)) + (compute_num_svr xs m)

  fun toReal [] = []
    | toReal (x::[]) = Real.fromInt(x)::[]
    | toReal (x::xs) = Real.fromInt(x)::(toReal xs)

  (*fun stat_test_ext_alg i pattern_params target_params =
    let
      val matchings = iterate_stats_ext_alg i pattern_params target_params

      val matchings_without_alg = (#num_graphs pattern_params)*(#num_nodes target_params)
      
      val num_matchings = sum matchings
      val avg_matchings = Real.fromInt(num_matchings) / Real.fromInt(i)

      val num_svr = compute_num_svr (toReal matchings) avg_matchings 
      val svr_matchings = num_svr / Real.fromInt(i)

      val _ = printout ("\n\n\n\n")
      
      val _ = printout ("***************************************************************************\n")
      
      val _ = printout ("I executed " ^ string_of_int(i) ^ " iterations with these parameters: \n\n")
      
      val _ = printout ("PATTERNS: \n")
      val _ = printout ("Graphs: " ^ string_of_int(#num_graphs pattern_params)^ "\n")
      val _ = printout ("Nodes: " ^ string_of_int(#num_nodes pattern_params)^ "\n")
      val _ = printout ("Edges: " ^ string_of_int(#num_edges pattern_params)^ "\n")
      val _ = printout ("Kinds: " ^ string_of_int(length (#kinds pattern_params)) ^ "\n")
      val _ = printout ("BBoxes: " ^ string_of_int(#num_bbox pattern_params)^ "\n")
      val _ = printout ("Max nodes per bbox: " ^ string_of_int(#max_num_nodes_per_bbox pattern_params)^ "\n")
      val _ = if (#bound_k pattern_params>0) then printout ("Prob boundary: 1/" ^ string_of_int(#bound_k pattern_params)^ "\n")
              else printout ("Prob boundary: 0\n")

      val _ = printout ("\n")
      
      val _ = printout ("TARGETS: \n")
      val _ = printout ("Graphs: " ^ string_of_int(#num_graphs target_params)^ "\n")
      val _ = printout ("Nodes: " ^ string_of_int(#num_nodes target_params)^ "\n")
      val _ = printout ("Edges: " ^ string_of_int(#num_edges target_params)^ "\n")
      val _ = printout ("Kinds: " ^ string_of_int(length (#kinds target_params)) ^ "\n")
      val _ = printout ("BBoxes: " ^ string_of_int(#num_bbox target_params)^ "\n")
      val _ = printout ("Max nodes per bbox: " ^ string_of_int(#max_num_nodes_per_bbox target_params)^ "\n")
      val _ = if (#bound_k target_params>0) then printout ("Prob boundary: 1/" ^ string_of_int(#bound_k target_params)^ "\n")
              else printout ("Prob boundary: 0\n")

      val _ = printout ("\n\n\n")

      val _ = printout ("STAT RESULTS: \n")
      val _ = printout ("Normally I would need to run: " ^ string_of_int(matchings_without_alg)^ " iterations of the matching alg.\n")
      val _ = printout ("Now I need to run only: " ^ Real.toString(avg_matchings)^ "(avg) " ^ Real.toString(svr_matchings) ^ "(var) iterations of the matching alg.\n")
      
      val _ = printout ("***************************************************************************\n")
    in
      1
    end*)


  fun stat_test i pattern_params target_params =
    let
      val matchings = iterate_stats i pattern_params target_params

      val ext_matchings = fst (split_list matchings)
      val std_matchings = snd (split_list matchings)

      val matchings_without_alg = (#num_graphs pattern_params)*(#num_nodes target_params)
      
      val ext_num_matchings = sum ext_matchings
      val ext_avg_matchings = Real.fromInt(ext_num_matchings) / Real.fromInt(i)
      val ext_num_sample_var = compute_num_svr (toReal ext_matchings) ext_avg_matchings 
      val ext_sample_var_matchings = ext_num_sample_var / Real.fromInt(i)

      val std_num_matchings = sum std_matchings
      val std_avg_matchings = Real.fromInt(std_num_matchings) / Real.fromInt(i)
      val std_num_sample_var = compute_num_svr (toReal ext_matchings) std_avg_matchings 
      val std_sample_var_matchings = std_num_sample_var / Real.fromInt(i)

      val _ = printout ("\n\n\n\n")
      
      val _ = printout ("************************************************************************************\n")
      
      val _ = printout ("EXTENDED vs STANDARD vs NO-ALGORITHM!!\n\n")

      val _ = printout ("I executed " ^ string_of_int(i) ^ " iterations with these parameters: \n\n")
      
      val _ = printout ("PATTERNS: \n")
      val _ = printout ("Graphs: " ^ string_of_int(#num_graphs pattern_params)^ "\n")
      val _ = printout ("Nodes: " ^ string_of_int(#num_nodes pattern_params)^ "\n")
      val _ = printout ("Edges: " ^ string_of_int(#num_edges pattern_params)^ "\n")
      val _ = printout ("Kinds: " ^ string_of_int(length (#kinds pattern_params)) ^ "\n")
      val _ = printout ("BBoxes: " ^ string_of_int(#num_bbox pattern_params)^ "\n")
      val _ = printout ("Max nodes per bbox: " ^ string_of_int(#max_num_nodes_per_bbox pattern_params)^ "\n")
      val _ = if (#bound_k pattern_params>0) then printout ("Prob boundary: 1/" ^ string_of_int(#bound_k pattern_params)^ "\n")
              else printout ("Prob boundary: 0\n")

      val _ = printout ("\n")
      
      val _ = printout ("TARGETS: \n")
      val _ = printout ("Graphs: " ^ string_of_int(#num_graphs target_params)^ "\n")
      val _ = printout ("Nodes: " ^ string_of_int(#num_nodes target_params)^ "\n")
      val _ = printout ("Edges: " ^ string_of_int(#num_edges target_params)^ "\n")
      val _ = printout ("Kinds: " ^ string_of_int(length (#kinds target_params)) ^ "\n")
      val _ = printout ("BBoxes: " ^ string_of_int(#num_bbox target_params)^ "\n")
      val _ = printout ("Max nodes per bbox: " ^ string_of_int(#max_num_nodes_per_bbox target_params)^ "\n")
      val _ = if (#bound_k target_params>0) then printout ("Prob boundary: 1/" ^ string_of_int(#bound_k target_params)^ "\n")
              else printout ("Prob boundary: 0\n")

      val _ = printout ("\n\n\n")

      val _ = printout ("STAT RESULTS: \n")
      val _ = printout ("Normally I would need to run: " ^ string_of_int(matchings_without_alg)^ " iterations of the matching alg.\n")
      val _ = printout ("With the standard algorithm I would need to run: " ^ Real.toString(std_avg_matchings)^ "(avg) " ^ Real.toString(std_sample_var_matchings) ^ "(var) iterations of the matching alg.\n")
      val _ = printout ("Now with the extended algorithm I need to run only: " ^ Real.toString(ext_avg_matchings)^ "(avg) " ^ Real.toString(ext_sample_var_matchings) ^ "(var) iterations of the matching alg.\n")
      
      val _ = printout ("************************************************************************************\n")
    in
      1
    end



  val signal = stat_test iterations pattern_params target_params

  val _ = TextIO.closeOut out

