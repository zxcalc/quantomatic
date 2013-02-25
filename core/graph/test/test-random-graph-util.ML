(*The signature for random graph production.

This uses the random number generator inside isabelle: basics/library.ML
This random number generator uses references, so multiple instances of
Random_Graph should not be run at the same time.*)
signature RANDOM_GRAPH_GENERATOR =
sig
  type T (*The type of the graph structure usually*)
  type dir_or_undir (*Directed or undirected for edges*)
  (*Raised if there is something wrong with the arguments
  passed to a function*)
  exception RandomGraph_Arg_Exception of string
  (*Construct a new empty graph for the random graph generator
  to use in further functions.*)
  val empty : T
  (*Add a random number of node vertices to the graph between the two
  integers given*)
  val add_vertices : T
    -> (int * int) (* (Min,Max) - a range for the number of node
                      vertices to be added *)
    -> T
  (*Add a random number of edges to the graph. Note that these edges must
  connect between two node vertices permitting self-loops.
  
  If the maximum/minimum range supplied is greater than the maximum
  number that could be added, the values will be trimmed to provide
  a more even spread over the available range of values.*)
  val add_edges : T
    -> dir_or_undir (* Whether or not the edges should be directed *)
    -> (int * int)  (* (Min,Max) - a range for the number of edges 
                       to add between existing node vertices *)
    -> T
  (*Add a random number of input edges to the graph. Note that
  the number of inputs can exceed the number of vertices, but there
  must be at least one vertex in the graph already.*)
  val add_inputs : T
    -> (int * int) (* (Min,Max) - a range for the number of inputs 
                      to be added *)
    -> T
  (*Add a random number of output edges to the graph. Similarly to the
  inputs, there must be at least one vertex in the graph already.*)
  val add_outputs : T
    -> (int * int) (* (Min,Max) - a range for the number of outputs
                      to be added *)
    -> T
  (*Add a random number of circles to the graph. (No restrictions)
  These are always made up from two wire vertices connected by directed
  edges feeding into each other*)
  val add_circles : T
    -> (int * int) (* (Min,Max) - a range for the number of circles
                      to be added *)
    -> T
  (*Add a random number of bare edges to the graph. (No restrictions)
  These are always an input connected directly to an output*)
  val add_bare_wires : T
    -> (int * int) (* (Min,Max) - a range for the number of bare wires
                      to be added *)
    -> T
  (*Add a random number of bang boxes to the graph.
  This is a little hard to control. A random number of bang boxes will
  be chosen from the range supplied. Then, a random number of node vertices
  chosen from the other range will be added to each (individually - so
  randomly chosen for each case). All bang boxes will just be overlapping
  (no randomized nesting)
  
  If no range for the number of node vertices is supplied, a random number 
  if chosen from all available node vertices.
  
  If the range of node vertices to be added is greater than the number of
  available node vertices, the range is restricted to ensure a more even
  spread as in "add_edges".
  
  There must be at least one node vertex*)
  val add_bboxes : T
    -> (int * int) (* (Min,Max) - a range for the number of bang boxes
                      to be added *)
    -> (int * int) option (* (Min,Max) - a range for the number of node vertices
                              to be added to each bang box (optional) *)
    -> T
    
  (*There is currently no way to randomly the nesting of bang boxes. Having tried,
  to do this in a meaningfully random way (i.e: possibly uniform) is
  quite expensive as you must determine which are subsets of each other beforehand.
  It might be possible in roughly the following way:
  
  For memory:
  
  A fast way to generate random forests for the bang boxes is as follows:
  
  1. Construct a graph with the given set of vertices (bang boxes)
  with an edge (b1,b2) if b2 could be nested in b1. Include one additional vertex,
  which points to every other vertex. This ensures connectivity and will become
  the root of our tree eventually since nothing points to it.
  2. Generate with quasi-uniform probability a spanning tree quickly (??)
  - can be done quickly!
  3. Delete the root of the tree to create a forest.
  
  Note that all possible forests could be generated below the root vertex with uniform probability
  (ordering of children irrelevant), and each forest can only be connected up to the
  original vertex in one way so I think the forests are also generated with uniform
  probability too - there is a bijection from the forests to these trees.
  These forests could represent the bang box layout.
  *)
end;


(*A structure for generating "random" graphs for testing purposes.
Not opaque so that you can use existing graphs.*)
structure RandomGraphGenerator : RANDOM_GRAPH_GENERATOR =
struct

  local open Test_BBox_Util; open SimpleGraphUtil; in
  
  (***********************************************************)
  (***************************Types***************************)
  (***********************************************************)

  type T = Test_MkG.G.T (*Use the test graphs for simplicity*)
  type dir_or_undir = Test_MkG.G.dir_or_undir
  
  (***********************************************************)
  (********************Auxiliary Functions********************)
  (***********************************************************)
  
  (*Raised if there is something wrong with the arguments
  passed to a function*)
  exception RandomGraph_Arg_Exception of string
  
  (*Create a new empty graph*)
  val empty = empty
  
  (*Check that the min is indeed lower than the max*)
  fun check_range (min,max) = if (min<=max) then () else
    raise RandomGraph_Arg_Exception (
    "Bad range: min=" ^ Int.toString(min) ^
    " max=" ^ Int.toString(max))
  (*Apply a function to an argument n times*)
  fun repeat 0 _ arg = arg
    | repeat n f arg = repeat (n-1) f (f arg)
  
  (*Helper funtion for adding inputs and outputs*)
  fun add_inputs_or_outputs isInput g (min,max) =
    let
      val _ = check_range (min,max); (*Check that the range is legitimate*)
      val number = random_range min max; (*Choose how many I want to add*)
      (*Get the list of vertices, and check that the graph has at least one*)
      val vertices = get_list_node_vertex_names g;
      val n = List.length vertices;
      val _ = if (n=0) then
        if (min>0) then
          raise RandomGraph_Arg_Exception ("Cannot add " ^
          (if (isInput) then "inputs" else "outputs") ^ " to a graph" ^
          "with no node vertices")
        else
          () else ();
      (*Randomly choose to add a wire vertex and edge to the graph*)
      fun addInputOrOutput g' =
        let
          val choice = random_range 0 (n-1);
          val v = List.nth (vertices,choice);
          val w = unique_V_name g'
        in
          if isInput then add_edge_dir w v (add_wire_vertex w g')
          else add_edge_dir v w (add_wire_vertex w g')
        end
    in
      repeat number addInputOrOutput g
    end
  
  (*Remove the first element in the list which meets the predicate*)
  fun removeFirst _ [] = []
    | removeFirst f (x::xs) = if (f x) then xs else
      x::(removeFirst f xs)
  (*Remove the nth element from the list*)
  fun removeNth [] _ = []
    | removeNth (_::xs) 0 = xs
    | removeNth (x::xs) n = x::(removeNth xs (n-1))
  
  (***********************************************************)
  (********************Signature Functions********************)
  (***********************************************************)
  
  (*Add a random number of node vertices to the graph between the two
  integers given*)
  fun add_vertices g (min,max) =
    let
      val _ = check_range (min,max);
      val number = random_range min max; (*Choose how many I want to add*)
      fun add_vertex g' = add_node_vertex (unique_V_name g') g' (*Select a new name not already used by the graph*)
    in
      repeat number add_vertex g
    end
  (*Add a random number of edges to the graph. Note that these edges must
  connect between two vertices (see input/output) so the maximum number
  that can be added with this function is V^2 directed and (V^2 - V)/2 + V
  for undirected (permitting self-loops). If the maximum/minimum range
  supplied is greater than the maximum number that could be added, the
  values will be trimmed to provide a more even spread over
  the available range of values.
  
  IMPLEMENTATION:
  This works by identifying first all the possible edges that
  can exist (between node vertices), then subtracting from
  this all the edges that do exist between node vertices
  in the existing graph.
  
  Note that for directed edges an undirected edge removes both
  possible directions. For an undirected edge, a directed
  edge in either direction removes all possibilities.
  
  Once the number of possible edges that could be added has been
  calculated, the min and max values are checked against this,
  and reduced to at most this value. Hence, if only 5 edges could
  be added, and you specified the range 1 to 20, the random number
  would be chosen from 1 to 5 (20% each).
  *)
  fun add_edges g dir_or_undir (min,max) =
    let
      val _ = check_range (min,max); (*Check that the range is legitimate*)
      fun isDirected d = G.Directed=d
      val vertexList = get_list_node_vertex_names g;
      val edgeList = get_list_edge_names g;
      (*Construct a list of existing edges between node vertices...*)
      (*Return NONE if the edge is not between node vertices,
      and return SOME (src,tgt) otherwise*)
      fun isBetweenNVs e = let
        val src = get_source e g; (*Names of the source and target vertices*)
        val tgt = get_target e g
      in
        if ((is_node_vertex src g) andalso (is_node_vertex tgt g)) then
          SOME (src,tgt) else NONE end;
      (*Construct a list of all the pairs of vertices joined
      by edges in the existing graph*)
      fun existAlready' [] ps = ps
        | existAlready' (e::es) ps = case (isBetweenNVs e)
        of  NONE => existAlready' es ps
          | SOME (src,tgt) => if (is_directed e g) then
          existAlready' es ((src,tgt)::ps) else
          existAlready' es ((src,tgt)::(tgt,src)::ps);
      val existAlready = existAlready' edgeList [];
      (*Check if an edge is already in the existence list...*)
      fun alreadyExists (src,tgt) = if (isDirected dir_or_undir) then
          List.exists (fn (src',tgt') =>
          (vertex_eq src src' andalso vertex_eq tgt tgt')) existAlready
        else (*Need to check both directions*)
          (List.exists (fn (src',tgt') =>
          (vertex_eq src src' andalso vertex_eq tgt tgt')) existAlready)
          orelse
          (List.exists (fn (tgt',src') =>
          (vertex_eq src src' andalso vertex_eq tgt tgt')) existAlready)
      (*Construct a list of edges which could be added between node vertices...*)
      fun pairWithAll _ [] = []
        | pairWithAll v (u::us) = if (alreadyExists (v,u)) then
          (*Undirected edges have been added in both ways*)
          (pairWithAll v us) else (v,u)::(pairWithAll v us);
      (*For the overall result...*)
      fun allPossibleEdges' [] _ = []
        | allPossibleEdges' (v::vs) us = (pairWithAll v us)@(allPossibleEdges' vs us);
      (*Find all duplicates for the undirected case*)
      fun removeDuplicates [] = []
        | removeDuplicates ((src,tgt)::es) =
          (src,tgt)::(removeDuplicates (removeFirst (fn (src',tgt') =>
          (vertex_eq tgt' src) andalso (vertex_eq src' tgt)) es));
      val allPossibleEdges = if (isDirected dir_or_undir) then
        allPossibleEdges' vertexList vertexList
        else removeDuplicates (allPossibleEdges' vertexList vertexList);
      val n = List.length allPossibleEdges;
      val number = random_range (*Restrict the range if necessary*)
        (if (min>n) then n else min)
        (if (max>n) then n else max); (*Choose how many I want to add*)
      (*Now add all of the edges...*)
      fun addEdges _ 0 _ g = g
        | addEdges es cs n' g = let
            val choice = random_range 0 (n'-1);
            val (src,tgt) = List.nth (es,choice)
          in
            addEdges (removeNth es choice) (cs-1) (n'-1) (
            if (isDirected dir_or_undir) then add_edge_dir src tgt g
            else add_edge_undir src tgt g)
          end
    in
      (*All possible edges will contain no duplicates in the directed
      and undirected case, and will not contain edges already in the graph*)
      addEdges allPossibleEdges number n g
    end
  (*Add a random number of input edges to the graph. Note that
  the number of inputs can exceed the number of vertices, but there
  must be at least one vertex in the graph already.*)
  val add_inputs = add_inputs_or_outputs true
  (*Add a random number of output edges to the graph. Similarly to the
  inputs, there must be at least one vertex in the graph already.*)
  val add_outputs = add_inputs_or_outputs false
  (*Add a random number of circles to the graph. (No restrictions)
  These are always made up from two wire vertices connected by directed
  edges feeding into each other*)
  fun add_circles g (min,max) =
    let
      val _ = check_range (min,max); (*Check that the range is legitimate*)
      val number = random_range min max; (*Choose how many I want to add*)
      fun addCircle g' =
        let
          val w1 = unique_V_name g';
          val g2' = add_wire_vertex w1 g';
          val w2 = unique_V_name g2'
        in
          ((add_edge_dir w2 w1) o (add_edge_dir w1 w2) o (add_wire_vertex w2)) g2'
        end
    in
      repeat number addCircle g
    end
  (*Add a random number of bare edges to the graph. (No restrictions)
  These are always an input connected directly to an output*)
  fun add_bare_wires g (min,max) =
    let
      val _ = check_range (min,max); (*Check that the range is legitimate*)
      val number = random_range min max; (*Choose how many I want to add*)
      fun addBareWire g' =
        let
          val w1 = unique_V_name g';
          val g2' = add_wire_vertex w1 g';
          val w2 = unique_V_name g2'
        in
          ((add_edge_dir w1 w2) o (add_wire_vertex w2)) g2'
        end
    in
      repeat number addBareWire g
    end
  (*Add a random number of bang boxes to the graph.
  This is a little hard to control. A random number of bang boxes will
  be chosen from the range supplied. Then, a random number of node vertices
  chosen from the other range will be added to each (individually - so
  randomly chosen for each case). All bang boxes will just be overlapping
  (no randomized nesting)
  
  If no range for the number of node vertices is supplied, a random number 
  if chosen from all available node vertices.
  
  If the range of node vertices to be added is greater than the number of
  available node vertices, the range is restricted to ensure a more even
  spread as in "add_edges".
  
  There must be at least one node vertex*)
  fun add_bboxes g (min,max) vRng =
    let
      (*Check both ranges are OK*)
      val _ = check_range (min,max);
      val _ = if (is_none vRng) then () else (check_range o the) vRng;
      val number = random_range min max; (*Choose how many I want to add*)
      (*Figure out the range for the node vertices...*)
      val vertices = get_list_node_vertex_names g;
      val n = List.length vertices;
      (*Check there is at least one node vertex in the graph...*)
      val _ = if ((n=0) andalso (min>0)) then
        raise RandomGraph_Arg_Exception ("Cannot add " ^
          "bang boxes to a graph with no node vertices") else ();
      val (min_t,max_t) = (if (is_none vRng) then 1 else (fst o the) vRng,
        if (is_none vRng) then n else (snd o the) vRng);
      (*Adjust for maximum possible*)
      val (min',max') = (if (min_t>n) then n else min_t,
        if (max_t>n) then n else max_t);
      (*This will randomly add vertices to the given bang box*)
      fun addVerticesToBBox _ 0 _ g' = g'
        | addVerticesToBBox b n vs g' =
          let
            val choice = random_range 0 (n-1);
            val g'' = add_vertex_to_bbox b (List.nth (vs,choice)) g'
          in
            addVerticesToBBox b (n-1) (removeNth vs choice) g''
          end
    in
      repeat number (fn g' =>
        let
          val b' = unique_B_name g';
          val g'' = add_bbox b' g';
          val choice = random_range min' max'
        in (*Add vertices to the new bang box randomly*)
          addVerticesToBBox b' choice vertices g''
        end) g
    end
    
    end (*End of local*)
end;

(*These functions are here to test the random-graph generator.
Since the graphs are generated at random, it is more
what is created that is of interest...*)
structure Test_RandomGraphGenerator =
struct
  local
  open Test_BBox_Util;
  open RandomGraphGenerator;
  open SimpleGraphUtil;
  (*Return the number of node vertices in the graph*)
  fun vertex_count g = V.NSet.cardinality (G.get_node_vertices g)
  in
    (*Repeat a function n times (the function should expect unit)*)
    fun repeat 0 _ = ()
      | repeat n f = (f();repeat (n-1) f)
    (*Generate a graph with a random number of vertices*)
    fun rand_vertices (min,max) = fn () =>
      add_vertices empty (min,max)
    (*Generate a graph with a random number of edges, based on how many node vertices there are.
    The edges to be added are all directed
    (Permits the full range of edges)*)
    fun rand_edges g = fn () =>
      add_edges g G.Directed (0,(vertex_count g) * (vertex_count g))
  end;
end;
