(*This file contains various structures designed to test the speed of the matching
algorithms. It focuses on testing the speed of the speed of the concrete algorithm
as well as the greedy algorithm which supports bang box expansion.*)

local
  (*Files needed for sanity*)
  open RandomGraphGenerator;
  (*  open Test_BBox_Util; *)
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;
  (*open Test_BGMatchUtil;*)
  open SimpleGraphUtil;
  (*The original structures:*)
  structure Speed_Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState);
  structure Speed_Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Speed_Test_InnerMatcher);
  structure Concrete_M = Speed_Test_ConcreteMatcher;
  structure Greedy_M = Test_GreedyMatcher;
  structure Inner_M = Speed_Test_InnerMatcher;
  structure Inner_BG_M = Test_BGInnerMatcher;
  structure MatchState = Test_BGMatchState;
  (*Parallel versions:*)
  structure Inner_Parallel_M = SimpleInnerMatchSearchParallel(structure MatchState = Test_BGMatchState);
  structure Concrete_Parallel_M = ConcreteMatchSearchParallel(structure InnerMatchSearch = Inner_Parallel_M);
  structure Inner_BG_Parallel_M =
  SimpleInnerMatchSearchParallel(structure MatchState = Test_BGMatchState);
  structure Greedy_Parallel_M = GreedyMatchSearchParallel(
    
  (*structure InnerMatchSearch = Inner_BG_Parallel_M (*Uses parallel atm*)*)
    structure InnerMatchSearch = Inner_BG_M;
    structure BGMatchState = Test_BGMatchState);
  
  (*Other useful structures:*)
  structure File = SimpleFileIO; (*For writing to files*)
  structure Future = Future_Wrapper;
  
  fun infinite() = infinite()
  (*Wrap a sequential function to make it behave more "fairly"*)
  fun wrap_sequential sq =
        fn() =>(
        let
          val num_cores = Thread.numProcessors();
          fun gen_futures 0 = []
            | gen_futures n = let
                val f = Future.future infinite
              in
                f::(gen_futures (n-1))
              end;
          val to_cancel = gen_futures (num_cores-1);
          fun cancel_all [] = ()
            | cancel_all (f::fs) = (Future.cancel f;cancel_all fs)
        in
          (sq();cancel_all to_cancel;())
        end)
  
  (*The type of a print function*)
  type print = G.T -> G.T -> Time.time -> unit
  
  (*Construct a random target graph (no bang boxes) with n vertices.*)
  fun random_target n = add_edges (add_vertices empty (n,n)) Directed (0,n*n)
  fun complete_target n = add_edges (add_vertices empty (n,n)) Directed (n*n,n*n)
  (*Construct a target graph with the given number of circles.*)
  fun circles_target n = add_circles empty (n,n)
  
  (*Construct a railtrack graph. This is essentially a group of pattern three (rail) graphs joined
  together, and results in a large number of matches.
  
  Note: matching grows unexpectedly quickly! The number of matches can be computed with this formula
  
  A(0)= 0; A(K+1) = ((2*K)*A(K)) + 1
  *)
  fun railtrack_target n =
    let
      fun rails _ _ 0 g = g
        | rails w1 w2 n g = (*The previous wire vertices to add rail to...*)
          let
            (*Add the necessary vertices...*)
            val w3 = unique_V_name g;
            val g = add_wire_vertex w3 g;
            val w4 = unique_V_name g;
            val g = add_wire_vertex w4 g;
            val n1 = unique_V_name g;
            val g = add_node_vertex n1 g;
            val n2 = unique_V_name g;
            val g = add_node_vertex n2 g;
          in
            (*Add the edges to form the rest of the graph...*)
            rails w3 w4 (n-1) (
              g |> add_edge_dir w1 n1
                |> add_edge_dir n1 w3
                |> add_edge_dir w2 n2
                |> add_edge_dir n2 w4
                |> add_edge_dir n1 n2
                |> add_edge_dir n2 n1)
          end
    in
      if (n=0) then G.empty else
        (*Create the first two wire vertices...*)
        let
          val g = G.empty;
          val w1 = unique_V_name g;
          val g = add_wire_vertex w1 g;
          val w2 = unique_V_name g;
          val g = add_wire_vertex w2 g
        in
          rails w1 w2 n g
        end
    end
  
  (*Other helpful functions*)
  
  (*Pull elements from a sequence, but allow them to be throw away*)
  fun pull_all sq = case (Seq.pull sq) of
      NONE => ()
    | SOME (x,sq) => pull_all sq
  
  (*Note: the pattern graph should come first when matching with the greedy
  algorithm.*)
  
  (*Force the evaluation of all the matches, and return the results as a list*)
  fun match_all_concrete g1 g2 = pull_all (Concrete_M.match g1 g2)
  fun match_all_greedy g1 g2 = pull_all (Greedy_M.match g1 g2)
  (*Wrapped version of the sequential algorithm*)
  fun match_all_concrete_wrapped g1 g2 = (wrap_sequential (fn () => (
    let val ls = pull_all (Concrete_M.match g1 g2) in
    (*PolyML.print ("Seq Contained: " ^ Int.toString(List.length ls));*)() end))) ()
  fun match_all_greedy_wrapped g1 g2 =
    (wrap_sequential (fn () => (
    let val ls = pull_all (Greedy_M.match g1 g2) in
    (*PolyML.print ("Seq Contained: " ^ Int.toString(List.length ls));*)() end))) ()
  (*Parallel version!*)
  fun match_all_parallel_concrete g1 g2 = (*let
    val n = List.length (Seq.list_of (Concrete_Parallel_M.match g1 g2))
    in ((*PolyML.print("Par Contained " ^ Int.toString(n));*)()) end*)
    pull_all (Concrete_Parallel_M.match g1 g2)
  fun match_all_parallel_greedy g1 g2 = (*let
    val n = List.length (Seq.list_of (Greedy_Parallel_M.match g1 g2))
    in (PolyML.print("Par Contained " ^ Int.toString(n));()) end*)
    pull_all (Greedy_Parallel_M.match g1 g2)
  
  (*Force the evaluation of all matching circles only, and return the results as a list*)
  fun match_all_circles_concrete g1 g2 = Seq.list_of (
    Seq.filter MatchState.is_total (Inner_M.match_circles (
    MatchState.init_and_schedule_all (MatchState.G.normalise g1) (MatchState.G.normalise g2))))
  fun match_all_circles_parallel_concrete g1 g2 = Seq.list_of (
    Seq.filter MatchState.is_total (Inner_Parallel_M.match_circles (
    MatchState.init_and_schedule_all (MatchState.G.normalise g1) (MatchState.G.normalise g2))))
  
  (*Create a list of numbers between the specified range*)
  fun --- (n1,n2) = if (n1>n2) then --- (n2,n1) else
    if (n1=n2) then [n1] else n1::(---(n1+1,n2))
  infix ---;
  (*Stick a common element to the right in a pair with all elements in a list*)
  fun stick [] _ = []
        | stick (x::xs) n = (x,n)::(stick xs n)
  (*Create a list of pairs of integers, specifying a fixed number
  of repeats and creating a range of integers*)
  fun range_fixed_repeats (low,high) repeat = stick (low --- high) repeat
in
  (*A structure containing the functions for dealing with the file.
  Not thread safe!*)
  structure Test_MatchingSpeedFile =
  struct
    exception ERROR of string
    (*The default file to use*)
    val file = "matching/test/time-results.txt";
    (*Open the file and be ready to append it...*)
    val is_open = Unsynchronized.ref NONE;
    (*Get the file*)
    fun get_file() = case (!is_open) of
        NONE => raise ERROR "cannot get the file"
      | (SOME f) => f
    (*Test to see if the file is currently open*)
    fun is_file_open() = not (is_none (!is_open))
    (*Open and return the file for appending*)
    fun append() = if (is_file_open()) then get_file()
      else let val ans = File.append file in
        (is_open:=SOME ans;ans) end
    (*Return the file for writing. Wipes out everything
    previously in the file*)
    fun write() = if (is_file_open()) then
      (File.close (get_file());let val ans = File.write file in
        (is_open:=SOME ans;ans) end) else
        let val ans = File.write file in (is_open:=SOME ans;ans) end
    (*Close the file*)
    fun close() = if (is_file_open()) then
        (File.close (get_file());is_open:=NONE;())
      else ()
    (*Remove all the contents in the file.
    This leaves the file closed.*)
    fun renew() =
      (
      if (is_file_open()) then close() else ();
      write();
      close()
      )
  end;

  (*This structure contains predefined (not random) graphs
  for testing purposes*)
  structure Test_MatchingSpeedPredfinedGraphs =
  struct
    (*A graph with a fixed number of circles*)
    val circle_graph = circles_target 4
    (*Input/output wire vertices:*)
    val W_input = create_vertex "input";
    val W_output1 = create_vertex "output1";
    val W_output2 = create_vertex "output2";
    (*Other wire vertices*)
    val W_1 = create_vertex "W_1";
    val W_2 = create_vertex "W_2";
    val W_3 = create_vertex "W_3";
    val W_4 = create_vertex "W_4";
    (*Node vertices*)
    val N_1 = create_vertex "N_1";
    val N_2 = create_vertex "N_2";
    val N_3 = create_vertex "N_3";
    val N_4 = create_vertex "N_4";
    val N_5 = create_vertex "N_5";
    (*Bang boxes*)
    val B_1 = create_bbox "B_1";
    val B_2 = create_bbox "B_2";
    val B_3 = create_bbox "B_3";
    (*A small pattern graph! See Pattern Graph 1 if you have Graph Drawing
    for a visual representation*)
    val pattern_graph1 =
      empty |> add_wire_vertex W_input (*Add inputs/outputs*)
            |> add_wire_vertex W_output1
            |> add_wire_vertex W_output2
            |> add_wire_vertex W_1 (*Add wire vertices*)
            |> add_wire_vertex W_2
            |> add_wire_vertex W_3
            |> add_wire_vertex W_4
            |> add_node_vertex N_1 (*Node vertices*)
            |> add_node_vertex N_2
            |> add_node_vertex N_3
            |> add_node_vertex N_4
            |> add_node_vertex N_5
            |> add_edge_dir W_input N_1 (*Edges - Stem*)
            |> add_edge_dir N_1 W_1 (*Left side (on diagram)*)
            |> add_edge_dir W_1 N_2
            |> add_edge_dir N_2 W_2
            |> add_edge_dir W_2 N_3
            |> add_edge_dir N_3 W_output1
            |> add_edge_dir N_1 W_3 (*Right side (on diagram)*)
            |> add_edge_dir W_3 N_4
            |> add_edge_dir N_4 W_4
            |> add_edge_dir W_4 N_5
            |> add_edge_dir N_5 W_output2
            |> add_bbox B_1 (*Bang boxes - top left (only node vertices added*)
            |> add_vertex_to_bbox B_1 N_3
            |> add_bbox B_2 (*Central*)
            |> add_vertex_to_bbox B_2 N_2
            |> add_vertex_to_bbox B_2 N_4
            |> add_bbox B_3 (*Mini right*)
            |> add_vertex_to_bbox B_3 N_4
            |> set_bbox_parent B_3 B_2 (*Set the parent relation*)
    
    (*A fixed concrete graph. See Concrete Graph 1 (its a square)*)
    val concrete_graph1 =
      empty |> add_wire_vertex W_1 (*Add wire vertices*)
            |> add_wire_vertex W_2
            |> add_wire_vertex W_3
            |> add_wire_vertex W_4
            |> add_node_vertex N_1 (*Node vertices*)
            |> add_node_vertex N_2
            |> add_node_vertex N_3
            |> add_node_vertex N_4
            |> add_edge_dir N_1 W_1 (*Edges*)
            |> add_edge_dir W_1 N_2
            |> add_edge_dir N_2 W_2
            |> add_edge_dir W_2 N_3
            |> add_edge_dir N_3 W_3
            |> add_edge_dir W_3 N_4
            |> add_edge_dir N_4 W_4
            |> add_edge_dir W_4 N_1
    
    (*A graph of a line. See Concrete Graph 2
    This is primarily for testing against extremely large target graphs.*)
    val concrete_graph2 =
      empty |> add_wire_vertex W_1 (*Add wire vertices*)
            |> add_node_vertex N_1 (*Node vertices*)
            |> add_node_vertex N_2
            |> add_edge_dir N_1 W_1 (*Edges*)
            |> add_edge_dir W_1 N_2
            
    (*The square graph, with the left half surrounded by a bang box*)
    val pattern_graph2 = concrete_graph1
            |> add_bbox B_1
            |> add_vertex_to_bbox B_1 N_1
            |> add_vertex_to_bbox B_1 N_2
            
    (*A "rail". Due to a misunderstanding with what is considereda match, this pattern will match
    well against rail track like graphs. The rail can be copied and connected together*)
    val pattern_graph3 =
      empty |> add_wire_vertex W_1 (*Wire vertices*)
            |> add_wire_vertex W_2
            |> add_wire_vertex W_3
            |> add_wire_vertex W_4
            |> add_node_vertex N_1 (*Node vertices*)
            |> add_node_vertex N_2
            |> add_edge_dir W_1 N_1 (*Edges*)
            |> add_edge_dir N_1 W_2
            |> add_edge_dir N_1 N_2
            |> add_edge_dir N_2 N_1
            |> add_edge_dir W_3 N_2
            |> add_edge_dir N_2 W_4
            |> add_bbox B_1 (*To surround the whole graph*)
            |> add_vertex_to_bbox B_1 W_1
            |> add_vertex_to_bbox B_1 W_2
            |> add_vertex_to_bbox B_1 W_3
            |> add_vertex_to_bbox B_1 W_4
            |> add_vertex_to_bbox B_1 N_1
            |> add_vertex_to_bbox B_1 N_2
            
  end;
  
  (*A structure contain various print functions:*)
  structure Test_MatchingSpeedPrinters =
  struct
    structure File_Aux = Test_MatchingSpeedFile;
    (*This printer will print the following:
    
    Time taken (ns)
    The number of node vertices in both graphs.
    The number of edges in both graphs.
    
    It will write to the default file.
    
    G1: |V| |E| G2: |V| |E| Time: time
    *)
    fun numbers_printer g1 g2 time =
      let
        val file = File_Aux.append();
      in
        (
        File.writeStr file ("G1: " ^ Int.toString(node_vertex_count g1) ^ " " ^ Int.toString(edge_count g1) ^ " ");
        File.writeStr file ("G2: " ^ Int.toString(node_vertex_count g2) ^ " " ^ Int.toString(edge_count g2) ^ " ");
        File.writeStr file ("Time: " ^ LargeInt.toString(Time.toMicroseconds time) ^ "\n");
        ()
        )
      end
      
    (*Print the results paired against each other*)
    fun pair_printer g1 g2 t1 t2 =
      let
        val file = File_Aux.append();
      in
        (
        File.writeStr file ("G1: " ^ Int.toString(node_vertex_count g1) ^ " " ^ Int.toString(edge_count g1) ^ " ");
        File.writeStr file ("G2: " ^ Int.toString(node_vertex_count g2) ^ " " ^ Int.toString(edge_count g2) ^ " ");
        File.writeStr file ("T1: " ^ LargeInt.toString(Time.toMicroseconds t1) ^ " ");
        File.writeStr file ("T2: " ^ LargeInt.toString(Time.toMicroseconds t2) ^ "\n");
        ()
        )
      end
  end;
  
  (*This structure contains functions designed to test the speeds of the
  various matching algorithms. Note that none of these functions are run
  unless explicitly called, since these are generally time consuming.
  Therefore, they're not run at compile time!*)
  structure Test_MatchingSpeed =
  struct
    structure Graphs = Test_MatchingSpeedPredfinedGraphs;
    structure Printers = Test_MatchingSpeedPrinters;
    structure File_Aux = Test_MatchingSpeedPrinters.File_Aux;
    
    (*Wrap a sequential function to make it behave more "fairly"*)
    
    (*This function is used to wrap up tests to control the log levels
    inside the matching algorithms and to close the file...
    The flag states whether or not to remove the current contents of the file*)
    fun wrap_test f do_renew =
      let
        (*Remember the current log levels:*)
        val concrete_Log = (!Concrete_M.Log.level_ref);
        val greedy_Log = (!Greedy_M.Log.level_ref);
        val inner_Log = (!Inner_M.Log.level_ref);
        val inner_BG_Log = (!Inner_BG_M.Log.level_ref);
        val concrete_Parallel_Log = (!Concrete_Parallel_M.Log.level_ref);
        val inner_Parallel_Log = (!Inner_Parallel_M.Log.level_ref);
        val greedy_Parallel_Log = (!Greedy_Parallel_M.Log.level_ref);
        val inner_BG_Parallel_Log = (!Inner_BG_Parallel_M.Log.level_ref);
        (*Set the log levels to prevent information being written anywhere*)
        val _ = Concrete_M.Log.level_ref:=(~100000);
        val _ = Greedy_M.Log.level_ref:=(~100000);
        val _ = Inner_M.Log.level_ref:=(~100000);
        val _ = Inner_BG_M.Log.level_ref:=(~100000);
        val _ = Concrete_Parallel_M.Log.level_ref:=(~100000);
        val _ = Inner_Parallel_M.Log.level_ref:=(~100000);
        val _ = Greedy_Parallel_M.Log.level_ref:=(~100000);
        val _ = Inner_BG_Parallel_M.Log.level_ref:=(~100000);
      in
        (
        (*If the flag is true erase the contents of the file*)
        if (do_renew) then File_Aux.renew() else ();
        f (); (*Run the test*)
        File_Aux.close(); (*Close the file*)
        (*Reset the log levels*)
        Concrete_M.Log.level_ref:=concrete_Log;
        Greedy_M.Log.level_ref:=greedy_Log;
        Inner_M.Log.level_ref:=inner_Log;
        Inner_BG_M.Log.level_ref:=inner_BG_Log;
        Inner_Parallel_M.Log.level_ref:=inner_Parallel_Log;
        Concrete_Parallel_M.Log.level_ref:=concrete_Parallel_Log;
        Greedy_Parallel_M.Log.level_ref:=greedy_Parallel_Log;
        Inner_BG_Parallel_M.Log.level_ref:=inner_BG_Parallel_Log;
        ()
        )
      end
    (*
    Run tests with a single fixed size graph against graphs of vertex sizes in the list,
    repeated the specified number of times. (vertex_count,repeat_no)
    Expects a print function too to decide what to do with the result.
    The second to last argument is a graph generation function which should
    accept an integer for the number of vertices.
    The last argument is the matching algorithm function which should
    accept two graphs.
    *)
    fun test_against_generic [] _ _ _ _ = ()
      | test_against_generic ((vn,n)::ps) g p gen match_algo =
        let
          val g2 = gen vn;
          val sw' = Stopwatch.new; (*Start timing*)
          val sw = Stopwatch.start sw';
        in if (n=0) then test_against_generic ps g p gen match_algo else
          (
          match_algo g g2;
          p g g2 (Stopwatch.check sw); (*Print the results...*)
          if (n=1) then test_against_generic ps g p gen match_algo else test_against_generic ((vn,n-1)::ps) g p gen match_algo)
        end
        
    (*Test against both graphs at the same time!*)
    fun test_together [] _ _ _ _ _ = ()
      | test_together ((vn,n)::ps) g p gen match_algo1 match_algo2 =
        if (n=0) then test_together ps g p gen match_algo1 match_algo2 else
        let
          val g2 = gen vn;
          val _ = PolyML.print("Running parallel algorithm");
          val sw = Stopwatch.start Stopwatch.new; (*Start timing*)
          val _ = match_algo1 g g2;
          val t1 = fst (Stopwatch.stop sw);
          val _ = PolyML.print("Finished parallel algorithm");
          val _ = PolyML.print("Running sequential algorithm");
          val sw = Stopwatch.start Stopwatch.new; (*Start timing*)
          val _ = match_algo2 g g2;
          val t2 = fst (Stopwatch.stop sw);
          val _ = PolyML.print("Finished sequential algorithm");
          val _ = p g g2 t1 t2
        in
          if (n=1) then test_together ps g p gen match_algo1 match_algo2 else test_together ((vn,n-1)::ps) g p gen match_algo1 match_algo2
        end
    
    (*Testing the square against random target
    graphs produced over a range of vertices...*)
    fun test_with_square_concrete (low,high) repeat =
      wrap_test (fn() =>
      test_against_generic (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.numbers_printer random_target match_all_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Testing the square against random target
    graphs produced over a range of vertices...*)
    fun test_with_square_concrete_complete (low,high) repeat =
      wrap_test (fn() =>
      test_against_generic (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.numbers_printer complete_target match_all_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Testing the square against random target
    graphs produced over a range of vertices
    PARALLEL version*)
    fun test_with_square_concrete_parallel (low,high) repeat =
      wrap_test (fn() =>
      test_against_generic (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.numbers_printer random_target match_all_parallel_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Testing the square against complete target
    graphs produced over a range of vertices...*)
    fun test_with_square_concrete_complete_parallel (low,high) repeat =
      wrap_test (fn() =>
      test_against_generic (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.numbers_printer complete_target match_all_parallel_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Test the parallel and sequential algorithms on the same random graphs!*)
    fun test_with_square_concrete_together (low,high) repeat =
      wrap_test (fn() =>
      test_together (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.pair_printer random_target match_all_parallel_concrete match_all_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Test the parallel and sequential algorithms together on complete graphs*)
    fun test_with_square_concrete_together_complete (low,high) repeat =
      wrap_test (fn() =>
      test_together (range_fixed_repeats (low,high) repeat) Graphs.concrete_graph1 Printers.pair_printer complete_target match_all_parallel_concrete match_all_concrete)
      true (*Erase the contents of the result file for each test*)
      
    (*Test the parallel and sequential algorithms together
    for graphs consisting of only circles. The integer range supplied
    will be used such that only one graph contains that many circles
    (so the pattern graph is in a sense still fixed)*)
    fun test_with_circles_concrete_together (low,high) repeat = 
      wrap_test (fn() =>
      test_together (range_fixed_repeats (low,high) repeat) Graphs.circle_graph Printers.pair_printer circles_target match_all_circles_parallel_concrete match_all_circles_concrete)
      true (*Erase the contents of the result file for each test*)
    
    (*Test the greedy matching algorithm! Uses the rail graph*)
    fun test_with_rail_greedy_together (low,high) bare_wires repeat =
      wrap_test (fn() =>
      test_together (range_fixed_repeats (low,high) repeat) (add_bare_wires Graphs.pattern_graph3 (bare_wires,bare_wires)) Printers.pair_printer railtrack_target match_all_parallel_greedy match_all_greedy)
      true
      
    (*Test the greedy matching algorithm on its own, with the rail graph*)
    fun test_with_rail_greedy (low,high) bare_wires repeat =
      wrap_test (fn () =>
      test_against_generic (range_fixed_repeats (low,high) repeat) (add_bare_wires Graphs.pattern_graph3 (bare_wires,bare_wires)) Printers.numbers_printer railtrack_target match_all_parallel_greedy)
      true
      
    (*Some issues with the correctness of the matching algorithm have been found... This does a few checks...*)
    fun test_correctness g1 g2 =
      PolyML.print("Number was " ^ Int.toString(
      List.length (Seq.list_of (
      Concrete_M.match g1 g2))))
      
    (*REMOVE*)
    fun test hardness = test_with_square_concrete_together_complete (hardness,hardness) 1
    fun test_parallel hardness = test_with_square_concrete_complete_parallel (hardness,hardness) 1
    (*REMOVE*)
    fun test_greedy_together (low,high) bare_wires = test_with_rail_greedy_together (low,high) bare_wires 1
    fun test_greedy hardness bare_wires = test_with_rail_greedy (hardness,hardness) bare_wires 1
    
    (*REMOVE*)
    val complete_target = complete_target
    (*REMOVE*)
    fun run() = let
    val _ = Concrete_M.Log.level_ref:=(~100000);
        val _ = Greedy_M.Log.level_ref:=(~100000);
        val _ = Inner_M.Log.level_ref:=(~100000);
        val _ = Inner_BG_M.Log.level_ref:=(~100000);
        val _ = Concrete_Parallel_M.Log.level_ref:=(~100000);
        val _ = Inner_Parallel_M.Log.level_ref:=(~100000);
        val _ = Greedy_Parallel_M.Log.level_ref:=(~100000);
        val _ = Inner_BG_Parallel_M.Log.level_ref:=(~100000);
    in
    test_correctness Graphs.concrete_graph1 (complete_target 4) end
  end;
end;
