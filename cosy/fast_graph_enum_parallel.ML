signature PARALLEL_GRAPH_ENUM =
sig
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure EqClassTab : EQ_CLASS_TAB
  
  type data
  (*Corresponds to a single vertex with a certain number of inputs outputs
  (data relevant to theory,input,output)
  We stick the generators together to form graphs (all possible)*)
  type generator = (Theory.Graph.VData.data * int * int)
  
  val default_data : data
  type size_param = int * int * int * int
  
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph
  sharing Theory.Ruleset.Sharing =
          EqClassTab.Theory.Ruleset.Sharing
  
  val tab_update : generator list -> size_param -> EqClassTab.T -> EqClassTab.T
  val tab_enum : generator list -> size_param -> EqClassTab.T
end

functor ParallelFastGraphEnum(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure Equiv : GRAPH_EQUIV
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          Equiv.Graph.SharingOGraph
) : PARALLEL_GRAPH_ENUM =
struct
  structure Theory = Theory
  structure Metric = Metric
  structure Graph = Theory.Graph
  (*Import all of the parallel structures*)
  structure Par_Seq = Safe_Parallel_Seq;
  structure Future = Future_Wrapper;
  
  type size_param = int * int * int * int
  type generator = (Theory.Graph.VData.data * int * int) (* generators (data + arities) *)
  (* Ordered Vertices, free inputs, free outputs, list of lists of
     edges, one vertex at a time *)
  (*generator list
  inputs left on each generator
  outputs left on each generator
  Number of edges connecting source to target (directed - multiple)*)
  type adjmat = generator list * int list * int list * (int list) list 
  type data = adjmat * int
  val empty_graph_rep = (([],[],[],[]),0)
  val default_data = empty_graph_rep
  
  fun generator_eq ((data1,i1,o1), (data2,i2,o2)) =
    Theory.Graph.VData.data_eq (data1, data2) andalso
    (i1,o1) = (i2,o2) (*Generator of graphs is the same*)
  
  structure EqClassTab = ParallelEqClassTab(
    structure Theory = Theory
    structure Metric = Metric
    structure Equiv = Equiv
    type data = data
    val default_data = default_data)
  structure GraphEntry = EqClassTab.GraphEntry

  (****** Utility Functions ******)

  fun drop_while _ [] = []
    | drop_while p (x::xs) = 
      if p x then drop_while p xs 
      else x::xs

  fun keep_drop 0 0 ts = ts
    | keep_drop 0 j (t::ts) = keep_drop 0 (j-1) ts
    | keep_drop i j (t::ts) = t::(keep_drop (i-1) j ts)

  fun filter_max m zs = let
    fun filter_max' m (x, (n,ys)) = 
      case (Int.compare (m x, n)) of 
        LESS => (n,ys)
      | GREATER => ((m x),x::[])
      | EQUAL => (n,x::ys) 
  in snd (List.foldl (filter_max' m) (0,[]) zs)
  end

  val sum = List.foldr (fn (x,y) => x+y) 0


  (****** adjmat functions ******)

  (* Number of: vertices, total inputs, total outputs *)
  fun adjmat_total_arities (gs,_,_,_) = let
    val m = sum (map (fn (_,i,_) => i) gs)
    val n = sum (map (fn (_,_,j) => j) gs)
  in (m, n)
  end

  (* Number of: vertices, total inputs, total outputs *)
  fun adjmat_arity (g as (gs,_,_,edges)) = let
    val p = sum (map sum edges)
    val (m,n) = adjmat_total_arities g
  in (m-p, n-p)
  end

  (* Number of: vertices, total inputs, total outputs *)
  fun adjmat_pluggings (g as (_,_,_,edges)) = sum (map sum edges)



  fun split_edge [] = ([],[])
    | split_edge xs = let 
      fun split_edge' xs ys 0 = (xs,ys)
        | split_edge' (x::xs) ys n = split_edge' xs (x::ys) (n-1) 
    in split_edge' xs [] ((length xs - 1) div 2)
    end

  val to_block_matr = let
    fun switch xs yss = let 
      val (cs,rs) = split_edge xs
    in ListPair.map (fn (y, ys) => y::ys) (cs, rs::yss)
    end
  in List.foldr (uncurry switch) []
  end

  fun from_block_matr [] = []
    | from_block_matr (es::ess) = 
      (List.revAppend (map hd ess, es)) :: from_block_matr (map tl ess)

  val order = List.foldl (List.revAppend) []

  fun swap_adj_matr i tss = let 
    fun swap_adj_list 1 (t1::t2::ts) = t2::t1::ts
      | swap_adj_list i (t::ts) = t::(swap_adj_list (i-1) ts)
      | swap_adj_list _ ts = ts
  in swap_adj_list i (map (swap_adj_list i) tss)
  end

  fun perm_matr tss = let
    fun shift_matr 0 tsss = tsss
      | shift_matr i tsss = tsss @ (shift_matr (i-1) (map (swap_adj_matr i) tsss))
    fun perm_matr' tss 0 = [tss]
      | perm_matr' tss i = shift_matr i (perm_matr' tss (i-1))
  in perm_matr' tss (length tss-1)
  end

  fun matr_canonical ess = let
    val cur_order = order ess 
  in forall
    ((fn x => (list_ord int_ord (x, cur_order) <> GREATER)) o
               order o from_block_matr)
    (perm_matr (to_block_matr ess))
  end

  fun compare_vert ts us = let
    val k = (length ts - 1) div 2
    val d = (length us - 1) div 2 - k
    val ts' = (keep_drop (k+1) d (keep_drop k d us))
  in list_ord int_ord (ts, ts')
  end
 
  fun num_same_vert xss = let 
    fun num_same_vert' i (v::[]) = i
      | num_same_vert' i (v1::v2::vs) = 
      case (compare_vert v1 v2) of 
        LESS => 0
      | GREATER => num_same_vert' 1 (v2::vs)
      | EQUAL => num_same_vert' (i+1) (v1::vs)
  in num_same_vert' 1 xss
  end
  
  fun centre [] = []
    | centre (t::ts) = let
      fun centre' x i [] = []
        | centre' x i (t::ts) = take i (drop x t) :: (centre' x (i+2) ts)
    in centre' ((length t - 1) div 2) 1 (t::ts)
    end

  fun edges_canonical [] = true
    | edges_canonical ts = let 
        val k = num_same_vert ts
        val vs = drop (length ts - k) ts
    in (matr_canonical o rev o centre) vs
    end

  fun last_vert_canonical ([], _, _, _) = true
    | last_vert_canonical (vs, _, _, ess) = let 
      fun last_vert (x::y::xs) (es1::ess) ls = if (x=y) then last_vert (x::xs) ess (es1::ls) else es1::ls
        | last_vert (x::_) (es1::_) ls = es1::ls
    in edges_canonical (last_vert vs ess [])
    end

  fun to_graph_idfree (vs,is,os,ess) = let
    val g0 = Graph.empty
    fun to_graph_vert [] g = ([],g)
      | to_graph_vert ((v,_,_)::vs) g = let
        val (vs', g') = to_graph_vert vs g
        val (v', g'') = Graph.add_vertex v g'
      in (v'::vs', g'')
      end
    val (vertices, g0) = to_graph_vert vs g0
    fun into_edges [] (_, []) = []
      | into_edges (vin::vins) (vout, (n::ns)) = List.tabulate (n, K (vout,vin)) @ (into_edges vins (vout, ns))
    fun list_int_edges vs ess = maps (into_edges vs) (ListPair.zip (vs, ess))
    val edge_list = list_int_edges vertices (to_block_matr ess)
    val g0 = List.foldr (fn ((x,y), g) => snd (g |> Graph.add_edge (Graph.Directed, Theory.EData.default_data) x y)) g0 edge_list
    val edge_in_list = maps (fn (n,y) => List.tabulate (n, K y)) (ListPair.zip (is, vertices))
    val edge_out_list = maps (fn (n,y) => List.tabulate (n, K y)) (ListPair.zip (os, vertices))
    fun add_in_edge (v, g) = let
      val (bound, g') = g |> Graph.add_vertex (Theory.OVData.WVert);
      val (_,g'') = g' |> Graph.add_edge (Graph.Directed, Theory.EData.default_data) bound v
    in g''
    end
    fun add_out_edge (v, g) = let
      val (bound, g') = g |> Graph.add_vertex (Theory.OVData.WVert);
      val (_,g'') = g' |> Graph.add_edge (Graph.Directed, Theory.EData.default_data) v bound
    in g''
    end  
    val g0 = List.foldr add_in_edge g0 edge_in_list
    val g0 = List.foldr add_out_edge g0 edge_out_list
  in g0
  end

  fun add_ident_wire g = let 
      val (x, g) = g |> Graph.add_vertex (Theory.OVData.WVert);
      val (y, g) = g |> Graph.add_vertex (Theory.OVData.WVert);
      val (_, g) = g |> Graph.add_edge (Graph.Directed, Theory.EData.default_data) x y
  in g
  end

  fun to_graph (am,i) = funpow i add_ident_wire (to_graph_idfree am)

  fun to_graph_entry ami = GraphEntry.mk (to_graph ami, ami)


  (****** Building adjmat graphs ******)
  
  (*Adds this generator to the existing list of generators.
  Adds the desired input to the inputs and the same for the output
  The last part figures out all possible combinations that this edge could be added.
  
  Note: List.tabulate ((2 * length (verts) +1), K 0) returns [0,0,0,0...] Hmm...
  => 0, 1, 3, (2*3 + 1) = 7, etc...
  *)
  (* adds a specific type of vertex with no edges *)
  (* add_gen : generator -> adjmat -> adjmat *)
  (*Add 0s to show no inputs or outputs used on new edge of matrix*)
  fun add_gen (v' as (_,inp,out)) (verts, inps, outs, edges) = (v'::verts, inp::inps, out::outs, (List.tabulate ((2 * length (verts) +1), K 0)::edges))

  (* adds any possible vertex with no edges *)
  (* add_gens : [generator] -> adjmat -> adjmat list *)
  fun add_gens (max_p,max_m,max_n) gens (matr as ([],_,_,_)) = let
      val gens = filter (fn (_,m,n) => m <= max_m + max_p andalso n <= max_n + max_p) gens
    in map (fn v => add_gen v matr) gens
    end
    | add_gens (max_p,max_m,max_n) gens (matr as (vert::_,_,_,_)) = let
      fun is_small (_,gm,gn) = let
        val (m,n) = adjmat_total_arities matr
      in (m+gm <= max_m + max_p) andalso (n+gn <= max_n + max_p)
      end
      (*Strip away inappropriate generators*)
      val gens = drop_while (fn g => not (generator_eq (g,vert))) gens
      val gens = filter is_small gens
  in map (fn v => add_gen v matr) gens (*Add the applicable generators*)
  end
  (* adds any possible edge *)
  (* add_edge : adjmat -> adjmat list *)
  fun add_edge ([], _, _, _) = [] (*No generators - no nodes to add*)
    | add_edge (verts, inps, outs, edge::edges) = let
      (*i2 is the number of inputs for this particular vertex, and e2 is the number of edges*)
      (*See diagram for lexicographic ordering - we look for non-zero to add to*)
      fun new_edge_here (i2::is2) (o2::os2) (e2::es2) = if ((i2<>0) andalso (o2<>0)) (*Deduct from the difference in the table*)
                                                        then [(i2-1::is2, o2-1::os2, e2+1::es2)] else []
      fun add_edge' [i1] [o1] es = new_edge_here [i1] [o1] es (*Row of inputs finishes - switch to right column of outputs (going up)*)
        | add_edge' (i1::[]) (o1::os) (e1::es) = new_edge_here (i1::[]) (o1::os) (e1::es) @ 
                                                map (fn (x,y,z) => (x,o1::y,0::z)) (if (e1=0) then add_edge' [i1] os es else [] )
        | add_edge' (i1::is) (o1::os) (e1::es) = new_edge_here (i1::is) (o1::os) (e1::es) @ 
                                                map (fn (x,y,z) => (i1::x,y,0::z)) (if (e1=0) then add_edge' is (o1::os) es else [] )
  in map (fn (x,y,z) => (verts,rev x,y,z::edges)) (add_edge' (rev inps) outs edge)
  end

  (*The default compactor used within the parallel code*)
  fun default_compactor() =(*Compactor.do_not_prime ( *)  Compactor.prime ( 
    Compactor.set_estimated_number (
    Compactor.new_dynamic (
      Time.fromMicroseconds 20000)) (*Time to aim for*)
      (SOME 1))(* ) (*Initial estimate*) *)
      (SOME 15) false; (*Prime length + do/don't go static*)
  
  (****** Enumeration with EqClassTab ******)
  
  (* TODO: replace fopt with (f, filt)
  At the moment "opt" is EqClassTab.update_one*)
  fun fold_graphs gens (max_v,max_p,max_m,max_n) (fopt,filt,fclean,fmerge) initial = let
    fun save (fopt,filt) xs eq_tab =
      let
        val disable = true;
        (*Firstly, declare the original equivalence class table as clean (This is O(1) at the moment)*)
        val eq_tab = fclean eq_tab;
        (*Would like to check that this is where communication with the equivalence
        class table is really happening - in other words that the list of graphs
        is large enough...*)
        val _ = if (disable) then () else (PolyML.print("Processing " ^ (Int.toString(List.length xs)) ^ " graphs next. Using " ^ 
          (Int.toString (Multithreading.max_threads_value())) ^ " threads.");())
        (*Firstly, split the incoming graphs into two halves - those which are too large to be added
        to the equivalence class table, and those which might be added to the equivalence class table...*)
        fun size_check (inputs,outputs) bare_wires =
          ((inputs + bare_wires) <= max_m) andalso (outputs + bare_wires <= max_n);
        (*Perform the check to see if a particular "x" might be added to the table*)
        fun perform_size_check x = size_check (adjmat_arity (fst x)) (snd x);
        (*Split a list into two "halves" according to the filter condition. Left is for "true"
        while right is for "false"*)
        fun split_filter p [] (ts,fs) = (ts,fs)
          | split_filter p (x::xs) (ts,fs) = if (p x) then split_filter p xs (x::ts,fs)
            else split_filter p xs (ts,x::fs);
        (*Now filter the list of incoming graphs for those which might be added later, and those
        which certainly won't be...*)
        val (to_try_adding,not_to_add) = split_filter perform_size_check xs ([],[]);
        (*val _ = PolyML.print("Performed size check");
        val _ = PolyML.print("Will try adding " ^ (Int.toString (List.length to_try_adding)));*)
        (*to_try_adding contains all of the graphs that need to attempt being added to the equivalence
        class table!!!!*)
        
        (*****************************************************************************************)
        (***************************Pumping graphs through the DNET*******************************)
        (*****************************************************************************************)
        (*Filter out all the graphs which are not allowed to be added to the equivalence class
        table according to its existing DNET...*)
        
        fun check_graph_against_DNET x = filt eq_tab (to_graph_entry x)
        (*val _ = PolyML.print("Performing filtering...");*)
        (*Filter through all of the graphs*)
        val compactor = default_compactor();
        fun to_filter x = if (check_graph_against_DNET x) then NONE else SOME x;
        (*Group the tasks...*)
        val groups = Compactor.compact_map_list compactor to_filter to_try_adding;
        val fd = Future.new_future_data (SOME "Passing new graphs throught the DNET");
        val par_seq = Par_Seq.of_sequential_seq fd groups; (*Sticking to the default*)
        (*Pump elements through the DNET in parallel*)
        val _ = if (disable) then () else (PolyML.print("Pumping through DNET...");())
        val res_list = Par_Seq.list_of (Par_Seq.map (fn _ => fn f => f()) par_seq);
        val _ = if (disable) then () else (PolyML.print("Through DNET");())
        val res_list = flat res_list
        fun remove_filters [] = []
          | remove_filters (NONE::xs) = remove_filters xs
          | remove_filters ((SOME x)::xs) = x::(remove_filters xs);
        (*passed through DNET now contains all of the graphs which passed through
        the equivalence class's original DNET (so graphs were not added to modify the DNET
        during this check*)
        val passed_through_DNET = remove_filters res_list;
        (*val _ = PolyML.print("Pumped through DNET. Got " ^ (Int.toString(List.length passed_through_DNET)));*)
        (*Now, all graphs within "passed_through_DNET" need to be added to the equivalence
        class table, as unless they reduce to each other, they will be added...*)
        val add_to_eq_tab = fopt; (*For emphasis*)
        (*Now I need to add each graph to its own unique equivalence class table and then merge them
        In theory this is quick since I know they pass the DNET by this point. Hence, parallelisation here
        is probably unnecessary...*)
        fun add_to_own_eq_tab eq_tab x = add_to_eq_tab (to_graph_entry x) eq_tab;
        val compactor = default_compactor();
        val groups = Compactor.compact_map_list compactor (add_to_own_eq_tab eq_tab) passed_through_DNET;
        val fd = Future.new_future_data (SOME "Forming unique equivalence class tables");
        val par_seq = Par_Seq.of_sequential_seq fd groups; (*Sticking to default*)
        val _ = if (disable) then () else (PolyML.print("Constructing unique tables");())
        val res_list = flat (Par_Seq.list_of (Par_Seq.map (fn _ => fn f => f()) par_seq));
        val _ = if (disable) then () else (PolyML.print("Done constructing unique tables");())
        (*All should have been added successfully from this list*)
        (*val _ = PolyML.print("Created all of the new equivalence class tables before merging...");*)
        val eq_tab_list = map the res_list;
        (*val _ = PolyML.print("I have " ^ (Int.toString (List.length eq_tab_list)) ^ " equivalence class tables");*)
        (*The merge function itself...*)
        fun merge x y = fmerge x y (*Uses the merge function within the equivalence class table to make sure that this is efficient*)
        (*Now eq_tab list contains all of the equivalence classes that need to be merged! Now I have to perform
        the merging in parallel...*)
        (*Take a list and form the functions ready to merge all pairs...*)
        fun merge_partition [] = []
          | merge_partition [x] = (*No one to merge with ): *) [fn () => x]
          | merge_partition (x::y::xs) = (fn () => merge x y)::(merge_partition xs)
        (*partition the list, and then merge in parallel...*)
        fun merge_list_once xs =
          let
            val pairs = merge_partition xs;
            (*Choose the compactor etc...*)
            val compactor = default_compactor();
            val groups = Compactor.compact_map_list compactor (fn f => f()) pairs;
            val fd = Future.new_future_data (SOME "Merging pairs of equivalence class tables");
            val par_seq = Par_Seq.of_sequential_seq fd groups;
            val res_list = flat (Par_Seq.list_of (Par_Seq.map (fn _ => fn f => f()) par_seq));
          in
            res_list
          end
        (*Keeping merging until only one equivalence class table remains...*)
        fun merge_list_all [] = eq_tab (*Nothing was actually added...*)
          | merge_list_all [x] = x
          | merge_list_all xs = merge_list_all (merge_list_once xs);
        (*Perform the merge!*)
        val _ = if (disable) then () else (PolyML.print("Performing merge");())
        val new_eq_tab = merge_list_all eq_tab_list;
        val _ = if (disable) then () else (PolyML.print("Performed entire merge");())
        (*SANITY CHECK*)
        (*
        fun add_all_to_tab [] (succeeded,eq_tab) = (succeeded,eq_tab)
          | add_all_to_tab (x::xs) (succeeded,eq_tab) = case (add_to_eq_tab (to_graph_entry x) eq_tab) of
              SOME eq_tab' => add_all_to_tab xs (x::succeeded,eq_tab')
            | NONE => add_all_to_tab xs (succeeded,eq_tab)
        (*Finish by adding the graphs back in which were too large...*)
        val (were_added,new_eq_tab) = add_all_to_tab passed_through_DNET ([],eq_tab);*)
        val were_added = passed_through_DNET; (*"SHOULD" BE TRUE possibly... otherwise need to pay attention during merging
        Requires implementation of mysterious merge function*)
      in
        (were_added @ not_to_add,new_eq_tab)
      end
      (* add edges in all possible ways *)
    fun fold_edges_round [] (next, accum) = (next, accum)
      | fold_edges_round egs (next, accum) =
    let
        (*next' includes all of the irreducible graphs and some other graphs which are
        too large at the moment, but that you might want to add in the future...
        0 would appear to be the number of bare wires*)
        val (next',accum') = save (fopt,filt) (map (fn eg => (eg,0)) egs) accum
        (*Check that adding the additional edge did not go above the number of pluggings*)
        val egs' = filter (fn (g,_) => (adjmat_pluggings g) < max_p) next'
        (*egs' contains all graphs that were irreducible from the previous run, with edges added
        to them, and possibly some graphs which were never added to the equivalence class table,
        but might be added in the future...*)
        val egs' = maps (add_edge o fst) egs'
    in fold_edges_round (egs') (next' @ next, accum')
    end
    (***)
    (* one round of enumeration. (prev, accum) => add one vertex => (next, accum')
    Accum is an accumulator which is the equivalence class table at the moment.*)
    fun fold_graphs_round (prev, accum) =
    let
      (* start by adding one bare wire to the output of the previous round
      If the graph ends up have too many inputs or outputs, remove it.*)
      val (prev_with_bare, accum') =
        save (fopt,filt) (map (fn (am,i) => (am,i+1)) (filter (fn (am,i) => (fst (adjmat_arity am) + i < max_m ) andalso (snd (adjmat_arity am) + i < max_n )) prev)) accum  
      (* only add non-trivial generators to graphs that don't have any bare wires yet *)
      val prev = map fst (filter (fn (_,i)=> i = 0) prev)
      (*Hoping this is where most of the work is at the moment...*)
      (*Set the compactor to use*)
      val compactor = default_compactor();
      val fd = Future.new_future_data (SOME "Enumerating next graphs");
      (*Create the compacted elements...*)
      val groups = Compactor.compact_map_list compactor (add_gens (max_p,max_m,max_n) gens) prev;
      (*Set the parallel lazy sequence to use*)
      val par_seq = Par_Seq.of_sequential_seq fd groups; (*Sticking to the default*)
      val par_seq = flat (Par_Seq.list_of (Par_Seq.map (fn _ => fn f => f()) par_seq));
      (*This is the list of graphs to be added to the equivalence class table*)
      val graphs = flat par_seq;
    in fold (fn g => fold_edges_round [g]) graphs (prev_with_bare, accum')
    end
    (***)
  in
    (*Apply fold_graphs_round max_v times to the graphs...*)
    snd (funpow max_v fold_graphs_round ([empty_graph_rep], initial))
  end
  
  (*This repeatedly adds graphs to the equivalence class table, but one at a time...*)
  fun tab_update gens max_size = fold_graphs gens max_size (EqClassTab.hard_update_one, (*Add a single graph to the equivalence class table*)
    EqClassTab.has_match_in_dnet, (*Check to see if a match in the dnet exists*)
    EqClassTab.clean, (*Declare that a round of modifications to the equivalence class table has been completed*)
    fn eqt1 => fn eqt2 => snd (EqClassTab.merge eqt1 eqt2)) (*Merge two equivalence class tables*)
  (*Enumerate the equivalence class table up to graphs with the specified
  maximum size (which specifies the maximum number of inputs, outputs, vertices
  and the maximum number of pluggings
  Gens is the list of initial tables that the enumerator uses.
  Seems a little odd that the equivalence class.
  Will be passed an initial ruleset too*)
  fun tab_enum gens max_size = tab_update gens max_size EqClassTab.empty
end
