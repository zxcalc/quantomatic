signature GRAPH_ENUM =
sig
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure EqClassTab : EQ_CLASS_TAB
  
  type data
  type generator = (Theory.Graph.VData.data * int * int)
  
  val default_data : data
  
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph
  sharing Theory.Ruleset.Sharing =
          EqClassTab.Theory.Ruleset.Sharing
  
  val tab_update : generator list -> int -> EqClassTab.T -> EqClassTab.T
  val tab_enum : generator list -> int -> EqClassTab.T
  val enum : generator list -> int -> Theory.Graph.T list
end

functor FastGraphEnum(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure Equiv : GRAPH_EQUIV
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          Equiv.Graph.SharingOGraph
) : GRAPH_ENUM =
struct
  structure Theory = Theory
  structure Metric = Metric
  structure Graph = Theory.Graph

  type generator = (Theory.Graph.VData.data * int * int) (* generators (data + arities) *)
  (* Ordered Vertices, free inputs, free outputs, list of lists of
     edges, one vertex at a time *)
  type adjmat = generator list * int list * int list * (int list) list 
  type data = adjmat * int
  val empty_graph_rep = (([],[],[],[]),0)
  val default_data = empty_graph_rep
  
  fun generator_eq ((data1,i1,o1), (data2,i2,o2)) =
    Theory.Graph.VData.data_eq (data1, data2) andalso
    (i1,o1) = (i2,o2)
  
  structure EqClassTab = EqClassTab(
    structure Theory = Theory
    structure Metric = Metric
    structure Equiv = Equiv
    type data = data)
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


  (****** adjmat functions ******)

  fun adjmat_size (gs,_,_,_) = length gs

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

 (* adds a specific type of vertex with no edges *)
  (* add_gen : generator -> adjmat -> adjmat *) 
  fun add_gen (v' as (_,inp,out)) (verts, inps, outs, edges) = (v'::verts, inp::inps, out::outs, (List.tabulate ((2 * length (verts) +1), K 0)::edges))

  (* adds any possible vertex with no edges *)
  (* add_gens : [generator] -> adjmat -> adjmat list *)
  fun add_gens gens (matr as ([],_,_,_)) = map (fn v => add_gen v matr) gens
    | add_gens gens (matr as (vert::_,_,_,_)) = let
      val gens' = drop_while (fn g => not (generator_eq (g,vert))) gens
    in map (fn v => add_gen v matr) gens'
    end
  
  (* adds any possible edge *)
  (* add_edge : adjmat -> adjmat list *)
  fun add_edge ([], _, _, _) = []
    | add_edge (verts, inps, outs, edge::edges) = let 
      fun new_edge_here (i2::is2) (o2::os2) (e2::es2) = if ((i2<>0) andalso (o2<>0)) 
                                                        then [(i2-1::is2, o2-1::os2, e2+1::es2)] else []
      fun add_edge' [i1] [o1] es = new_edge_here [i1] [o1] es
        | add_edge' (i1::[]) (o1::os) (e1::es) = new_edge_here (i1::[]) (o1::os) (e1::es) @ 
                                                map (fn (x,y,z) => (x,o1::y,0::z)) (if (e1=0) then add_edge' [i1] os es else [] )
        | add_edge' (i1::is) (o1::os) (e1::es) = new_edge_here (i1::is) (o1::os) (e1::es) @ 
                                                map (fn (x,y,z) => (i1::x,y,0::z)) (if (e1=0) then add_edge' is (o1::os) es else [] )
    in map (fn (x,y,z) => (verts,rev x,y,z::edges)) (add_edge' (rev inps) outs edge)
    end


  (****** Enumeration with EqClassTab ******)
  
  local
    fun fold_save fopt (x::xs) (ys,accum) = (
      case fopt x accum
        of SOME accum' => fold_save fopt xs (x::ys, accum')
         | NONE        => fold_save fopt xs (ys,accum))
      | fold_save fopt [] (ys,accum) = (ys, accum)
  in
    fun fold_graphs gens max_verts fopt initial =
    let
      (***)
      (* add edges in all possible ways *)
      fun fold_edges_round [] (prev, accum) = (prev, accum)
        | fold_edges_round egs (prev, accum) =
      let
          val egs' = maps add_edge egs
          val (next,accum') = fold_save (fopt o to_graph_entry) (map (fn eg => (eg,0)) egs') ([],accum)
      in fold_edges_round (map fst next) (next @ prev,accum')
      end
      (***)
      (* one round of enumeration. (prev, accum) => add one vertex => (next, accum') *)
      fun fold_graphs_round (prev, accum) =
      let
        (* start by adding one bare wire to the output of the previous round *)
        val (next, accum') = fold_save (fopt o to_graph_entry) (map (fn (am,i) => (am,i+1)) prev) ([], accum)
        (* only add non-trivial generators to graphs that don't have any bare wires yet *)
        val prev = map fst (filter (fn (_,i)=> i = 0) prev)
        val graphs = flat (map (add_gens gens) prev)
      in fold (fn g => fold_edges_round [g]) graphs (next, accum')
      end
      (***)
    in
      snd (funpow max_verts fold_graphs_round ([empty_graph_rep], initial))
    end
  end
  
  fun enum gens max_verts =
    fold_graphs gens max_verts
    (fn g => fn gs => SOME (GraphEntry.get_graph g :: gs)) []
  
  fun tab_update gens max_verts = fold_graphs gens max_verts EqClassTab.update_one
  fun tab_enum gens max_verts = tab_update gens max_verts EqClassTab.empty

  (* recursively adds new graphs to tab by adding one new edge to each irredex *)
  (* tab_add_edges : [adjmat] -> EqClassTab -> EqClassTab *)
  fun tab_add_edges [] tab = tab
    | tab_add_edges egs tab = let
      val egs' = maps add_edge egs 
      val (irreds,tab') = EqClassTab.update (map (fn eg => to_graph_entry (eg,0)) egs') tab
    in tab_add_edges (map (fst o GraphEntry.get_data) irreds) tab'
    end

  (* adds new graphs with one new vertex and any possible number of extra edges *)
  (* tab_add_gens : generator list -> EqClassTab -> EqClassTab *)
  fun tab_add_gens gens tab = let
    val irred_graphs = filter_max (fn (g,i) => i + adjmat_size g)
                      (map GraphEntry.get_data (EqClassTab.get_irredexes tab))
    val (_,tab') = EqClassTab.update (map (fn (am,i) => to_graph_entry (am,i+1)) irred_graphs) tab
    val irred_graphs = map fst (filter (fn (g,i) => i=0) irred_graphs)
    val graphs = flat (map (add_gens gens) irred_graphs)
  in List.foldl (fn (g,t) => tab_add_edges [g] t) tab' graphs
  end

  
  (*fun tab_update gens max_verts tab =
    funpow max_verts (tab_add_gens gens)
    (snd (EqClassTab.update [to_graph_entry (([],[],[],[]),0)] tab))
  
  (* Creates a table of graphs up to a certain size *)
  (* tab_enum : generator list -> Int -> EqClassTab *)
  fun tab_enum gens max_verts = EqClassTab.empty |> tab_update gens max_verts*)
end