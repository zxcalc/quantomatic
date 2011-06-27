signature EQ_CLASS =
sig
  type 'a T = {
        rep : 'a option,
      congs : 'a list,
    redexes : 'a list }
  val get_rep : 'a T -> 'a option
  val get_congs : 'a T -> 'a list
  val get_redexes : 'a T -> 'a list
  val update_rep : ('a option -> 'a option) -> 'a T -> 'a T
  val update_congs : ('a list -> 'a list) -> 'a T -> 'a T
  val update_redexes : ('a list -> 'a list) -> 'a T -> 'a T
  val set_rep : 'a option -> 'a T -> 'a T
  val set_congs : 'a list -> 'a T -> 'a T
  val set_redexes : 'a list -> 'a T -> 'a T
  val empty : 'a T
end

structure EqClass : EQ_CLASS =
struct
  type 'a T = {
        rep : 'a option,
      congs : 'a list,
    redexes : 'a list }
  fun get_rep ({rep=x,...}:'a T) = x
  fun get_congs ({congs=x,...}:'a T) = x
  fun get_redexes ({redexes=x,...}:'a T) = x
  fun update_rep f ({rep=rep,congs=congs,redexes=redexes}:'a T) = {rep=f rep,congs=congs,redexes=redexes}
  fun update_congs f ({rep=rep,congs=congs,redexes=redexes}:'a T) = {rep=rep,congs=f congs,redexes=redexes}
  fun update_redexes f ({rep=rep,congs=congs,redexes=redexes}:'a T) = {rep=rep,congs=congs,redexes=f redexes}
  fun set_rep x cl = update_rep (K x) cl
  fun set_congs x cl = update_congs (K x) cl
  fun set_redexes x cl = update_redexes (K x) cl
  
  val empty = { rep = NONE, congs = [], redexes = [] }
end

signature SYNTHESIZE =
sig
  structure Graph : BANG_GRAPH
  structure GraphEnum : GRAPH_ENUMERATOR
  structure Metric : GRAPH_METRIC
  structure TData : TENSOR_DATA
  sharing Graph.SharingBangGraph = Metric.Graph.SharingBangGraph = GraphEnum.Graph.SharingBangGraph
  type rundesc = int * int * int * int
  type T = rundesc * (Graph.T EqClass.T) TData.Tensor.Tab.table
  val synth : (Graph.T * int * int) list -> int * int * int * int -> T
  val synth_with_rs : GraphEnum.Ruleset.T -> (Graph.T * int * int) list -> int * int * int * int -> T
  val stats : T -> int * int * int
  val eqclass_fold : (TData.Tensor.T * Graph.T EqClass.T -> 'b -> 'b) -> T -> 'b -> 'b
end

functor Synthesize (
  structure GraphEnum : GRAPH_ENUMERATOR
  structure Metric : GRAPH_METRIC
  structure TData : TENSOR_DATA
  sharing GraphEnum.Graph.SharingBangGraph =
          Metric.Graph.SharingBangGraph =
          TData.Graph.SharingBangGraph
) : SYNTHESIZE =
struct
  structure TData = TData
  structure TEval = TensorEval(structure TData=TData)
  structure GraphEnum = GraphEnum
  structure Graph = GraphEnum.Graph
  structure Metric = Metric
  structure Tensor = TData.Tensor
  
  type rundesc = int * int * int * int
  type T = rundesc * (Graph.T EqClass.T) Tensor.Tab.table
  
  
  fun classify gr class_table = let
    val (ins,outs,tens) = TEval.tensor_for_graph gr
    val ((scalar,perm_outs,perm_ins),tens') = Tensor.normalise tens
    val norm_ins = map (fn n => Graph.Vertex.mk ("i_"^(Int.toString n))) perm_ins
    val norm_outs = map (fn n => Graph.Vertex.mk ("o_"^(Int.toString n))) perm_outs
    val gr' = fold2 Graph.rename_vname ins norm_ins (fold2 Graph.rename_vname outs norm_outs gr)
  in class_table |> Tensor.Tab.update (tens',
      (case Tensor.Tab.lookup class_table tens'
         of SOME eq => eq | NONE => EqClass.empty)
       |> EqClass.update_congs (cons gr'))
  end
  
  fun filter_class class = let
    fun f graph (min,list) = let
      val metric = Metric.compute graph
    in case min of SOME (min_met, min_graph) =>
                     if Metric.ord (metric, min_met) = LESS
                      then (SOME (metric, graph), (min_met, min_graph)::list)
                      else (SOME (min_met, min_graph), (metric, graph)::list)
                 | NONE => (SOME (metric, graph), list)
    end
    val (min_met, min_graph, metrics) = case fold f (EqClass.get_congs class) (NONE, [])
                                          of (NONE, ms) => (Metric.bottom, Graph.empty, ms)
                                           | (SOME (mm,mg), ms) => (mm,mg,ms)
    val congs   = fold (fn (m,g) => fn lst =>
                        if Metric.ord (m, min_met) = EQUAL then g::lst else lst) metrics []
    val redexes = fold (fn (m,g) => fn lst =>
                        if Metric.ord (m, min_met) = GREATER then g::lst else lst) metrics []
  in class
      |> EqClass.set_rep (SOME min_graph)
      |> EqClass.set_congs congs
      |> EqClass.set_redexes redexes
  end
  
  fun synth_with_rs rs gens (rd as (ins,outs,verts,plugs)) = (rd,
    Tensor.Tab.map (K filter_class)
                   (GraphEnum.fold_graphs_with_rs
                      rs classify (gens,ins,outs,verts,plugs) Tensor.Tab.empty))
  
  val synth = synth_with_rs GraphEnum.Ruleset.empty
  
  fun stats (_,class_tab) =
    Tensor.Tab.fold (fn (_,class) => fn (i,j,k) =>
                       (i + 1,
                        j + length (EqClass.get_congs class),
                        k + length (EqClass.get_redexes class))
                    ) class_tab (0,0,0)
  
  fun eqclass_fold f (_,tab) base = TData.Tensor.Tab.fold f tab base
end
  