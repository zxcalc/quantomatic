signature GRAPH_ENTRY =
sig
  structure Graph : BANG_GRAPH
  type T
  type data
  val update_graph : (Graph.T -> Graph.T) -> T -> T
  val update_dirty : (bool -> bool) -> T -> T
  val update_data  : (data -> data) -> T -> T
  val get_graph    : T -> Graph.T
  val get_dirty    : T -> bool
  val get_data     : T -> data
  val set_graph    : Graph.T -> T -> T
  val set_dirty    : bool -> T -> T
  val set_data     : data -> T -> T
  val mk : Graph.T * data -> T
end

signature EQ_CLASS =
sig
  type T
  structure GraphEntry : GRAPH_ENTRY
  val get_rep : T -> GraphEntry.T option
  val get_congs : T -> GraphEntry.T list
  val get_redexes : T -> GraphEntry.T list
  val update_rep : (GraphEntry.T option -> GraphEntry.T option) -> T -> T
  val update_congs : (GraphEntry.T list -> GraphEntry.T list) -> T -> T
  val update_redexes : (GraphEntry.T list -> GraphEntry.T list) -> T -> T
  val set_rep : GraphEntry.T option -> T -> T
  val set_congs : GraphEntry.T list -> T -> T
  val set_redexes : GraphEntry.T list -> T -> T
  val empty : T
end

functor EqClass(
  structure Graph : BANG_GRAPH
  type data
) : EQ_CLASS =
struct
  structure GraphEntry =
  struct
    structure Graph = Graph
    type data = data
    datatype T = GE of { graph: Graph.T, data: data, dirty: bool }
    fun update_graph f (GE r) = GE {graph= f(#graph r),dirty= #dirty r,data= #data r}
    fun update_dirty f (GE r) = GE {graph= #graph r,dirty= f(#dirty r),data= #data r}
    fun update_data f (GE r)  = GE {graph= #graph r,dirty= #dirty r,data= f(#data r)}
    fun get_graph (GE r) = #graph r
    fun get_dirty (GE r) = #dirty r
    fun get_data  (GE r) = #data r
    val set_graph = update_graph o K
    val set_dirty = update_dirty o K
    val set_data  = update_data  o K
    fun mk (gr, d) = GE { graph = gr, data = d, dirty = true }
  end

  type T = {
        rep : GraphEntry.T option,
      congs : GraphEntry.T list,
    redexes : GraphEntry.T list }
  fun get_rep ({rep=x,...}:T) = x
  fun get_congs ({congs=x,...}:T) = x
  fun get_redexes ({redexes=x,...}:T) = x
  fun update_rep f ({rep=rep,congs=congs,redexes=redexes}:T) = {rep=f rep,congs=congs,redexes=redexes}
  fun update_congs f ({rep=rep,congs=congs,redexes=redexes}:T) = {rep=rep,congs=f congs,redexes=redexes}
  fun update_redexes f ({rep=rep,congs=congs,redexes=redexes}:T) = {rep=rep,congs=congs,redexes=f redexes}
  fun set_rep x cl = update_rep (K x) cl
  fun set_congs x cl = update_congs (K x) cl
  fun set_redexes x cl = update_redexes (K x) cl
  
  val empty = { rep = NONE, congs = [], redexes = [] }
end

signature EQ_CLASS_TAB =
sig
  type T
  structure GraphEntry : GRAPH_ENTRY
  val get_irredexes : T -> GraphEntry.T list
  val update : T -> GraphEntry.T -> T
  val empty : T
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  val eq : Theory.Graph.T * Theory.Graph.T -> bool
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
  type data
) : EQ_CLASS_TAB =
struct
  structure G = Theory.Graph
  structure EqClass = EqClass(
    structure Graph = G
    type data = data)
  structure GraphEntry = EqClass.GraphEntry
  
  structure ClassIndex = Table(
    type key = int * int
    val ord = prod_ord int_ord int_ord
  )
  
  type T = (EqClass.T list) ClassIndex.table
  
  fun get_irredexes _  = []
  fun update _ _ = ClassIndex.empty
  val empty = ClassIndex.empty
end
