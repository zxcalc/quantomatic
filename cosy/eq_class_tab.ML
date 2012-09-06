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

functor GraphEntry(
  structure Graph : BANG_GRAPH
  type data
) : GRAPH_ENTRY =
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

signature EQ_CLASS =
sig
  type 'a T
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

signature EQ_CLASS_TAB =
sig
  type T
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure GraphEntry : GRAPH_ENTRY
  val get_irredexes : T -> GraphEntry.T list
  val update : T -> GraphEntry.T -> T
  val empty : T
  
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          GraphEntry.Graph.SharingOGraph
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  val eq : Theory.Graph.T * Theory.Graph.T -> bool
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
  type data
) : EQ_CLASS_TAB =
struct
  structure Theory = Theory
  structure Metric = Metric
  structure G = Theory.Graph
  structure GraphEntry = GraphEntry(
    structure Graph = G
    type data = data)
  
  type T = unit
  
  fun get_irredexes _  = []
  fun update _ _ = ()
  val empty = ()
end
