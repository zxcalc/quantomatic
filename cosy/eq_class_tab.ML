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
  val get_irredexes : T -> Theory.Graph.T list
  val update : T -> Theory.Graph.T -> T
  val empty : T
  
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
  val eq : Theory.Graph.T * Theory.Graph.T -> bool
) : EQ_CLASS_TAB =
struct
  structure Theory = Theory
  structure Metric = Metric
  structure G = Theory.Graph
  
  type T = unit
  
  fun get_irredexes _  = []
  fun update _ _ = ()
  val empty = ()
end
