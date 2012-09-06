signature GRAPH_ENUM =
sig
  type data
end

functor FastGraphEnum(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  val eq : Theory.Graph.T * Theory.Graph.T -> bool
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
) : GRAPH_ENUM =
struct
  
  type data = unit
  
  structure EqClassTab = EqClassTab(
    structure Theory = Theory
    structure Metric = Metric
    val eq = eq
    type data = data)
  
  structure G = Theory.Graph
  
  (****)
end