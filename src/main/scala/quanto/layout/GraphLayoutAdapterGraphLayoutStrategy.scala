package quanto.layout

import quanto.data.{Graph, VName}

/// Builds a GraphLayoutData object containing the computed layout for a graph.
/// Adapts legacy GraphLayout instances.
class GraphLayoutAdapterGraphLayoutStrategy extends GraphLayoutStrategy {
  private[this] var strategy : GraphLayout = null;
  private[this] var graph : Graph = null;

  /// Secondary constructor, which allows the GraphLayout to be provided at construction.
  def this(strategy : GraphLayout) {
    this();
    this.setStrategy(strategy);
  }

  /// Secondary constructor, which allows the GraphLayout and Derivation to be provided at construction.
  def this(strategy : GraphLayout, graph : Graph) {
    //this(graph); // Secondary constructors cannot call superclass constructors
    this();
    this.setGraph(graph);
    this.setStrategy(strategy);
  }

  /// Provide a GraphLayout to adapt into a GraphLayoutStrategy.
  /// @param strategy the GraphLayout to adapt
  def setStrategy(strategy : GraphLayout) : Unit = {
    this.strategy = strategy;
  }

  /// Provide a Graph for which the layout can be computed with DStepLayoutStrategy.layout().
  /// @param graph the Graph that is to be laid out
  def setGraph(graph : Graph) : Unit = {
    this.graph = graph;
  }

  /// Create a GraphLayoutData object containing the layout data for the graph.
  /// The graph must have been previously provided to the GraphLayoutStrategy (by constructing with the data or by calling GraphLayoutStrategy.setGraph).
  /// @return a GraphLayoutData containing the layout data for the graph.
  def layout() : GraphLayoutData = {
    var newGraph = graph.copy()
    strategy.layout(newGraph, false)
    return new GraphLayoutAdapterGraphLayoutData(graph, newGraph);
  }
}

class GraphLayoutAdapterGraphLayoutData extends GraphLayoutData {
  private[this] var _graph : Graph = null;
  private[this] var newGraph : Graph = null;

  private[layout] def this(graph : Graph, newGraph : Graph) = {
    this();
    this._graph = _graph;
    this.newGraph = newGraph
  }

  /// Get the layout's coordinates for the vertex named v.
  /// @param v the name of the vertex in the graph selected by dso
  /// @return the layout's coordinates for the queried vertex.
  def getCoords(v: VName) : (Double, Double) = {
    return newGraph.vdata(v).coord;
  }

  /// The original Graph, with the previous layout, that this layout was computed for.
  def graph : Graph = {
    return _graph;
  }
}
