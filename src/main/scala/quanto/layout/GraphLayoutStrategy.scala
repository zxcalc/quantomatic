package quanto.layout

import quanto.data.{Graph, VName}

/// Builds a GraphLayoutData object containing the computed layout for a graph.
abstract class GraphLayoutStrategy {
  /// Secondary constructor, which allows the Graph to be provided at construction.
  def this(graph : Graph) {
    this();
    this.setGraph(graph)
  }

  /// Provide a Graph for which the layout can be computed with DStepLayoutStrategy.layout().
  /// @param graph the Graph that is to be laid out
  def setGraph(graph : Graph) : Unit;

  /// Implementations may provide additional methods for setting parameters for the layout algorithm.
  /// Implementations may also provide additional constructors for setting parameters for the layout algorithm.

  /// Create a GraphLayoutData object containing the layout data for the graph.
  /// The graph must have been previously provided to the GraphLayoutStrategy (by constructing with the data or by calling GraphLayoutStrategy.setGraph).
  /// @return a GraphLayoutData containing the layout data for the graph.
  def layout() : GraphLayoutData;
}

/// Represents the layout of a Graph.
/// Users do not construct objects of this class directly, but obtain instances from a GraphLayoutStrategy instead.
/// Note that for legacy reasons, although the GraphLayout class represents the Strategy design pattern, it does not generate GraphLayoutData objects.
abstract class GraphLayoutData {
  /// Get the layout's coordinates for the vertex named v.
  /// @param v the name of the vertex in the graph selected by dso
  /// @return the layout's coordinates for the queried vertex.
  def getCoords(v: VName) : (Double, Double);

  /// The original Graph, with the previous layout, that this layout was computed for.
  def graph : Graph;

  /// Get a new Graph exactly equal to the input, but having the layout from this GraphLayoutData object. The Graph that was computed on is not modified. This method is part of the trusted computing base.
  /// @return a new Graph exactly equal to the input, but having the layout from this GraphLayoutData object.
  final def asGraph() : Graph = {
    var newGraph = graph
    // For each vertex in the graph, replace its coordinate with the new coordinate of the vertex.
    for (v <- graph.verts) {
      newGraph = newGraph.updateVData(v) { vd => vd.withCoord(getCoords(v)) }
    }
    return newGraph
  }
}
