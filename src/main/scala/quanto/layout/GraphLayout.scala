package quanto.layout

import quanto.data.{Graph, VName}

import scala.collection.mutable
import scala.util.Random

class LayoutUninitializedException extends Exception("Layout data read before layout() called")

abstract class GraphLayout {
  protected val lockedVertices: mutable.Set[VName] = collection.mutable.Set()
  private val _coords = collection.mutable.Map[VName, (Double, Double)]()
  private var _graph: Graph = _

  /// Lock vertex named v.
  /// @param v Name of vertex to lock.
  def lockVertex(v: VName) {
    lockedVertices += v
  }

  /// Clear the set of locked vertices.
  def clearLockedVertices() {
    lockedVertices.clear()
  }

  /// Setter of existing values in _coords.
  /// @param v Vertex name to set coordinate of. 
  /// @param p New coordinates of vertex named v.
  final def setCoord(v: VName, p: (Double, Double)) {
    if (!lockedVertices.contains(v)) _coords(v) = p
  }

  /// Getter for values in _coords.
  /// @param v Vertex name to get coordinate of.
  final def coord(v: VName) = _coords(v)

  /// Copying getter (no matching setter) for _coords.
  final def coords: mutable.Map[VName, (Double, Double)] = _coords.clone()

  /// Layout graph g, initialising the algorithm with either the existing vertex coordinates from g or randomised coordinates.
  /// @param g Graph to lay out
  /// @param randomCoords true to randomise initial coordinates, false to use g's existing vertex coordinates as initial coordinates.
  final def layout(g: Graph, randomCoords: Boolean = true): Graph = {
    initialize(g, randomCoords)
    compute()
    updateGraph()

    graph
  }

  /// Initialise _coords to either the existing vertex coordinates of the graph g or to randomised coordinates.
  /// @param g Graph to lay out
  /// @param randomCoords true to randomise _coords, false to use g's existing vertex coordinates.
  def initialize(g: Graph, randomCoords: Boolean = true) {
    _graph = g
    _coords.clear()
    if (randomCoords) {
      val r = new Random(0xdeadbeef)
      graph.vdata.foreach { case (v, d) =>
        _coords(v) = (0.5 - r.nextDouble(), 0.5 - r.nextDouble())
      }
    }
    else {
      graph.vdata.foreach { case (v, d) =>
        _coords(v) = d.coord
      }
    }
  }

  /// Write the coordinates of the new layout to _graph.
  final def updateGraph() {
    _graph = _coords.foldLeft(graph) { case (g, (v, c)) => g.updateVData(v) {
      _.withCoord(c)
    }
    }
  }

  /// Getter (no matching setter) for _graph.
  final def graph: Graph = _graph

  /// Lay out _graph, writing new vertex coordinates to _coords (and helper functions above).
  protected def compute()
}
