package quanto.gui

import java.io.File
import quanto.data.{Theory, HasGraph, Graph}
import quanto.util.json.Json
import scala.swing.Component


class GraphDocument(val parent: Component, theory: Theory) extends Document with HasGraph {
  val description = "Graph"
  val fileExtension = "qgraph"

  // the graph, as it was last saved or loaded
  private var storedGraph: Graph = Graph(theory)
  var _graph = storedGraph
  def unsavedChanges = storedGraph != _graph

  def graph = _graph
  def graph_=(g: Graph) {
    _graph = g

    // clears any stored filename and the undo stack
    resetDocumentInfo()
  }

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    _graph = Graph.fromJson(json, theory)
    storedGraph = _graph
  }

  protected def saveDocument(f: File) {
    val json = Graph.toJson(_graph, theory)
    json.writeTo(f)
    storedGraph = _graph
  }

  protected def clearDocument() {
    graph_=(Graph(theory))
  }
}
