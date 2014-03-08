package quanto.gui

import java.io.File
import graphview.GraphView
import quanto.data.Graph
import quanto.util.json.Json


class GraphDocument(view: GraphView) extends Document {
  val description = "Graph"
  val fileExtension = "qgraph"

  // the graph, as it was last saved or loaded
  private var storedGraph: Graph = Graph(view.theory)
  def unsavedChanges = storedGraph != view.graph
  val parent = view

  def graph = view.graph
  def graph_=(g: Graph) {
    storedGraph = g
    view.graph = g
    view.invalidateGraph()
    view.repaint()

    // clears any stored filename and the undo stack
    resetDocumentInfo()
  }

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    val g = Graph.fromJson(json, view.theory)
    storedGraph = g
    view.graph = g
    view.invalidateGraph()
    view.repaint()
  }

  protected def saveDocument(f: File) {
    val json = Graph.toJson(view.graph, view.theory)
    json.writeTo(f)
    storedGraph = view.graph
  }

  protected def clearDocument() {
    graph_=(Graph(view.theory))
  }
}
