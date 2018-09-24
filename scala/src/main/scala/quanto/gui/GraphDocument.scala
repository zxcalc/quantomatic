package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import scala.swing.Component


class GraphDocument(val parent: Component, theory: Theory) extends Document with HasGraph {
  val description = "Graph"
  val fileExtension = "qgraph"

  // the graph, as it was last saved or loaded
  private var storedGraph: Graph = Graph(theory)
  protected var gr = storedGraph
  def unsavedChanges = storedGraph != graph

//  protected def gr = _graph
//  protected def gr_=(g: Graph) {
//    _graph = g
//
//    // clears any stored filename and the undo stack
//    resetDocumentInfo()
//  }

  def replaceJson(json: Json) {
    graph = Graph.fromJson(json, theory)
    publish(GraphReplaced(this, clearSelection = true))
    publish(DocumentReplaced(this))
    publish(DocumentChanged(this))
  }

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    storedGraph = Graph.fromJson(json, theory)
    graph = storedGraph
    publish(GraphReplaced(this, clearSelection = true))
  }

  protected def saveDocument(f: File) {
    val json = Graph.toJson(graph, theory)
    json.writeTo(f)
    storedGraph = graph
  }

  protected def clearDocument() {
    graph = Graph(theory)
    publish(GraphReplaced(this, clearSelection = true))
  }

  override protected def exportDocument(f: File) = {
    val view = parent match {
      case component : GraphEditPanel => component.graphView
      case _ => throw new Exception(
        "Exporting from this component is not supported. Please report bug"
      )
    }
    view.exportView(f, false)
  }
}
