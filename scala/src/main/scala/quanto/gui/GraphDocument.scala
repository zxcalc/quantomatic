package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.{JsonParseException, Json}
import scala.swing.Component
import quanto.layout.ForceLayout
import quanto.layout.constraint._


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

  def loadGraph(json : Json) {
    try {
      // force to layout the graph before drawing
      //val lo = new ForceLayout with Clusters
      val layout = new ForceLayout with IRanking with VerticalBoundary with Clusters
      graph = layout.layout(Graph.fromJson(json, theory))
      publish(GraphReplaced(this, clearSelection = true))

    } catch {
      case _: JsonParseException => sys.error("load - mal-formed JSON")
      case _: GraphLoadException => sys.error("load - invalid graph")
    }
  }

  def exportJson () =  {
    Graph.toJson(graph, theory)
  }
}
