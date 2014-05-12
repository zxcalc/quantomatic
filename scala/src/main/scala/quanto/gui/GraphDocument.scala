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
    def printToFile(file_name: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(file_name)
      try { op(p) } finally { p.close() }
    }

    graph = graph.snapToGrid() // this is bad, need to fix it later
    val view = new graphview.GraphView(theory, this)

    printToFile(f)(p => {
      p.println("\\begin{tikzpicture}")
      p.println("\t\\begin{pgfonlayer}{nodelayer}")

      /* fill in all nodes */
      for ((vn,vd) <- graph.vdata) {
        val style = vd match {
          case vertexData : NodeV =>
            if (vertexData.typ == "Z") "gn"
            else if (vertexData.typ == "X") "rn"
            else "unknown"
          case _ : WireV => "wire"
        }

        val number = vn.toString
        val coord = "(" + vd.coord._1.toString + ", " + vd.coord._2.toString +")"

        val data = vd match {
          case vertexData : NodeV => vertexData.label
          case _ => ""
        }

        p.println("\t\t\\node [style=" + style +"] (" + number + ") at " + coord + " {" + data +"};")
      }

      p.println("\t\\end{pgfonlayer}")

      p.println("\t\\begin{pgfonlayer}{edgelayer}")

      /* fill in all edges, need to take care of parallel edges */
      for ((en, ed) <- graph.edata) {
        val style = if (ed.isDirected) "directed" else "simple"
        p.println("\t\t\\draw [style=" + style + "] (" + graph.source(en).toString + ") to (" + graph.target(en).toString + ");" )
      }
      p.println("\t\\end{pgfonlayer}")
      p.println("\\end{tikzpicture}")
    })
  }
}
