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

    val view = parent match {
      case component : GraphEditPanel => component.graphView
    }

    /* rescale from screen coordinates to normal and return string */
    def coordToString(c1 : Double, c2 : Double) = {
      val p = view.trans.fromScreen(c1, c2)
      "(" + p._1.toString + ", " + p._2.toString +")"
    }

    printToFile(f)(p => {
      p.println("\\begin{tikzpicture}")
      p.println("\t\\begin{pgfonlayer}{nodelayer}")

      /* fill in all vertices */
      for ((vn,vd) <- graph.vdata) {
        val style = vd match {
          case vertexData : NodeV =>
            if (vertexData.typ == "Z") "gn"
            else if (vertexData.typ == "X") "rn"
            else "unknown"
          case _ : WireV => "wire"
        }

        val number = vn.toString
        val disp_rec = view.vertexDisplay(vn).shape.getBounds
        val coord = coordToString(disp_rec.getCenterX, disp_rec.getCenterY)

        val data = vd match {
          case vertexData : NodeV => vertexData.label
          case _ => ""
        }

        p.println("\t\t\\node [style=" + style +"] (" + number + ") at " + coord + " {" + data +"};")
      }

      /* fill in corners of !-boxes */
      for ((bbn,bbd) <- view.bboxDisplay) {
        val number_ul = bbn.toString + "ul"
        val coord_ul = coordToString(bbd.rect.getMinX, bbd.rect.getMinY)
        p.println("\t\t\\node [style=bbox] (" + number_ul + ") at " + coord_ul + " {};")

        val number_ur = bbn.toString + "ur"
        val coord_ur = coordToString(bbd.rect.getMaxX, bbd.rect.getMinY)
        p.println("\t\t\\node [style=none] (" + number_ur + ") at " + coord_ur + " {};")

        val number_ll = bbn.toString + "ll"
        val coord_ll = coordToString(bbd.rect.getMinX, bbd.rect.getMaxY)
        p.println("\t\t\\node [style=none] (" + number_ll + ") at " + coord_ll + " {};")

        val number_lr = bbn.toString + "lr"
        val coord_lr = coordToString(bbd.rect.getMaxX, bbd.rect.getMaxY)
        p.println("\t\t\\node [style=none] (" + number_lr + ") at " + coord_lr + " {};")
      }

      p.println("\t\\end{pgfonlayer}")

      p.println("\t\\begin{pgfonlayer}{edgelayer}")

      /* fill in all edges, need to take care of parallel edges */
      for ((en, ed) <- graph.edata) {
        val style = if (ed.isDirected) "directed" else "simple"
        p.println("\t\t\\draw [style=" + style + "] (" + graph.source(en).toString + ") to (" + graph.target(en).toString + ");" )
      }

      for ((bbn, _) <- view.bboxDisplay) {

        /* fill in edges connecting !-box corners */
        val number_ul = bbn.toString + "ul.center"
        val number_ur = bbn.toString + "ur.center"
        val number_ll = bbn.toString + "ll.center"
        val number_lr = bbn.toString + "lr.center"
        p.println("\t\t\\draw [style=blue] (" + number_ul + ") to (" + number_ur + ");" )
        p.println("\t\t\\draw [style=blue] (" + number_ul + ") to (" + number_ll + ");" )
        p.println("\t\t\\draw [style=blue] (" + number_ll + ") to (" + number_lr + ");" )
        p.println("\t\t\\draw [style=blue] (" + number_lr + ") to (" + number_ur + ");" )

        /* draw edges indicating nested !-boxes */
        graph.bboxParent.get(bbn) match {
          case Some(bb_parent) => {
            val parent_number_ul = bb_parent.toString + "ul.center"
            p.println("\t\t\\draw [style=blue] (" + number_ul + ") to (" + parent_number_ul + ");" )
          }
          case None =>
        }
      }
      p.println("\t\\end{pgfonlayer}")
      p.println("\\end{tikzpicture}")
    })
  }
}
