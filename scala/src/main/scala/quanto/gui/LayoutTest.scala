package quanto.gui

import quanto.layout._
import quanto.layout.constraint._
import quanto.data._
import Names._

import graphview.GraphView
import swing._
import java.awt.event.{ActionEvent, ActionListener}


object LayoutTest extends SimpleSwingApplication {
  import Constraint._

  val graphView = new GraphView(Theory.DefaultTheory) {
    drawGrid = true
    dynamicResize = false
    focusable = true
  }

  graphView.graph = (Graph()
    addVertex ("b0", WireV() withCoord(-0.13234,-0.126))
    addVertex ("b1", WireV() withCoord(-0.245,-0.23452))
    addVertex ("b2", WireV() withCoord(-0.3345,-0.33654))
    addVertex ("b3", WireV() withCoord(-0.42,-0.434523))
    addVertex ("v0", NodeV() withCoord (0.03452,0.2456))
    addVertex ("v1", NodeV() withCoord (0.1253,0.1354))
    addVertex ("v2", NodeV() withCoord (0.235232,0.234253))
    addVertex ("v3", NodeV() withCoord (0.323453,0.3767))
    addEdge   ("e0", DirEdge(), "v0" -> "v2")
    addEdge   ("e1", DirEdge(), "v0" -> "v3")
    addEdge   ("e2", DirEdge(), "v1" -> "v2")
    addEdge   ("e3", DirEdge(), "v1" -> "v3")
    addEdge   ("e4", DirEdge(), "b0" -> "v0")
    addEdge   ("e5", DirEdge(), "b1" -> "v1")
    addEdge   ("e6", DirEdge(), "v2" -> "b2")
    addEdge   ("e7", DirEdge(), "v3" -> "b3")
    addBBox   ("bb0", BBData(), Set("b0","b1","b2","b3"))
    addBBox   ("bb1", BBData(), Set("b0","v3"))
  )

//  graphView.graph = Graph.random(20,20,1)

  val layout = new ForceLayout with Clusters with Ranking with VerticalBoundary
  layout.alpha0 = 0.2

  layout.initialize(graphView.graph)
  var run = 0
  var constraints = false

  val timer = new javax.swing.Timer(50, new ActionListener {
    def actionPerformed(e: ActionEvent) {

      layout.step()

      constraints = !constraints

      layout.updateGraph()
      graphView.graph = layout.graph
      graphView.invalidateGraph()
      graphView.repaint()

//      if (layout.prevEnergy < layout.energy) {
//        print("+")
//      } else if (layout.prevEnergy > layout.energy) {
//        print("-")
//      } else {
//        print("=")
//      }
//      run += 1
//      if (run % 80 == 0) println()
    }
  })

  def top = new MainFrame {
    title = "GraphView"
    contents = graphView
    size = new Dimension(800,800)

    timer.start()
  }
}
