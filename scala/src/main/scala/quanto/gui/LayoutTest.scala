package quanto.gui

import quanto.layout._
import quanto.data._
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

  graphView.graph = Graph.random(5,10,0)

  val layout = new ForceLayout
  layout.initialize(graphView.graph)
  layout.alpha = 0.5

  val dag = layout.graph.dagCopy
  for (e <- dag.edges) {
    layout.constraints += { (distance from (dag.source(e)) to (dag.target(e)) along (0,1)) <=> 1.0 }
  }

  var run = 0

  val timer = new javax.swing.Timer(10, new ActionListener {
    def actionPerformed(e: ActionEvent) {

      layout.step()
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
