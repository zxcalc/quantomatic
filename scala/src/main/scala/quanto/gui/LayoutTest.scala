package quanto.gui

import quanto.layout._
import quanto.layout.constraint._
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

  graphView.graph = Graph.random(10,15, 3)

  val layout = new ForceLayout with Ranking with Clusters

  layout.initialize(graphView.graph)
  layout.alpha = 0.5

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
