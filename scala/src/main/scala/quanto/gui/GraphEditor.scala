package quanto.gui

import graphview._
import swing._
import quanto.data._
import Names._


object GraphEditor extends SimpleSwingApplication {
  val initialGraph = (Graph[Unit,VData,Unit,Unit]("g0",())
    addVertex ("n0", NodeV((0, 0)))
    addVertex ("n1", NodeV((1, 1)))
    addVertex ("w0", WireV((0, 1.5)))
    newEdge   ((), ("n0", "n1"))
    newEdge   ((), ("n0", "n1"))
    newEdge   ((), ("n1", "n0"))
    newEdge   ((), ("n1", "w0"))
    newEdge   ((), ("n1", "n1"))
    //newEdge   ((), ("n1", "n1"))
    //newEdge   ((), ("n1", "n1"))
  )
  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = new GraphView {
      graph = initialGraph
      editMode = GraphView.ReadWrite
    }
    size = new Dimension(500,500)
  }
}