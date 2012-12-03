package quanto.gui

import swing._
import quanto.data._
import quanto.data.Names._


object GraphEditor extends SimpleSwingApplication {
  val graph = (Graph[Unit,VData,Unit,Unit]("g0",())
    addVertex ("n0", NodeV((0, 0)))
    addVertex ("n1", NodeV((1, 1)))
    addVertex ("w0", WireV((0, 1.5)))
    newEdge   ((), ("n0", "n1"))
    newEdge   ((), ("n0", "n1"))
    newEdge   ((), ("n1", "w0"))
  )
  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = new GraphView(graph)
    size = new Dimension(500,500)
  }
}