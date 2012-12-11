package quanto.gui

import graphview._
import swing._
import event.{UIElementResized, UIElementMoved}
import quanto.data._
import Names._


object GraphEditor extends SimpleSwingApplication {
  val nverts = 4
  val nedges = 10
  val rand = new util.Random
  var randomGraph = Graph[Unit,VData,Unit,Unit]("g0",())
  for (i <- 1 to nverts) {
    val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
    randomGraph = randomGraph.newVertex(NodeV(p))
  }
  val varray = randomGraph.verts.keys.toArray
  for(j <- 1 to nedges) {
    val s = varray(rand.nextInt(varray.size))
    val t = varray(rand.nextInt(varray.size))
    randomGraph = randomGraph.newEdge((), (s,t))
  }

//  var initialGraph = (Graph[Unit,VData,Unit,Unit]("g0",())
//    addVertex ("n0", NodeV((0, 0)))
//    addVertex ("n1", NodeV((1, 1)))
//    addVertex ("w0", WireV((0, 1.5)))
//    newEdge   ((), ("n0", "n1"))
//    newEdge   ((), ("n0", "n1"))
//    newEdge   ((), ("n1", "n0"))
//    newEdge   ((), ("n1", "w0"))
//    newEdge   ((), ("n1", "n1"))
//    //newEdge   ((), ("n1", "n1"))
//    //newEdge   ((), ("n1", "n1"))
//  )

  val UndoStack_ = new UndoStack

  val GraphView_ = new GraphView {
    graph = randomGraph
    drawGrid = true
    dynamicResize = true
    undoStack = Some(UndoStack_)
    editMode = GraphView.ReadWrite
  }

  val ScrollPane_ = new ScrollPane(GraphView_)

  val FileMenu_ = new Menu("File") {}

  val EditMenu_ = new Menu("Edit") {
    contents += new MenuItem(Action("Undo") {
      UndoStack_.undo()
    })
    contents += new MenuItem(Action("Redo") {
      UndoStack_.redo()
    })
  }

  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = ScrollPane_
    size = new Dimension(500,500)

    menuBar = new MenuBar {
      contents += (FileMenu_, EditMenu_)
    }

    listenTo(ScrollPane_)
    reactions += {
      case UIElementResized(ScrollPane_) => GraphView_.repaint()
    }
  }
}