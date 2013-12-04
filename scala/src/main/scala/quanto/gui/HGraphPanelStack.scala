package quanto.gui

import graphview.GraphView
import quanto.data._
import swing._
import swing.event._
import Key.Modifier

class HGraphFrame(graph: Graph, name: String) extends Frame {

  val graphView = new GraphView(graph.data.theory) {
    drawGrid = true
    dynamicResize = true
    focusable = false
  }
  graphView.graph = graph

  val panel = new ScrollPane(graphView)
  contents = panel
  title = name

  size_=(new Dimension(200, 200))
  visible_=(true)

  //disable close operation
  override def closeOperation() {};

  import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE

  peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  // this is a simplied event version of the main frame
  graphView.listenTo(graphView.mouse.clicks, graphView.mouse.moves)

  // copy from controler
  def selectedVerts = graphView.selectedVerts
  def selectedVerts_=(s: Set[VName]) { graphView.selectedVerts = s }
  private def shiftVertsNoRegister(vs: TraversableOnce[VName], p1: Point, p2: Point) {
    val (dx,dy) = (graphView.trans scaleFromScreen (p2.getX - p1.getX), graphView.trans scaleFromScreen (p2.getY - p1.getY))
    graphView.graph = vs.foldLeft(graphView.graph) { (g,v) =>
      graphView.invalidateVertex(v)
      graphView.graph.adjacentEdges(v) foreach (graphView.invalidateEdge(_))
      g.updateVData(v) { d => d.withCoord (d.coord._1 + dx, d.coord._2 - dy) }
    }
  }

  var prevPoint : Point = null     //for draging
  //end of copy

  /*
  graphView.reactions += {
    case (e: MousePressed) =>
      graphView.requestFocus()

      val modifiers = e.modifiers
      val pt = e.point
      val clicks = e.clicks

      val vertexHit = graphView.vertexDisplay find {
        _._2.pointHit(pt)
      } map {
        _._1
      }
      val mouseDownOnSelectedVert = vertexHit exists (graphView.selectedVerts.contains(_))

      // clear the selection if the shift key isn't pressed and the vertex clicked isn't already selected
      if (!mouseDownOnSelectedVert &&
        (modifiers & Modifier.Shift) != Modifier.Shift) {
        selectedVerts = Set()
      }

      vertexHit match {
        case Some(v) =>
          selectedVerts += v // make sure v is selected, if it wasn't before
          prevPoint = pt

        case None => // no select box here
      }
      graphView.repaint()

    case MouseDragged(_, pt, _) =>
      if (!selectedVerts.isEmpty && prevPoint != null){
        shiftVertsNoRegister(selectedVerts, prevPoint, pt)
        graphView.repaint()
        prevPoint = pt
      }

  }
  */
}

class HGraphPanelStack {

  // in a stack order
  var frameStack: List[HGraphFrame] = List();

  def newFrame(graph: Graph, name: String) = {
    val frame = new HGraphFrame(graph, name)

    frameStack = frame :: frameStack
  }

  def closeFrame() = {

    frameStack match {
      case ((a: HGraphFrame) :: b) => {
        frameStack = b
        a.visible_=(false)
        a.dispose()
      }
      case List() => println(" error: no parent frame to close");
    }
  }

}