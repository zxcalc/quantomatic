package quanto.gui

import graphview._
import swing._
import swing.event._
import Key.Modifier
import quanto.data._
import Names._

class GraphEditController(view: GraphView, val readOnly: Boolean = false) {
  private var _mouseState: MouseState = SelectTool()
  def mouseState = _mouseState

  def mouseState_=(s: MouseState) {
    if (readOnly) s match {
      case AddVertexTool() | AddEdgeTool() | AddBangBoxTool() | DragEdge(_) | BangSelectionBox(_,_) =>
        throw new InvalidMouseStateException("readOnly == true", s)
      case _ =>
    }

    _mouseState = s
  }

  // GUI component connections
  var vertexTypeLabel : Option[Label] = None
  var vertexTypeSelect : ComboBox[String] = _
  var edgeTypeLabel: Option[Label] = None
  var edgeTypeSelect : ComboBox[String] = _
  var edgeDirectedCheckBox : CheckBox = _
  var dataField : TextField = _

  // listen to undo stack
  private var _undoStack: UndoStack = new UndoStack
  view.listenTo(_undoStack)
  def undoStack = _undoStack
  def undoStack_=(s: UndoStack) {
    view.deafTo(_undoStack)
    _undoStack = s
    view.listenTo(_undoStack)
  }

  view.reactions += {
    case UndoPerformed(_) => view.repaint()
    case RedoPerformed(_) => view.repaint()
  }

  // wire up the view's internal state
  def graph = view.graph
  def graph_=(g: Graph) { view.graph = g }
  def selectedVerts = view.selectedVerts
  def selectedVerts_=(s: Set[VName]) { view.selectedVerts = s }
  def selectedEdges = view.selectedEdges
  def selectedEdges_=(s: Set[EName]) { view.selectedEdges = s }
  def selectedBBoxes = view.selectedBBoxes
  def selectedBBoxes_=(s: Set[BBName]) { view.selectedBBoxes = s }
  def theory = graph.data.theory


  // controller actions.

  private def shiftVertsNoRegister(vs: TraversableOnce[VName], p1: Point, p2: Point) {
    val (dx,dy) = (view.trans scaleFromScreen (p2.getX - p1.getX), view.trans scaleFromScreen (p2.getY - p1.getY))
    graph = vs.foldLeft(graph) { (g,v) =>
      view.invalidateVertex(v)
      graph.adjacentEdges(v) foreach (view.invalidateEdge(_))
      g.updateVData(v) { d => d.withCoord (d.coord._1 + dx, d.coord._2 - dy) }
    }
  }

  // shift vertices and register undo
  private def shiftVerts(vs: TraversableOnce[VName], p1: Point, p2: Point) {
    shiftVertsNoRegister(vs, p1, p2)
    undoStack.register("Move Vertices") { shiftVerts(vs, p2, p1) }
  }

  private def addEdge(e: EName, d: EData, vs: (VName, VName)) {
    graph = graph.addEdge(e, d, vs)
    graph.edgesBetween(vs._1, vs._2).foreach { view.invalidateEdge(_) }
    undoStack.register("Add Edge") { deleteEdge(e) }
  }

  private def deleteEdge(e: EName) {
    val d = graph.edata(e)
    val vs = (graph.source(e), graph.target(e))
    val selected = if (selectedEdges.contains(e)) {
      selectedEdges -= e; true
    } else false

    graph.edgesBetween(vs._1, vs._2).foreach(view.invalidateEdge(_))
    graph = graph.deleteEdge(e)

    undoStack.register("Delete Edge") {
      addEdge(e, d, vs)
      if (selected) selectedEdges += e
    }
  }


  private def addVertex(v: VName, d: VData) {
    graph = graph.addVertex(v, d)
    undoStack.register("Add Vertex") { deleteVertex(v) }
  }

  private def deleteVertex(v: VName) {
    undoStack.start("Delete Vertex")
    graph.adjacentEdges(v).foreach { deleteEdge(_) }
    // update bang boxes containing the vertex
    //removeVertexBangBoxes(v)  

    val d = graph.vdata(v)
    view.invalidateVertex(v)
    val selected = if (selectedVerts.contains(v)) {
      selectedVerts -= v; true
    } else false

    graph = graph.deleteVertex(v)

    undoStack += {
      addVertex(v, d)
      if (selected) selectedVerts += v
    }
    undoStack.commit()
  }

  //private def removeVertexBangBoxes(vname: VName) {
  //  graph.inBBox.unmapDom(vname)
  //}

  private def addBBox(bbname: BBName, d: BBData, contents: Set[VName]) {
    graph = graph.addBBox(bbname, d, contents)
    undoStack.register("Add Bang Box") {deleteBBox(bbname)}
  }

  private def deleteBBox(bbname: BBName) {
    val data = graph.bbdata(bbname)
    val contents = graph.contents(bbname)
    // TODO bang box selection

    view.invalidateBBox(bbname)
    graph = graph.deleteBBox(bbname)

    undoStack.register("Delete Bang Box") {
      addBBox(bbname, data, contents)
    }
  }

  private def setEdgeValue(e: EName, str: String) {
    val data = graph.edata(e)
    val oldVal = data.value
    graph = graph.updateEData(e) { _ => data.withValue(str) }
    graph.edgesBetween(graph.source(e), graph.target(e)).foreach { view.invalidateEdge(_) }
    undoStack.register("Set Edge Data") { setEdgeValue(e, oldVal) }
  }

  private def setVertexValue(v: VName, str: String) {
    graph.vdata(v) match {
      case data: NodeV =>
        val oldVal = data.value
        graph = graph.updateVData(v) { _ => data.withValue(str) }
        view.invalidateVertex(v)
        graph.adjacentEdges(v).foreach { view.invalidateEdge(_) }
        undoStack.register("Set Vertex Data") { setVertexValue(v, oldVal) }
      case _ =>
    }
  }

  view.listenTo(view.mouse.clicks, view.mouse.moves)
  view.reactions += {
    case MousePressed(_, pt, modifiers, clicks, _) =>
      view.requestFocus()
      mouseState match {
        case SelectTool() =>
          if (clicks == 2) {
            if (!readOnly) {
              val vertexHit = view.vertexDisplay find { case (v, disp) =>
                disp.pointHit(pt) && !graph.vdata(v).isWireVertex
              } map { _._1 }

              vertexHit.map{v => (v,graph.vdata(v))} match {
                case Some((v, data: NodeV)) =>
                  Dialog.showInput(
                    title = "Vertex data",
                    message = "Vertex data",
                    initial = data.value).map { newVal => setVertexValue(v, newVal) }
                case _ =>
                  val edgeHit = view.edgeDisplay find { _._2.pointHit(pt) } map { _._1 }
                  edgeHit.map { e =>
                    val data = graph.edata(e)
                    Dialog.showInput(
                      title = "Edge data",
                      message = "Edge data",
                      initial = data.value).map { newVal => setEdgeValue(e, newVal) }
                    view.repaint()
                  }
              }
            }
          } else {
            val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
            val mouseDownOnSelectedVert = vertexHit exists (view.selectedVerts.contains(_))

            // clear the selection if the shift key isn't pressed and the vertex clicked isn't already selected
            if (!mouseDownOnSelectedVert &&
              (modifiers & Modifier.Shift) != Modifier.Shift)
            {
              selectedVerts = Set()
              selectedEdges = Set()
              selectedBBoxes = Set()
            }

            vertexHit match {
              case Some(v) =>
                selectedVerts += v // make sure v is selected, if it wasn't before
                mouseState = DragVertex(pt,pt)
              case None =>
                val box = SelectionBox(pt, pt)
                mouseState = box
                view.selectionBox = Some(box.rect)
            }

            view.repaint()
          }
        case AddVertexTool() => // do nothing
        case AddEdgeTool() =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          vertexHit map { startV =>
            mouseState = DragEdge(startV)
            view.edgeOverlay = Some(EdgeOverlay(pt, src = startV, tgt = Some(startV)))
            view.repaint()
          }
        case AddBangBoxTool() => 
          val box = BangSelectionBox(pt, pt)
          mouseState = box
          view.selectionBox = Some(box.rect)
          
        case state => throw new InvalidMouseStateException("MousePressed", state)
      }

    case MouseDragged(_, pt, _) =>
      mouseState match {
        case SelectTool() =>      // do nothing
        case AddVertexTool() =>   // do nothing
        case AddEdgeTool() =>     // do nothing
        case AddBangBoxTool() =>  // do nothing
        case SelectionBox(start,_) =>
          val box = SelectionBox(start, pt)
          mouseState = box
          view.selectionBox = Some(box.rect)
          view.repaint()
        case BangSelectionBox(start,_) =>
          val box = BangSelectionBox(start, pt)
          mouseState = box
          view.selectionBox = Some(box.rect)
          view.repaint() 
        case DragVertex(start, prev) =>
          shiftVertsNoRegister(selectedVerts, prev, pt)
          view.repaint()
          mouseState = DragVertex(start, pt)
        case DragEdge(startV) =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          view.edgeOverlay = Some(EdgeOverlay(pt, startV, vertexHit))
          view.repaint()
      }

    case MouseReleased(_, pt, modifiers, _, _) =>
      mouseState match {
        case SelectTool()  =>     // do nothing
        case AddEdgeTool() =>     // do nothing
        case AddBangBoxTool () => // do nothing
        case SelectionBox(start,_) =>
          view.computeDisplayData()

          if (pt.getX == start.getX && pt.getY == start.getY) {
            var selectionUpdated = false
            view.vertexDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedVerts += x._1 }

            if (!selectionUpdated)
              view.edgeDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedEdges += x._1 }

            if (!selectionUpdated)
              view.bboxDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedBBoxes += x._1 }

            // TODO: bbox selection
          } else {
            // box selection only affects vertices
            val r = mouseState.asInstanceOf[SelectionBox].rect
            view.vertexDisplay filter (_._2.rectHit(r)) foreach { selectedVerts += _._1 }
          }

          mouseState = SelectTool()
          view.selectionBox = None
          view.repaint()
        
        case BangSelectionBox(start, _) =>
          view.computeDisplayData()

          if (pt.getX == start.getX && pt.getY == start.getY) {
            var selectionUpdated = false
            view.vertexDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedVerts += x._1 }

            if (!selectionUpdated)
              view.edgeDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedEdges += x._1 }
          } else {
            // box selection only affects vertices
            val r = mouseState.asInstanceOf[BangSelectionBox].rect
            view.vertexDisplay filter (_._2.rectHit(r)) foreach { selectedVerts += _._1 }
          }

          val bangBoxData = BBData(theory = theory) // fix this, first two parameters are left to default
          addBBox(graph.bboxes.fresh, bangBoxData, selectedVerts)

          selectedVerts = Set()
          selectedEdges = Set()
          selectedBBoxes = Set()

          mouseState = AddBangBoxTool()
          view.selectionBox = None
          view.repaint()

        case DragVertex(start, end) =>
          if (start.getX != end.getX || start.getY != end.getY) {
            // we don't call shiftVerts directly, because the vertices have already moved
            val verts = selectedVerts
            undoStack.register("Move Vertices") { shiftVerts(verts, end, start) }
          }

          mouseState = SelectTool()

        case AddVertexTool() =>
          val coord = view.trans fromScreen (pt.getX, pt.getY)

          val vertexData = vertexTypeSelect.selection.item match {
            case "<wire>" => WireV(theory = theory)
            case typ      => NodeV.fromJson(theory.vertexTypes(typ).defaultData, theory).withCoord(coord)
          }

          addVertex(graph.verts.fresh, vertexData.withCoord(coord))
          view.repaint()
        case DragEdge(startV) =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          vertexHit map { endV =>
            val defaultData = if (edgeDirectedCheckBox.selected) DirEdge.fromJson(theory.defaultEdgeData, theory)
                              else UndirEdge.fromJson(theory.defaultEdgeData, theory)
            addEdge(graph.edges.fresh, defaultData, (startV, endV))
          }
          mouseState = AddEdgeTool()
          view.edgeOverlay = None
          view.repaint()
        //case state => throw new InvalidMouseStateException("MouseReleased", state)
      }

  }
  
  view.listenTo(view.keys)
  view.reactions += {
    case KeyPressed(_, (Key.Delete | Key.BackSpace), _, _) =>
      if (!readOnly && (!selectedVerts.isEmpty || !selectedEdges.isEmpty || !selectedBBoxes.isEmpty)) {
        undoStack.start("Delete Vertices/Edges/BBoxes")
        selectedVerts.foreach { deleteVertex(_) }
        selectedEdges.foreach { deleteEdge(_) }
        selectedBBoxes.foreach { deleteBBox(_)}
        undoStack.commit()
        view.repaint()
      }
  }
}
