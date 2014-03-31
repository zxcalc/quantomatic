package quanto.gui

import graphview._
import swing._
import swing.event._
import Key.Modifier
import quanto.data._
import Names._
import quanto.layout.ForceLayout
import quanto.util.json._
import quanto.layout.constraint._
import java.awt.event.{ActionEvent, ActionListener}

class GraphEditController(view: GraphView, val readOnly: Boolean = false) {
  private var _mouseState: MouseState = SelectTool()
  def mouseState = _mouseState

  def graphRef = view.graphRef

  def mouseState_=(s: MouseState) {
    if (readOnly) s match {
      case AddVertexTool() | AddBoundaryTool() | AddEdgeTool() | AddBangBoxTool() | DragEdge(_) | BangSelectionBox(_,_) =>
        throw new InvalidMouseStateException("readOnly == true", s)
      case _ =>
    }

    _mouseState = s
  }

  // a second controller that needs to by synchronised
  var pairedController : Option[GraphEditController] = None

  // GUI component connections
  var vertexTypeLabel : Option[Label] = None
  var vertexTypeSelect : ComboBox[String] = _
  var edgeTypeLabel: Option[Label] = None
  var edgeTypeSelect : ComboBox[String] = _
  var edgeDirectedCheckBox : CheckBox = _
  var dataField : TextField = _

  val qLayout = new ForceLayout with Clusters
  qLayout.alpha0 = 0.005
  qLayout.alphaAdjust = 1.0
  qLayout.keepCentered = false

  val layoutTimer = new javax.swing.Timer(10, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (qLayout.graph != null) {
        qLayout.step()
        qLayout.updateGraph()
        graph = qLayout.graph
        graphRef.publish(GraphReplaced(graphRef, clearSelection = false))
      } else {
        println("null graph")
      }
    }
  })


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
    case UndoPerformed(_) =>
      view.resizeViewToFit()
      view.repaint()
    case RedoPerformed(_) =>
      view.resizeViewToFit()
      view.repaint()
  }

  // wire up the view's internal state
  def graph = graphRef.graph
  def graph_=(g: Graph) { graphRef.graph = g }
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
      graph.adjacentEdges(v).foreach { view.invalidateEdge }
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
    graph.edgesBetween(vs._1, vs._2).foreach { view.invalidateEdge }
    undoStack.register("Add Edge") { deleteEdge(e) }
  }

  private def deleteEdge(e: EName) {
    val d = graph.edata(e)
    val vs = (graph.source(e), graph.target(e))
    val selected = if (selectedEdges.contains(e)) {
      selectedEdges -= e; true
    } else false

    graph.edgesBetween(vs._1, vs._2).foreach { view.invalidateEdge }
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
    graph.adjacentEdges(v).foreach { deleteEdge }
    graph.inBBox.domf(v).foreach { removeVertexFromBBox(_, v) }

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

  private def addBBox(bbname: BBName, d: BBData, contents: Set[VName]) {
    graph = graph.addBBox(bbname, d, contents)
    undoStack.register("Add Bang Box") {deleteBBox(bbname)}
  }

  private def deleteBBox(bbname: BBName) {
    val data = graph.bbdata(bbname)
    val contents = graph.contents(bbname)

    view.invalidateBBox(bbname)
    graph = graph.deleteBBox(bbname)

    undoStack.register("Delete Bang Box") {
      addBBox(bbname, data, contents)
    }
  }

  private def addVertexToBBox(bb: BBName, v: VName) {
    graph = graph.updateBBoxContents(bb, graph.contents(bb) + v)
    undoStack.register("Add Vertex to Bang Box") {
      removeVertexFromBBox(bb, v)
    }
  }

  private def removeVertexFromBBox(bb: BBName, v: VName) {
    graph = graph.updateBBoxContents(bb, graph.contents(bb) - v)
    undoStack.register("Remove Vertex from Bang Box") {
      addVertexToBBox(bb, v)
    }
  }

  private def setBBoxParent(bb: BBName, bbParentOpt : Option[BBName]) {
    val oldParentOpt = graph.bboxParent.get(bb)
    graph = graph.setBBoxParent(bb, bbParentOpt)
    undoStack.register("Set Bang Box Parent") {
      setBBoxParent(bb, oldParentOpt)
    }
  }

  private def setEdgeValue(e: EName, str: String) {
    val data = graph.edata(e)
    val oldVal = data.label
    graph = graph.updateEData(e) { _ => data.withValue(str) }
    graph.edgesBetween(graph.source(e), graph.target(e)).foreach { view.invalidateEdge }
    undoStack.register("Set Edge Data") { setEdgeValue(e, oldVal) }
  }

  private def setVertexValue(v: VName, str: String) {
    graph.vdata(v) match {
      case data: NodeV =>
        val oldVal = data.label
        graph = graph.updateVData(v) { _ => data.withValue(str) }
        view.invalidateVertex(v)
        graph.adjacentEdges(v).foreach { view.invalidateEdge }
        undoStack.register("Set Vertex Data") { setVertexValue(v, oldVal) }
      case _ =>
    }
  }

  private def replaceGraph(gr : Graph, desc: String) {
    val oldGraph = graph
    graph = gr
    graphRef.publish(GraphReplaced(graphRef, clearSelection = false))
    undoStack.register(desc) { replaceGraph(oldGraph, desc) }
  }

  def layoutGraph() {
    val lo = new ForceLayout with Clusters
    replaceGraph(lo.layout(graph), "Layout Graph")
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
                    initial = data.label).map { newVal => setVertexValue(v, newVal) }
                case _ =>
                  val edgeHit = view.edgeDisplay find { _._2.pointHit(pt) } map { _._1 }
                  edgeHit.map { e =>
                    val data = graph.edata(e)
                    Dialog.showInput(
                      title = "Edge data",
                      message = "Edge data",
                      initial = data.label).map { newVal => setEdgeValue(e, newVal) }
                    view.repaint()
                  }
              }
            }
          } else {
            val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
            val mouseDownOnSelectedVert = vertexHit.exists(view.selectedVerts.contains)

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
        case AddBoundaryTool() => // do nothing
        case AddEdgeTool() =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          vertexHit map { startV =>
            mouseState = DragEdge(startV)
            view.edgeOverlay = Some(EdgeOverlay(pt, src = startV, tgt = Some(startV)))
            view.repaint()
          }
        case AddBangBoxTool() =>
          val cornerHit = view.bboxDisplay find { _._2.cornerHit(pt) } map { _._1 }

          cornerHit match {
            case Some(bb) =>
              mouseState = DragBangBoxNesting(bb)
            case None =>
              val box = BangSelectionBox(pt, pt)
              mouseState = box
              view.selectionBox = Some(box.rect)
          }

        case state => throw new InvalidMouseStateException("MousePressed", state)
      }

    case MouseDragged(_, pt, _) =>
      mouseState match {
        case SelectTool() =>      // do nothing
        case AddVertexTool() =>   // do nothing
        case AddBoundaryTool() =>   // do nothing
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
        case DragBangBoxNesting(startBB) =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          val bboxHit = if (vertexHit == None) view.bboxDisplay find { _._2.cornerHit(pt) } map { _._1 }
                        else None
          view.bboxOverlay = Some(BBoxOverlay(pt, startBB, vertexHit, bboxHit))
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
            view.resizeViewToFit()
            undoStack.register("Move Vertices") { shiftVerts(verts, end, start) }
          }

          mouseState = SelectTool()

        case AddVertexTool() =>
          val coord = view.trans fromScreen (pt.getX, pt.getY)

          val vertexData = vertexTypeSelect.selection.item match {
            case "<wire>" => WireV(theory = theory)
            case typ      =>
//              println("adding: " + theory.vertexTypes(typ).defaultData)
              NodeV(data = theory.vertexTypes(typ).defaultData, theory = theory).withCoord(coord)
          }

          addVertex(graph.verts.freshWithSuggestion(VName("v0")), vertexData.withCoord(coord))

        case AddBoundaryTool() =>
          val coord = view.trans fromScreen (pt.getX, pt.getY)
          val vertexData = WireV(theory = theory, annotation = JsonObject("boundary" -> JsonBool(true)))
          addVertex(graph.verts.freshWithSuggestion(VName("b0")), vertexData.withCoord(coord))

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

        case DragBangBoxNesting(startBB) =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          val bboxHit = if (vertexHit == None) view.bboxDisplay find { _._2.cornerHit(pt) } map { _._1 }
          else None

          vertexHit.map { v =>
            if (graph.contents(startBB).contains(v)) removeVertexFromBBox(startBB, v)
            else addVertexToBBox(startBB, v)
          }

          bboxHit.map { bbChild =>
            // only consider adding this bbox as a child if it is not already a parent
            if (!graph.bboxParents(startBB).contains(bbChild)) {
              if (graph.bboxParent.get(bbChild) == Some(startBB)) setBBoxParent(bbChild, None)
              else setBBoxParent(bbChild, Some(startBB))
            }
          }

          mouseState = AddBangBoxTool()
          view.bboxOverlay = None
          view.repaint()

        case state => throw new InvalidMouseStateException("MouseReleased", state)
      }

  }
  
  view.listenTo(view.keys)
  var rDown = false

  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  view.reactions += {
    case KeyPressed(_, (Key.Delete | Key.BackSpace), _, _) =>
      if (!readOnly && (!selectedVerts.isEmpty || !selectedEdges.isEmpty || !selectedBBoxes.isEmpty)) {
        undoStack.start("Delete Vertices/Edges/BBoxes")
        selectedVerts.foreach { deleteVertex }
        selectedEdges.foreach { deleteEdge }
        selectedBBoxes.foreach { deleteBBox }
        undoStack.commit()
        view.repaint()
      }
    case KeyPressed(_, Key.R, _, _) =>
      if (!rDown) {
        rDown = true
        qLayout.initialize(graph, randomCoords = false)
        qLayout.lockedVertices.clear()
        if (!selectedVerts.isEmpty) {
          graph.verts.foreach { v => if (!selectedVerts.contains(v)) qLayout.lockedVertices += v }
        }

        undoStack.start("Relax layout")
        replaceGraph(graph, "")
        layoutTimer.start()
      }
    case KeyReleased(_, Key.R, _, _) =>
      rDown = false
      layoutTimer.stop()

      replaceGraph(graph, "")
      undoStack.commit()
    case KeyPressed(_, Key.Minus, _, _)  => view.zoom *= 0.6
    case KeyPressed(_, Key.Equals, _, _) => view.zoom *= 1.6
  }
}
