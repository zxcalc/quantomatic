package quanto.gui

import java.awt.Toolkit
import java.awt.datatransfer._
import java.awt.event.{ActionEvent, ActionListener}
import java.util.Calendar

import quanto.data.Names._
import quanto.data._
import quanto.gui.graphview.{BBoxOverlay, EdgeOverlay, _}
import quanto.layout.ForceLayout
import quanto.layout.constraint._
import quanto.util.json._
import quanto.util.{Globals, UserOptions}

import scala.swing._
import scala.swing.event.Key.Modifier
import scala.swing.event._

case class VertexSelectionChanged(graph: Graph, selectedVerts: Set[VName]) extends GraphEvent

class GraphEditController(view: GraphView, undoStack: UndoStack, val readOnly: Boolean = false)
  extends ClipboardOwner {

  private var _mouseState: MouseState = SelectTool()
  def mouseState = _mouseState

  def graphRef = view.graphRef

  def mouseState_=(s: MouseState) {
    if (readOnly) s match {
      case _: AddVertexTool | _: AddBoundaryTool | _: AddEdgeTool | _: AddBangBoxTool | _: DragEdge | _: BangSelectionBox =>
        throw new InvalidMouseStateException("readOnly == true", s)
      case _ =>
    }

    _mouseState = s
  }


  def lostOwnership(p1: Clipboard, p2: Transferable) {}

  // a second controller that needs to by synchronised
  var pairedController : Option[GraphEditController] = None

  // GUI component connections
  var vertexTypeLabel : Option[Label] = None
  var controlsOpt : Option[GraphEditControls] = None

  var keepSnapped = true

//  var vertexTypeSelect : ComboBox[String] = _
//  var edgeTypeLabel: Option[Label] = None
//  var edgeTypeSelect : ComboBox[String] = _
//  var edgeDirectedCheckBox : CheckBox = _
//  var dataField : TextField = _

  val qLayout = new ForceLayout with Clusters
  qLayout.alpha0 = 0.005
  qLayout.alphaAdjust = 1.0
  qLayout.keepCentered = false
  val q1Layout = new ForceLayout
  q1Layout.gravity = 0
  q1Layout.nodeCharge = 0
  q1Layout.alpha0 = 0.005
  q1Layout.alphaAdjust = 1.0
  q1Layout.keepCentered = false

  val layoutTimer = new javax.swing.Timer(5, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (qLayout.graph != null) {
        view.requestFocusInWindow()
        qLayout.step()
        qLayout.updateGraph()
        graph = qLayout.graph
        graphRef.publish(GraphReplaced(graphRef, clearSelection = false))
      } else {
        println("null graph")
      }
    }
  })

  val layoutTimer1 = new javax.swing.Timer(5, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (q1Layout.graph != null) {
        view.requestFocusInWindow()
        q1Layout.step()
        q1Layout.updateGraph()
        graph = q1Layout.graph
        graphRef.publish(GraphReplaced(graphRef, clearSelection = false))
      } else {
        println("null graph")
      }
    }
  })


  // listen to undo stack
  view.listenTo(undoStack)

  //private var _undoStack: UndoStack = new UndoStack

//  def undoStack = _undoStack
//  def undoStack_=(s: UndoStack) {
//    view.deafTo(_undoStack)
//    _undoStack = s
//    view.listenTo(_undoStack)
//  }

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

  // note we need start *and* previous position to do accurate dragging with snapping
  private def shiftVertsNoRegister(vs: TraversableOnce[VName], start: Point, prev: Point, end: Point) {
    val dx =
      roundIfSnapped(view.trans scaleFromScreen (end.getX - start.getX)) -
      roundIfSnapped(view.trans scaleFromScreen (prev.getX - start.getX))
    val dy =
      roundIfSnapped(view.trans scaleFromScreen (end.getY - start.getY)) -
      roundIfSnapped(view.trans scaleFromScreen (prev.getY - start.getY))
    //val (dx,dy) = (view.trans scaleFromScreen (p2.getX - p1.getX), view.trans scaleFromScreen (p2.getY - p1.getY))
    graph = vs.foldLeft(graph) { (g,v) =>
      view.invalidateVertex(v)
      graph.adjacentEdges(v).foreach { view.invalidateEdge }

      g.updateVData(v) { d =>
        d.withCoord (
          d.coord._1 + dx,
          d.coord._2 - dy
        ) }
    }
  }

  // shift vertices and register undo
  private def shiftVerts(vs: TraversableOnce[VName], p1: Point, p2: Point) {
    shiftVertsNoRegister(vs, p1, p1, p2)
    undoStack.register("Move Vertices") { shiftVerts(vs, p2, p1) }
  }

  private def addEdge(e: EName, d: EData, vs: (VName, VName)) {
    graph = graph.addEdge(e, d, vs)
    graph.edgesBetween(vs._1, vs._2).foreach { view.invalidateEdge }
    undoStack.register("Add Edge") { deleteEdge(e) }
  }

  // Uses the current state of the controller to add an edge
  private def addEdgeFromController(v1: VName, v2: VName): Unit = {
    controlsOpt.foreach { c =>
      val edgeType = theory.edgeTypes(c.EdgeTypeSelect.selection.item).defaultData
      val theoryJSON : JsonObject = JsonObject(
        "data" -> edgeType
      )

      val eData = if (c.EdgeDirected.selected) DirEdge.fromJson(theoryJSON, theory)
      else UndirEdge.fromJson(theoryJSON, theory)

      addEdge(graph.edges.fresh, eData, (v1, v2))
    }
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

  private def flipEdge(e: EName): Unit = {
    graph = graph.deleteEdge(e).addEdge(e, graph.edata(e), (graph.target(e),graph.source(e)))
    view.invalidateEdge(e)

    undoStack.register("Flip edge direction") { flipEdge(e) }
  }

  private def toggleDirected(e: EName): Unit = {
    val d = graph.edata(e)
    val vs = (graph.source(e), graph.target(e))
    if (d.isDirected) graph = graph.deleteEdge(e).addEdge(e, d.toUndirEdge, vs)
    else graph = graph.deleteEdge(e).addEdge(e, d.toDirEdge, vs)
    view.invalidateEdge(e)

    undoStack.register("Toggle edge directed") { toggleDirected(e) }
  }


  private def addVertex(v: VName, d: VData) {
    val d1 = d.withCoord(roundCoordIfSnapped(d.coord))
    graph = graph.addVertex(v, d1)
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
    val vertices = graph.contents(bbname)
    val parent_bbox = graph.bboxParent.get(bbname)
    val child_bboxes = graph.bboxChildren(bbname)
    val selected = if (selectedBBoxes.contains(bbname)) {
      selectedBBoxes -= bbname; true
    } else false

    view.invalidateBBox(bbname)
    graph = graph.deleteBBox(bbname)

    undoStack.register("Delete Bang Box") {
      addBBox(bbname, data, vertices)
      graph = graph.setBBoxParent(bbname, parent_bbox)
      child_bboxes.foreach {child => graph = graph.setBBoxParent(child, Some(bbname))}
      if (selected) selectedBBoxes += bbname
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
        val oldVal = data.value

        graph = graph.updateVData(v) { _ => data.withValue(str) }
        view.invalidateVertex(v)
        graph.adjacentEdges(v).foreach { view.invalidateEdge }
        undoStack.register("Set Vertex Data") { setVertexValue(v, oldVal) }
      case _ =>
    }
  }

  private def setVertexTyp(v: VName, typ: String) {
    graph.vdata(v) match {
      case data: NodeV =>
        val oldTyp = data.typ
        graph = graph.updateVData(v) { _ => data.withTyp(typ) }
        view.invalidateVertex(v)
        graph.adjacentEdges(v).foreach { view.invalidateEdge }
        undoStack.register("Set Vertex Type") { setVertexTyp(v, oldTyp) }
      case _ =>
    }
  }

  private def replaceGraph(gr : Graph, desc: String) {
    val oldGraph = graph
    graph = gr
    graphRef.publish(GraphReplaced(graphRef, clearSelection = false))
    undoStack.register(desc) { replaceGraph(oldGraph, desc) }
  }

  private def replaceSelection(vs: Set[VName], es: Set[EName], bbs: Set[BBName], desc: String) {
    val (oldVs, oldEs, oldBBs) = (selectedVerts, selectedEdges, selectedBBoxes)
    selectedVerts = vs
    selectedEdges = es
    selectedBBoxes = bbs
    undoStack.register(desc) { replaceSelection(oldVs, oldEs, oldBBs, desc) }
  }

  def copySubgraph() {
    if (!view.selectedVerts.isEmpty) {
      val jsonString = Graph.toJson(graph.fullSubgraph(view.selectedVerts, view.selectedBBoxes), theory).toString
      Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(jsonString), this)
    }
  }

  def cutSubgraph() {
    copySubgraph()

    if (!readOnly) {
      undoStack.start("Cut graph")
      view.selectedVerts.foreach(deleteVertex)
      view.selectedBBoxes.foreach(deleteBBox)
      undoStack.commit()
      view.repaint()
    }
  }

  def pasteSubgraph() {
    if (!readOnly) {
      val data = Toolkit.getDefaultToolkit.getSystemClipboard.getContents(this)
      if (data.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        //println("data supported")
        try {
          val jsonString = data.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
          val g = Graph.fromJson(Json.parse(jsonString), theory)
          val gfr = g.renameAvoiding(graph)
          undoStack.start("Paste from clipboard")
          replaceGraph(graph.appendGraph(gfr), "")
          replaceSelection(vs = gfr.verts, es = Set(), bbs = gfr.bboxes, "")
          undoStack.commit()
        } catch {
          case e: Exception =>
            // Fail silently if clipboard doesn't contain a parsable quantomatic graph
            //e.printStackTrace()
        }

        view.repaint()
      } //else println("data not supported")
    }
  }

  /**
   * Snaps the graph to a square grid with size 0.25
   */
  def snapToGrid() = {
    replaceGraph(graph.snapToGrid(), "Snap to grid")
    view.invalidateGraph(clearSelection = false)
    view.repaint()
  }

  def selectAll() : Unit = {
    selectedBBoxes = graph.bboxes
    selectedVerts = graph.verts
    selectedEdges = graph.edges
    view.publish(VertexSelectionChanged(graph, graph.verts))
    view.repaint()
  }

  private def roundIfSnapped(d : Double) = {
    if (keepSnapped) math.rint(d / 0.25) * 0.25 else d // rounds to .25
  }

  private def roundCoordIfSnapped(d : (Double, Double)) = (roundIfSnapped(d._1), roundIfSnapped(d._2))

  def layoutGraph() {
    val lo = new ForceLayout with Ranking with Clusters
    val t0 = System.currentTimeMillis()
    val newGraph = lo.layout(graph)
    val t1 = System.currentTimeMillis()
    println("time: " + (t1 - t0))
    println("final alpha: " + lo.alpha)
    println("final iteration: " + lo.iteration)
    replaceGraph(newGraph, "Layout Graph")
  }

  var rDown = false

  def startRelaxGraph(expandNodes : Boolean) {
    view.requestFocusInWindow()
    val layout = if (!expandNodes) q1Layout else qLayout
    if (!rDown) {
      rDown = true
      layout.initialize(graph, randomCoords = false)
      layout.clearLockedVertices()
      if (!selectedVerts.isEmpty) {
        graph.verts.foreach { v => if (!selectedVerts.contains(v)) layout.lockVertex(v) }
      }

      undoStack.start("Relax layout")
      replaceGraph(graph, "")
      if (!expandNodes) layoutTimer1.start() else layoutTimer.start()
    }
  }

  def endRelaxGraph() {
    if (rDown) {
      rDown = false
      layoutTimer.stop()
      layoutTimer1.stop()

      replaceGraph(graph, "")
      undoStack.commit()
    }
  }

  def cycleVertexType(vertex: VName, shift: Int = 1, includeWire: Boolean = false): Unit = {

    val currentData = graph.vdata(vertex)
    val options = theory.vertexTypes.keys.toSeq :+ "<wire>"
    val current = options.indexOf(currentData.typ)

    val next = options((current + shift + options.length) % options.length)
    val newTyp = if (next == "<wire>" & !includeWire)
      options((current + shift + 1 + options.length) % options.length)
    else next

    val coords = (currentData.coord._1, currentData.coord._2)
    // Using replaceGraph because of data lost on cycling
    replaceGraph(graph.updateVData(vertex)(d => {
      newTyp match {
        case "<wire>" =>
          WireV.apply(coords)
        case _ =>
          NodeV(theory.vertexTypes(newTyp).defaultData,
            annotation = d.annotation,
            theory = theory).withCoord(coords)
      }
    }
    ), "Cycled vertex")


  }


  def normaliseGraph(): Unit = {
    replaceGraph(graph.normalise.coerceWiresAndBoundaries, "Normalised graph")
  }

  def minimiseGraph(): Unit = {
    view.requestFocusInWindow()
    replaceGraph(graph.minimise.coerceWiresAndBoundaries, "Minimised graph")
  }

  def focusOnGraph(): Unit = {
    view.requestFocusInWindow()
    view.focusOnGraph()
  }

  def vertexAt(point: Point) : Option[VName] = view.vertexDisplay find {
    _._2.pointHit(point)
  } map {
    _._1
  }
  view.listenTo(view.mouse.clicks, view.mouse.moves)
  view.reactions += {
    case MousePressed(_, pt, modifiers, clicks, _) =>
      view.requestFocus()
      view.computeDisplayData()
      mouseState match {
        case SelectTool() =>
          if (clicks == 2) {
            if (!readOnly) {
              val vertexHit = view.vertexDisplay find { case (v, disp) =>
                disp.pointHit(pt) && !graph.vdata(v).isWireVertex
              } map { _._1 }

              vertexHit.map{v => (v,graph.vdata(v))} match {
                case Some((v, data: NodeV)) =>
                  if ((modifiers & Modifier.Control) == Modifier.Control) {
                    Dialog.showInput(
                      title = "Vertex type",
                      message = "Vertex type",
                      initial = data.typ).foreach { newTyp => setVertexTyp(v, newTyp) }
                  } else {
                    Dialog.showInput(
                      title = "Vertex data",
                      message = "Vertex data",
                      initial = data.value).foreach { newVal => setVertexValue(v, newVal) }
                  }
                  view.repaint()

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
            //val mouseDownOnSelectedVert = vertexHit.exists(view.selectedVerts.contains)
            //println(vertexHit)

            vertexHit match {
              case Some(v) =>

                // if 'v' was not previously selected, it should now be the *only* vertex selected unless shift key
                // is pressed
                if (!selectedVerts.contains(v)) {
                  if ((modifiers & Modifier.Shift) != Modifier.Shift) {
                    selectedVerts = Set(v)
                    selectedEdges = Set()
                    selectedBBoxes = Set()
                  } else {
                    selectedVerts += v
                  }

                  view.publish(VertexSelectionChanged(graph, selectedVerts))
                }

                mouseState = DragVertex(pt, pt)
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
        case FreehandTool(_, _) =>
          undoStack.start("Freehand drag")
          var vname = graph.verts.freshWithSuggestion(VName("v0"))
          vertexAt(pt) match {
            case Some(vertex) => // Already a vertex here, start a path
              vname = vertex
              mouseState = FreehandTool(Some(vname), startedWithNew = false)
            case None => // No vertex here! Create a wire node if enough time and distance have passed
              val coord = view.trans fromScreen(pt.getX, pt.getY)
              controlsOpt.foreach { c =>
                val vertexData = WireV(theory = theory)
                addVertex(vname, vertexData.withCoord(coord))
              }
              mouseState = FreehandTool(Some(vname), startedWithNew = true)
          }
          view.edgeOverlay = Some(EdgeOverlay(pt, src = vname, tgt = Some(vname)))
          view.repaint()
        case RequestMinimiseGraph() =>
        case RequestFocusOnGraph() =>
        case state => throw new InvalidMouseStateException("MousePressed", state)
      }

    case MouseDragged(_, pt, _) =>
      mouseState match {
        case SelectTool() =>      // do nothing
        case AddVertexTool() =>   // do nothing
        case AddBoundaryTool() =>   // do nothing
        case AddEdgeTool() =>     // do nothing
        case AddBangBoxTool() =>  // do nothing
        case FreehandTool(maybeVName, _) =>
          val start = maybeVName.get
          vertexAt(pt) match {
            case Some(vertex) =>
              view.edgeOverlay = Some(EdgeOverlay(pt, src = start, tgt = Some(vertex)))
            case None =>
                view.edgeOverlay = Some(EdgeOverlay(pt, src = start, tgt = None))
          }
          view.repaint()
        case RequestMinimiseGraph() =>
        case RequestFocusOnGraph() =>
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
          shiftVertsNoRegister(selectedVerts, start, prev, pt)
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
        case state => throw new InvalidMouseStateException("MouseReleased", state)
      }

    case MouseReleased(_, pt, modifiers, _, _) =>
      mouseState match {
        case SelectTool() => // do nothing
        case AddEdgeTool() => // do nothing
        case AddBangBoxTool() => // do nothing
        case FreehandTool(maybeVName, startedWithNew) =>
          view.edgeOverlay = None
          vertexAt(pt) match {
            case Some(vertex) => // Vertex here

              if (maybeVName.get == vertex) {
                // dragged to itself
                if (!startedWithNew) cycleVertexType(vertex)
              } else {
                // dragged to another vertex
                addEdgeFromController(maybeVName.get, vertex)
              }
            case None =>
              val coord = view.trans fromScreen(pt.getX, pt.getY)
              val vname = graph.verts.freshWithSuggestion(VName("v0"))
              controlsOpt.foreach { c =>
                val vertexData = WireV(theory = theory)
                addVertex(vname, vertexData.withCoord(coord))
              }                // dragged to another vertex
              addEdgeFromController(maybeVName.get, vname)
          }
          undoStack.commit()
          mouseState = FreehandTool(None, startedWithNew = false)

        case SelectionBox(start,_) =>
          val oldSelectedVerts = selectedVerts

          // clear the selection if the shift key isn't pressed
          if ((modifiers & Modifier.Shift) != Modifier.Shift)
          {
            selectedVerts = Set()
            selectedEdges = Set()
            selectedBBoxes = Set()
          }

          view.computeDisplayData()

          // add some wiggle room so moving by a few pixels still counts as a "click"
          if (math.abs(pt.getX - start.getX) < 3 && math.abs(pt.getY - start.getY) < 3) {
            var selectionUpdated = false
            view.vertexDisplay find (_._2.pointHit(pt)) foreach { x => selectionUpdated = true; selectedVerts += x._1 }

            if (!selectionUpdated)
              view.edgeDisplay find (_._2.pointHit(pt)) foreach { x => selectionUpdated = true; selectedEdges += x._1 }

            if (!selectionUpdated)
              view.bboxDisplay find (_._2.pointHit(pt)) foreach { x => selectionUpdated = true; selectedBBoxes += x._1 }

          } else {
            // box selection does not affect edges
            val r = mouseState.asInstanceOf[SelectionBox].rect
            view.vertexDisplay filter (_._2.rectHit(r)) foreach { selectedVerts += _._1 }
            view.bboxDisplay filter (_._2.insideRect(r)) foreach { selectedBBoxes += _._1 }
          }

          if (oldSelectedVerts != selectedVerts) {
            view.publish(VertexSelectionChanged(graph, selectedVerts))
          }


          mouseState = SelectTool()
          view.selectionBox = None
          view.repaint()
        
        case BangSelectionBox(start, _) =>
          view.computeDisplayData()

          val r = mouseState.asInstanceOf[BangSelectionBox].rect

//          if (pt.getX == start.getX && pt.getY == start.getY) {
//            var selectionUpdated = false
//            view.vertexDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedVerts += x._1 }
//
//            if (!selectionUpdated)
//              view.edgeDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedEdges += x._1 }
//          } else {
//            // box selection only affects vertices
//            val r = mouseState.asInstanceOf[BangSelectionBox].rect
//            view.vertexDisplay filter (_._2.rectHit(r)) foreach { selectedVerts += _._1 }
//          }

          val bangVerts = graph.verts.filter(view.vertexDisplay(_).rectHit(r))
          val bangBoxData = BBData(theory = theory) // no data/annotation for bboxes
          addBBox(graph.bboxes.fresh, bangBoxData, bangVerts)

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
          controlsOpt.map { c =>
            val coord = view.trans fromScreen (pt.getX, pt.getY)

            val vertexData = c.VertexTypeSelect.selection.item match {
              case "<wire>" => WireV(theory = theory)
              case typ      =>
                //              println("adding: " + theory.vertexTypes(typ).defaultData)
                NodeV(data = theory.vertexTypes(typ).defaultData, theory = theory)
            }

            addVertex(graph.verts.freshWithSuggestion(VName("v0")), vertexData.withCoord(coord))
          }


        case AddBoundaryTool() =>
          val coord = view.trans fromScreen (pt.getX, pt.getY)
          val vertexData = WireV(theory = theory, annotation = JsonObject("boundary" -> JsonBool(true)))
          addVertex(graph.verts.freshWithSuggestion(VName("b0")), vertexData.withCoord(coord))

        case DragEdge(startV) =>
          val vertexHit = view.vertexDisplay find { _._2.pointHit(pt) } map { _._1 }
          vertexHit.map { endV =>
              addEdgeFromController(startV, endV)
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
            // only consider adding this bbox as a child if it is not already a parent (or itself)
            if (!graph.bboxParentList(startBB).contains(bbChild) && startBB != bbChild) {
              if (graph.bboxParent.get(bbChild) == Some(startBB)) setBBoxParent(bbChild, None)
              else setBBoxParent(bbChild, Some(startBB))
            }
          }

          mouseState = AddBangBoxTool()
          view.bboxOverlay = None
          view.repaint()

        case RequestMinimiseGraph() =>
        case RequestFocusOnGraph() =>
        case state => throw new InvalidMouseStateException("MouseReleased", state)
      }

      // Auto-snapping disabled for now....
      //snapToGrid()

  }
  
  view.listenTo(view.keys)
  view.listenTo(view.mouse.wheel)


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
    case KeyPressed(_, Key.A, m, _) =>
      if (Modifier.Control == (m & Modifier.Control)) selectAll()
    case KeyPressed(_, Key.R, m, _) =>
      startRelaxGraph((m & Modifier.Shift) != Modifier.Shift)
    case KeyReleased(_, Key.R, _, _) =>
      endRelaxGraph()
    case KeyReleased(_, Key.G, _, _) =>
      snapToGrid()
      //replaceGraph(graph, "")
    case KeyPressed(_, Key.Minus, _, _)  => view.zoom *= 0.6
    case KeyPressed(_, Key.Equals, _, _) => view.zoom *= 1.6
    case KeyPressed(_, Key.Key0, _, _) => view.zoom = 1
    case MouseWheelMoved(_,_,modifiers,rotation) => view.zoom *= math.pow(1.2,-rotation)
    case KeyPressed(_, Key.C, modifiers, _) =>
      if ((modifiers & Globals.CommandDownMask) == Globals.CommandDownMask) { copySubgraph() }
    case KeyPressed(_, Key.X, modifiers, _) =>
      if ((modifiers & Globals.CommandDownMask) == Globals.CommandDownMask) { cutSubgraph() }
    case KeyPressed(_, Key.V, modifiers, _) =>
      if (modifiers == 0) {

        mouseState = AddVertexTool()
        controlsOpt.foreach { c =>
          if (c.GraphToolGroup.selected.contains(c.AddVertexButton)) {
            c.VertexTypeSelect.selection.index = (c.VertexTypeSelect.selection.index + 1) % (theory.vertexTypes.size + 1)
          }
          c.setMouseState(mouseState)
        }
      }
      else if ((modifiers & Globals.CommandDownMask) == Globals.CommandDownMask) { pasteSubgraph() }
    case KeyPressed(_, Key.S, modifiers, _)  =>
      if (modifiers  == 0) {
        mouseState = SelectTool()
        controlsOpt.map { c => c.setMouseState(mouseState) }
      }
    case KeyPressed(_, Key.E, modifiers, _)  =>
      if (modifiers  == 0) {
        mouseState = AddEdgeTool()
        controlsOpt.foreach { c =>
          if (c.GraphToolGroup.selected.contains(c.AddEdgeButton)) {
            c.EdgeTypeSelect.selection.index = (c.EdgeTypeSelect.selection.index + 1) % theory.edgeTypes.size
          }
          c.setMouseState(mouseState)
        }
      }
    case KeyPressed(_, Key.B, modifiers, _)  =>
      if (modifiers  == 0) {
        mouseState = AddBangBoxTool()
        controlsOpt.map { c => c.setMouseState(mouseState) }
      }
    case KeyPressed(_, Key.I, modifiers, _)  =>
      if (modifiers  == 0) {
        mouseState = AddBoundaryTool()
        controlsOpt.map { c => c.setMouseState(mouseState) }
      }
    case KeyPressed(_, Key.O, modifiers, _)  =>
      if (modifiers  == 0) {
        mouseState = AddBoundaryTool()
        controlsOpt.map { c => c.setMouseState(mouseState) }
      }
    case KeyPressed(_, Key.G, modifiers, _)  =>
      if ((modifiers & Globals.CommandDownMask) == Globals.CommandDownMask) {
        snapToGrid()
      }
    case KeyPressed(_, Key.M, _, _)  =>
      minimiseGraph()
    case KeyPressed(_, Key.F, modifiers, _) =>
      if ((modifiers & Modifier.Shift) != Modifier.Shift) {
        mouseState = FreehandTool(None, startedWithNew = false)
        controlsOpt.foreach { c => c.setMouseState(mouseState) }
      } else {
        undoStack.start("Flip edge direction")
        selectedEdges.foreach {
          flipEdge
        }
        undoStack.commit()
        view.repaint()
      }
    case KeyPressed(_, Key.D, _, _) =>
      if(selectedEdges.nonEmpty) {
        undoStack.start("Toggle edge directed")
        selectedEdges.foreach {
          toggleDirected
        }
        undoStack.commit()
        view.repaint()
      } else {
        controlsOpt.foreach { c =>
          if (c.GraphToolGroup.selected.contains(c.AddEdgeButton)) {
            c.EdgeDirected.selected = true
          }
        }
      }
  }
}
