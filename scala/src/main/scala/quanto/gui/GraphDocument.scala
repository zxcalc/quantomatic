package quanto.gui

import java.io.{FileNotFoundException, IOException, File}
import graphview.GraphView
import swing.event.Event
import swing.{FileChooser, Dialog, Publisher}
import quanto.data.{GraphLoadException, Graph}
import quanto.util.json.{JsonParseException, Json}

abstract class GraphDocumentEvent extends Event
case class GraphChanged(sender: GraphDocument) extends GraphDocumentEvent
case class GraphSaved(sender: GraphDocument) extends GraphDocumentEvent

class GraphDocument(view: GraphView) extends Publisher {
  var file: Option[File] = None
  val undoStack = new UndoStack

  // the graph, as it was last saved or loaded
  private var storedGraph: Graph = Graph(view.theory)
  def unsavedChanges = storedGraph != view.graph

  def graph = view.graph
  def graph_=(g: Graph) {
    undoStack.clear()
    storedGraph = g
    view.graph = g
    view.invalidateGraph()
    view.repaint()

    publish(GraphChanged(this))
  }

  def titleDescription =
    file.map(f => f.getName).getOrElse("untitled") + (if (unsavedChanges) "*" else "")

  def promptUnsaved() = {
    if (unsavedChanges) {
      Dialog.showConfirmation(
        title = "Unsaved changes",
        message = "There are unsaved changes, do you wish to continue?") == Dialog.Result.Yes
    } else true
  }

  private def promptExists(f: File) = {
    if (f.exists()) {
      Dialog.showConfirmation(
        title = "File exists",
        message = "File exists, do you wish to overwrite?") == Dialog.Result.Yes
    } else true
  }

  private def error(action: String, reason: String) {
    Dialog.showMessage(
      title = "Error",
      message = "Cannot " + action + " graph (" + reason + ")",
      messageType = Dialog.Message.Error)
  }

  def loadGraph(f: File) {
    try {
      val json = Json.parse(f)
      val g = Graph.fromJson(json, view.theory)

      file = Some(f)
      graph_=(g)
    } catch {
      case _: JsonParseException => error("load", "mal-formed JSON")
      case _: GraphLoadException => error("load", "invalid graph")
      case _: FileNotFoundException => error("load", "not found")
      case _: IOException => error("load", "file unreadable")
    }
  }

  def saveGraph(fopt: Option[File] = None) {
    fopt.orElse(file).map { f =>
      try {
        val json = Graph.toJson(view.graph, view.theory)
        json.writeTo(f)

        file = Some(f)
        storedGraph = view.graph
        publish(GraphSaved(this))
      } catch {
        case _: IOException => error("save", "file unwriteable")
      }
    }
  }

  def newGraph() {
    file = None
    graph_=(Graph(view.theory))
  }

  def showSaveAsDialog() {
    val chooser = new FileChooser()
    chooser.showSaveDialog(view) match {
      case FileChooser.Result.Approve =>
        if (promptExists(chooser.selectedFile)) saveGraph(Some(chooser.selectedFile))
      case _ =>
    }
  }

  def showOpenDialog() {
    if (promptUnsaved()) {
      val chooser = new FileChooser()
      chooser.showOpenDialog(view) match {
        case FileChooser.Result.Approve =>
          loadGraph(chooser.selectedFile)
        case _ =>
      }
    }
  }

//  def graph_=(g: Graph) {
//    view.graph = graph
//    view.invalidateGraph()
//  }
//
//  def graph = view.graph

  // any time the graph state changes in a meaningful way, an undo is registered
  listenTo(undoStack)
  reactions += {
    case UndoRegistered(_) =>
      publish(GraphChanged(this))
  }
}
