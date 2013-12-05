package quanto.gui

import java.io.{FileNotFoundException, IOException, File}
import graphview.GraphView
import swing.event.Event
import swing.{FileChooser, Dialog, Publisher}
import quanto.data.{GraphLoadException, Graph}
import quanto.util.json.{JsonParseException, Json}
import quanto.layout.DotLayout

import scala.collection.mutable.HashMap

abstract class GraphDocumentEvent extends Event

//TODO graph changed is not fully working with hgraph
case class GraphChanged(sender: GraphDocument) extends GraphDocumentEvent
case class GraphSaved(sender: GraphDocument) extends GraphDocumentEvent

class GraphDocument(view: GraphView, hgraphFrame : HGraphPanelStack) extends Publisher {
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

  def reLayout () {
    val dotLayout = new DotLayout();
    val g = dotLayout.layout(view.graph);
    graph_=(g);
  }


  //hgraph version of load graph, basically it loads all hgraph in the current dir,
  // and save them to the hgraph map

  //TODO: detail panel is not clean up here
  def loadGraph(f: File) {
    try {
      HGraph.clearGraphMap ()
      hgraphFrame.init()

      val dir = f.getAbsoluteFile().getParentFile()
      val files = dir.listFiles().filter(p => p.getName().contains(".hgraph"))
      //get a list of (name -> graph), please note to remove .hgraph has been removed from name
      val hgraphList = files.toList map {p => (p.getName().split(".hgraph").head, p)}
      //parse to graph
      val hgraphs = hgraphList map {i => (i._1, Graph.fromJson((Json.parse(i._2)), view.theory))}

      HGraph.initGraph (hgraphs)
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
/*
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

*/
  def loadGraph(json : Json) {
    try {

      val g = Graph.fromJson(json, view.theory)
      graph_=(g)
    } catch {
      case _: JsonParseException => error("load", "mal-formed JSON")
      case _: GraphLoadException => error("load", "invalid graph")
    }
  }

  def exportJson () =  {
    Graph.toJson(view.graph, view.theory)
  }


  // hgraph version of saveGraph
  def saveGraph(fopt: Option[File] = None) {
    fopt.orElse(file).map { f =>
      try {
        //save current graph to hgraph map
        HGraph.updateGraph (HGraph.current, HGraph.getParentKey(HGraph.current), graph)
        //get all graph
        val graphList = HGraph.exportToList ();

        def saveHGraph (item : (String, Graph)) = {
          // only for hgraph
          val fname = item._1
          val g = item._2
          if (fname != HGraph.toplevelKey) {
            //save to the same dir of the current strategy
            val path =  f.getAbsoluteFile().getParent() + "/" + fname + ".hgraph"
            val hgraphF = new File (path)
            Graph.toJson(g, view.theory).writeTo(hgraphF)
          }
        }
        //save all hgraph
        graphList map saveHGraph

        val main = HGraph.getGraph(HGraph.toplevelKey)
        val json = Graph.toJson(main, view.theory)
        json.writeTo(f)

        file = Some(f)
        storedGraph = view.graph
        publish(GraphSaved(this))
      } catch {
        case _: IOException => error("save", "file unwriteable")
      }
    }
  }

    /*
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
  */

  def newGraph() {
    file = None
    //clear hgraph mas
    HGraph.clearGraphMap ()
    hgraphFrame.init()

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

  def showCloseDialog (){
    if (promptUnsaved())
      newGraph()
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
