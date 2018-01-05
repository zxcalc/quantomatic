package quanto.gui

import java.awt.BorderLayout
import java.awt.event.{KeyAdapter, KeyEvent}
import java.io.File
import java.util.Calendar
import java.util.concurrent.TimeUnit

import org.gjt.sp.jedit.{Mode, Registers}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import quanto.rewrite.Simproc
import quanto.util.UserOptions.scaleInt
import quanto.util.swing.ToolBar
import quanto.data.Graph

import scala.swing.event.{ButtonClicked, Event, SelectionChanged}
import quanto.cosy.SimprocBatch
import quanto.util.{FileHelper, UserOptions}

import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dimension, GridPanel, Label, Orientation, Publisher, ScrollPane, Swing, TextArea}

case class HaltBatchProcessesEvent() extends Event

//
// This panel will create batch jobs
// The corresponding document for HasDocument is essentially empty

class BatchDerivationCreatorPanel extends BorderPanel with HasDocument with Publisher {
  val document = new BatchDerivationCreationDocument(this)
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val Toolbar = new ToolBar {
    //contents
  }
  val GraphList = new FilteredList(graphs)
  val LabelNumSimprocs = new Label()
  val LabelNumGraphs = new Label()
  val LabelNumPairs = new Label()
  val LabelTimeLimit = new Label()
  val NotesTextBox: TextEditor = new TextEditor(TextEditor.Modes.blank)


  val StartButton = new Button("Start")
  val HaltButton = new Button("Halt ongoing")
  val BatchDetails: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += VSpace
    contents += new GridPanel(4, 2) {

      contents += new Label("Simprocs:")
      contents += LabelNumSimprocs
      contents += new Label("Graphs:")
      contents += LabelNumGraphs
      contents += new Label("Total pairs:")
      contents += LabelNumPairs
      contents += new Label("Time limit:")
      contents += LabelTimeLimit

      maximumSize = new Dimension(scaleInt(200), scaleInt(400))
    }
  }
  val GraphChooser: Component = new BoxPanel(Orientation.Vertical) {

    contents += VSpace
    contents += header("Graphs to include:")
    contents += VSpace
    contents += GraphList
  }
  val IgnitionButtons: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, StartButton, HSpace, HaltButton, HSpace)
  }
  val NotesHolder : BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += VSpace
    contents += new Label("Notes:")
    contents += VSpace
    contents += NotesTextBox.Component
    NotesTextBox.Component.maximumSize = new Dimension(scaleInt(600), scaleInt(300))
  }
  def MainPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += SimprocChooser
    contents += GraphChooser
    contents += NotesHolder
    contents += BatchDetails

    contents += VSpace
    contents += IgnitionButtons
    contents += VSpace
  }
  //SimprocList is a var not a val, because it can then be destroyed and recreated when the simprocs in memory change
  var SimprocList = new FilteredList(simprocs.keys.toList)

  val SimprocChooser: BoxPanel = new BoxPanel(Orientation.Vertical) {
    // Made def not val because it references the var SimprocList
    contents += VSpace
    contents += header("Simprocs to include:")
    contents += VSpace
    contents += SimprocList
  }

  def header(title: String): BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, new Label(title), HSpace)
  }

  private def HSpace: Component = Swing.HStrut(scaleInt(10))

  private def VSpace: Component = Swing.VStrut(scaleInt(10))

  def TopScrollablePane = new ScrollPane(MainPanel)

  def simprocs: Map[String, Simproc] = QuantoDerive.CurrentProject.map(p => p.simprocs).getOrElse(Map())

  def graphs: List[String] = QuantoDerive.CurrentProject.map(p => p.filesEndingIn(".qgraph")).getOrElse(List())

  def loadGraph(name: String): Graph = {
    val root: String = QuantoDerive.CurrentProject.map(p => p.rootFolder).getOrElse("")
    val theory = QuantoDerive.CurrentProject.map(p => p.theory).get
    FileHelper.readFile[Graph](new File(root + "/" + name + ".qgraph"), json => Graph.fromJson(json, theory))
  }

  def refreshDataDisplay() {
    def numSimprocs: Int = simprocSelection.size

    def numGraphs: Int = graphSelection.size

    LabelNumSimprocs.text = numSimprocs.toString
    LabelNumGraphs.text = numGraphs.toString
    LabelNumPairs.text = (numGraphs * numSimprocs).toString
    val totalMilliseconds = numSimprocs * numGraphs * SimprocBatch.timeout
    LabelTimeLimit.text = s"${TimeUnit.MILLISECONDS.toHours(totalMilliseconds)} hrs ${
      TimeUnit.MILLISECONDS.toMinutes(totalMilliseconds) -
        TimeUnit.MINUTES.toMinutes(TimeUnit.MILLISECONDS.toHours(totalMilliseconds))
    } mins"
  }

  listenTo(this,
    StartButton,
    HaltButton,
    SimprocList.ListComponent.selection,
    GraphList.ListComponent.selection,
    PythonEditPanel)


  reactions += {
    case SimprocsUpdated() =>
      SimprocChooser.contents -= SimprocList
      SimprocList = new FilteredList(simprocs.keys.toList)
      SimprocChooser.contents += SimprocList
      refreshDataDisplay()
      listenTo(SimprocList.ListComponent.selection)
    case ButtonClicked(HaltButton) =>
      BatchDerivationCreatorPanel.jobID += 1
    case ButtonClicked(StartButton) =>
      val batch = SimprocBatch(simprocSelection, graphSelection.map(name => loadGraph(name)), NotesTextBox.getText)
      batch.run()
    case SelectionChanged(_) =>
      refreshDataDisplay()
  }

  def simprocSelection: List[String] = SimprocList.ListComponent.selection.items.toList

  def graphSelection: List[String] = GraphList.ListComponent.selection.items.toList

  add(TopScrollablePane, BorderPanel.Position.Center)

  add(Toolbar, BorderPanel.Position.North)

  refreshDataDisplay()

}

object BatchDerivationCreatorPanel {
  // The job ID exists so the user can cancel any currently running batch jobs
  // Jobs check the current job ID, and if it has increased since the job started the job will stop
  var jobID = 0

}