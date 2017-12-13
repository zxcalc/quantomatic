package quanto.gui

import java.io.File
import java.util.Calendar
import java.util.concurrent.TimeUnit

import quanto.rewrite.Simproc
import quanto.util.UserOptions.scaleInt
import quanto.util.swing.ToolBar
import quanto.data.Graph

import scala.swing.event.{ButtonClicked, Event, SelectionChanged}
import quanto.cosy.SimprocBatch
import quanto.util.FileHelper

import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dimension, GridPanel, Label, Orientation, Publisher, ScrollPane, Swing}

case class HaltBatchProcessesEvent() extends Event

class BatchDerivationCreatorPanel extends BorderPanel with HasDocument with Publisher {
  val document = new BatchDerivationCreationDocument(this)
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val Toolbar = new ToolBar {
    //contents
  }
  var SimprocList = new FilteredList(simprocs.keys.toList)
  val SimprocChooser: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += header("Simprocs to include:")
    contents += VSpace
    contents += SimprocList
  }

  val GraphList = new FilteredList(graphs)
  val GraphChooser: Component = new BoxPanel(Orientation.Vertical) {

    contents += header("Graphs to include:")
    contents += VSpace
    contents += GraphList
  }


  val LabelNumSimprocs = new Label()
  val LabelNumGraphs = new Label()
  val LabelNumPairs = new Label()
  val LabelTimeLimit = new Label()

  val BatchDetails: BoxPanel = new BoxPanel(Orientation.Vertical) {
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
  val StartButton = new Button("Start")
  val HaltButton = new Button("Halt ongoing")
  val IgnitionButtons: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, StartButton, HSpace, HaltButton, HSpace)
  }
  val MainPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += SimprocChooser
    contents += GraphChooser
    contents += BatchDetails
    contents += IgnitionButtons
  }
  val TopScrollablePane = new ScrollPane(MainPanel)

  def simprocs: Map[String, Simproc] = QuantoDerive.CurrentProject.map(p => p.simprocs).getOrElse(Map())

  def graphs: List[String] = QuantoDerive.CurrentProject.map(p => p.filesEndingIn(".qgraph")).getOrElse(List())

  def loadGraph(name: String): Graph = {
    val root: String = QuantoDerive.CurrentProject.map(p => p.rootFolder).getOrElse("")
    val theory = QuantoDerive.CurrentProject.map(p => p.theory).get
    FileHelper.readFile[Graph](new File(root + "/" + name + ".qgraph"), json => Graph.fromJson(json, theory))
  }

  def header(title: String): BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, new Label(title), HSpace)
  }

  private def HSpace: Component = Swing.HStrut(scaleInt(10))

  def simprocSelection: List[String] = SimprocList.ListComponent.selection.items.toList

  def graphSelection: List[String] = GraphList.ListComponent.selection.items.toList

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
    case ButtonClicked(HaltButton) =>
      BatchDerivationCreatorPanel.jobID += 1
    case ButtonClicked(StartButton) =>
      val batch = SimprocBatch(simprocSelection, graphSelection.map(name => loadGraph(name)))
      batch.run()
    case SelectionChanged(_) =>
      def numSimprocs: Int = simprocSelection.count(_ => true)

      def numGraphs: Int = graphSelection.count(_ => true)

      LabelNumSimprocs.text = numSimprocs.toString
      LabelNumGraphs.text = numGraphs.toString
      LabelNumPairs.text = (numGraphs * numSimprocs).toString
      val totalMilliseconds = numSimprocs * numGraphs * SimprocBatch.timeout
      LabelTimeLimit.text = s"${TimeUnit.MILLISECONDS.toHours(totalMilliseconds)} hrs ${
        TimeUnit.MILLISECONDS.toMinutes(totalMilliseconds) -
          TimeUnit.MINUTES.toMinutes(TimeUnit.MILLISECONDS.toHours(totalMilliseconds))
      } mins"

  }

  private def VSpace: Component = Swing.VStrut(scaleInt(10))

  add(TopScrollablePane, BorderPanel.Position.Center)

  add(Toolbar, BorderPanel.Position.North)

}

object BatchDerivationCreatorPanel {
  // The job ID exists so the user can cancel any currently running batch jobs
  // Jobs check the current job ID, and if it has increased since the job started the job will stop
  var jobID = 0

}