package quanto.gui

import java.io.File
import java.util.Calendar
import java.util.concurrent.TimeUnit

import quanto.rewrite.Simproc
import quanto.util.UserOptions.scaleInt
import quanto.util.swing.ToolBar
import quanto.data.Graph

import scala.swing.event.{ButtonClicked, Event, SelectionChanged}
import quanto.cosy.{SimprocBatch, SimprocBatchResult, SimprocSingleRun}
import quanto.util.FileHelper

import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dimension, GridPanel, Label, Orientation, Publisher, ScrollPane, Swing}

class BatchDerivationResultsPanel()
  extends BorderPanel with HasDocument with Publisher {

  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val Toolbar = new ToolBar {
    //contents
  }

  val document: BatchDerivationResultsDocument = new BatchDerivationResultsDocument(this)
  val LabelNumSimprocs = new Label("Calculating")
  val LabelSimprocsUsed = new Label("Calculating")
  val LabelNumGraphs = new Label("Calculating")
  val LabelNumPairs = new Label("Calculating")
  val LabelTimeTaken = new Label("Calculating")
  val SimprocsUsed: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += VSpace
    contents += new Label("Simprocs used:")
    contents += VSpace
    contents += LabelSimprocsUsed
    contents += VSpace
  }
  val BatchDetails: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += VSpace
    contents += new GridPanel(3, 2) {

      contents += new Label("No. Simprocs:")
      contents += LabelNumSimprocs
      contents += new Label("No. Graphs:")
      contents += LabelNumGraphs
      contents += new Label("Total pairs:")
      contents += LabelNumPairs
    }
    contents += VSpace
    maximumSize = new Dimension(scaleInt(400), scaleInt(200))
  }
  val NotesTextBox = new Label(document.notes.getOrElse(""))
  val Notes: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += VSpace
    contents += new Label("Notes:")
    contents += VSpace
    contents += NotesTextBox
    contents += VSpace
    NotesTextBox.preferredSize = new Dimension(scaleInt(600), scaleInt(300))
  }
  val MainPanel: BoxPanel = new BoxPanel(Orientation.Vertical) {
    contents += SimprocsUsed
    contents += BatchDetails
    contents += Notes
  }
  val TopScrollablePane = new ScrollPane(MainPanel)

  def allSimprocs: Option[Map[String, String]] = document.allSimprocs

  def results: Option[List[SimprocSingleRun]] = document.results

  def notes: Option[String] = document.notes

  def refreshData(): Unit = {
    val numSimprocs: Int = simprocsUsed.getOrElse(List()).size
    val numPairs: Int = resultsCount.getOrElse(0)
    LabelNumSimprocs.text = numSimprocs.toString
    LabelSimprocsUsed.text = simprocsUsed.getOrElse(List()).mkString("\n")
    LabelNumPairs.text = numPairs.toString
    LabelNumGraphs.text = (numPairs / numSimprocs).toString
    LabelTimeTaken.text = if (timeTaken.nonEmpty) {
      timeTaken.get.toString
    } else {
      "---"
    }
    NotesTextBox.text = notes.getOrElse("")
  }

  listenTo(document)
  reactions += {
    case DocumentChanged(d) => refreshData()
  }

  def simprocsUsed: Option[List[String]] = document.simprocsUsed

  def resultsCount: Option[Int] = document.resultsCount

  def timeTaken: Option[Double] = document.timeTaken

  private def HSpace: Component = Swing.HStrut(scaleInt(10))


  private def VSpace: Component = Swing.VStrut(scaleInt(10))

  add(TopScrollablePane, BorderPanel.Position.Center)

  add(Toolbar, BorderPanel.Position.North)

}
