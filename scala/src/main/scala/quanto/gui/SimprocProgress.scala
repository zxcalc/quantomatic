package quanto.gui

import quanto.{cosy, data}
import quanto.cosy.{AutoReduce, SimplificationInternalState, SimplificationProcedure}
import quanto.data.Derivation.DerivationWithHead

import scala.swing._
import scala.swing.event.ButtonClicked
import quanto.data._

import scala.util.{Failure, Success}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class SimprocProgress[T <: SimplificationInternalState](
                                                         project: Project,
                                                         actionName: String,
                                                         simplificationProcedure: SimplificationProcedure[T]
                                                       ) extends Dialog {

  modal = true
  title = actionName

  val StopButton = new Button("Stop")
  val ProgressBarCompletion = new ProgressBar
  val ProgressLabel = new Label("Initialising")
  val ProgressGraphSizeLabel = new Label("Initialising")
  ProgressBarCompletion.value = 0
  ProgressBarCompletion.min = 0
  ProgressBarCompletion.max = 100
  val MainPanel = new BoxPanel(Orientation.Vertical) {
    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += ProgressLabel
    }
    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(10)
      contents += ProgressBarCompletion
      contents += Swing.HStrut(10)
    }
    contents += Swing.VStrut(10)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += ProgressGraphSizeLabel
    }
    contents += Swing.VStrut(10)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += StopButton
    }
    contents += Swing.VStrut(10)
  }


  var returningDerivation: DerivationWithHead = simplificationProcedure.initialDerivation
  println("created future")
  val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
    println ("future started")
    while (!simplificationProcedure.stopped) {
      println ("futured loop")
      simplificationProcedure.step()
      Swing.onEDT {
        returningDerivation = simplificationProcedure.current
        val progress = simplificationProcedure.progress
        if (progress._1.nonEmpty) {
          ProgressLabel.visible = true
          ProgressLabel.text = simplificationProcedure.progress._1.getOrElse("")
        } else {
          ProgressLabel.visible = false
        }
        if (progress._2.nonEmpty) {
          ProgressBarCompletion.visible = true
          ProgressBarCompletion.value = (simplificationProcedure.progress._2.getOrElse(0.0) * 100).floor.toInt
        } else {
          ProgressBarCompletion.visible = false
        }
        val graphSize = data.Derivation.derivationHeadPairToGraph(simplificationProcedure.current).verts.size
        ProgressGraphSizeLabel.text = "Graph size: " + graphSize
      }
    }
    simplificationProcedure.current
  }
  backgroundDerivation onComplete {
    case Success(_) => Swing.onEDT {
      close()
    }
    case Failure(e) => Swing.onEDT {
      println("Error: " + e.getMessage)
      close()
    }
  }

  contents = MainPanel
  listenTo(StopButton)

  reactions += {
    case ButtonClicked(StopButton) =>
      simplificationProcedure.stop()
      close()
  }
}