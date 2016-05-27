package quanto.gui

import quanto.data.{GraphRef, Theory}
import scala.swing._
import scala.swing.event.{ButtonClicked, Event}
import quanto.gui.graphview.GraphView
import quanto.layout.ForceLayout
import quanto.layout.constraint.{Clusters, Ranking}
import java.awt.Cursor

case class SynthReady() extends Event

class SynthPanel(val theory: Theory, val readOnly: Boolean = false)
  extends BorderPanel
  with HasDocument
{
  val document = new SynthDocument(this, theory)

  val showEmpty = new CheckBox("Show empty")
  add(showEmpty, BorderPanel.Position.North)

  listenTo(document, showEmpty)

  val SynthOutput = new BoxPanel(Orientation.Vertical)
  val SynthScroll = new ScrollPane(SynthOutput)
  add(SynthScroll, BorderPanel.Position.Center)


  reactions += {
    case ButtonClicked(_) =>
      document.publish(SynthReady())
    case SynthReady() =>
      SynthOutput.contents.clear()
      SynthOutput.contents += new Label("Showing synthesis for " + document.synth.classes.length + " equivalence classes...")
      SynthOutput.contents += new Separator
      SynthOutput.contents += new Separator
      var i = 0
      val lo = new ForceLayout //with Ranking with Clusters
      lo.maxIterations = 200

      for (c <- document.synth.classes)
        if (c.redexes.size > 0 || c.congs.size > 0 || showEmpty.selected) {
          SynthOutput.contents += new Label {
            text = "Class " + i + ", redexes: " + c.redexes.size + ", congruences: " + c.congs.size
            font = new Font("Ariel", java.awt.Font.ITALIC, 24)
          }


          val rep = new FlowPanel() //new BoxPanel(Orientation.Horizontal)
          val rows = c.data.split("\n")
          val maxcol = rows.maxBy(_.length)
          rep.contents += new TextArea(c.data, rows.length, maxcol.length)
          rep.contents += new GraphView(theory, new GraphRef(lo.layout(c.rep)))
          rep.contents += new Label("")
          SynthOutput.contents += rep

          SynthOutput.contents += new Separator

          SynthOutput.contents += new Label("redexes")
          val redexes = new FlowPanel()
          for ((_,gr) <- c.redexes)
            redexes.contents += new GraphView(theory, new GraphRef(lo.layout(gr)))
          SynthOutput.contents += redexes
          SynthOutput.contents += new Separator

          SynthOutput.contents += new Label("congruences")
          val congs = new FlowPanel()
          for ((_,gr) <- c.congs)
            congs.contents += new GraphView(theory, new GraphRef(gr))
          SynthOutput.contents += congs
          SynthOutput.contents += new Separator

          SynthOutput.contents += new Separator()
          i += 1
        }

      SynthOutput.revalidate()

  }

}
