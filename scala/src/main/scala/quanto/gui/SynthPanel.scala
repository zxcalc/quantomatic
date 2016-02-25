package quanto.gui

import quanto.data.{GraphRef, Theory}
import scala.swing._
import scala.swing.event.Event
import quanto.gui.graphview.GraphView
import quanto.layout.ForceLayout
import quanto.layout.constraint.{Clusters, Ranking}

case class SynthReady() extends Event

class SynthPanel(val theory: Theory, val readOnly: Boolean = false)
  extends BorderPanel
  with HasDocument
{
  val document = new SynthDocument(this, theory)
  listenTo(document)

  val SynthOutput = new BoxPanel(Orientation.Vertical)
  add(new ScrollPane(SynthOutput), BorderPanel.Position.Center)


  reactions += {
    case SynthReady() =>
      SynthOutput.contents += new Label("Showing synthesis for " + document.synth.classes.length + " equivalence classes...")
      SynthOutput.contents += new Separator
      SynthOutput.contents += new Separator
      var i = 0
      val lo = new ForceLayout with Ranking with Clusters
      lo.maxIterations = 200

      for (c <- document.synth.classes) {
        SynthOutput.contents += new Label("Class " + i)
        val rep = new BoxPanel(Orientation.Horizontal)
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
          congs.contents += new GraphView(theory, new GraphRef(lo.layout(gr)))
        SynthOutput.contents += congs
        SynthOutput.contents += new Separator

        SynthOutput.contents += new Separator
        SynthOutput.contents += new Separator
        i += 1
      }

  }

}
