package quanto.gui

import quanto.data.Theory
import scala.swing.BorderPanel

class SynthPanel(val theory: Theory, val readOnly: Boolean = false)
  extends BorderPanel
  with HasDocument
{
  val document = new SynthDocument(this, theory)


}
