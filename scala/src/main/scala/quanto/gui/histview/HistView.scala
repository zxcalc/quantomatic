package quanto.gui.histview

import swing._
import quanto.util._
import java.awt.{Font => AWTFont}
import scala.swing.Font


class HistView[A](data: TreeSeq[A]) extends ListView(data.flatten) {
  renderer = new ListView.Renderer[(Seq[TreeLink[A]], A)] {
    def componentFor(list: ListView[_], isSelected: Boolean,
                     focused: Boolean, a: (Seq[TreeLink[A]], A), index: Int): Component =
    {
      new HistViewItem[A](a._1, a._2, isSelected, list.peer.getGraphics.getFontMetrics(HistView.ItemFont))
    }
  }
}

object HistView {
  final val ItemFont = new Font("Dialog", AWTFont.PLAIN, 12)
}
