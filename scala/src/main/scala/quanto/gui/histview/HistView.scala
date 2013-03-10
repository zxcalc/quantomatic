package quanto.gui.histview

import swing._
import quanto.util._
import java.awt.{Font => AWTFont,FontMetrics,Graphics}


class HistView[A](data: TreeSeq[A]) extends ListView[(Seq[TreeSeq.Decoration[A]],A)](data.flatten) {
  var itemWidth = -1
  def computeItemWidth() {
    val gr = peer.getGraphics
    for ((dec,value) <- listData) {
      val bds = gr.getFontMetrics(HistView.ItemFont).getStringBounds(value.toString, gr)
      val width = (TreeSeq.decorationWidth(dec) + 1) * HistView.xIncrement + bds.getWidth
      itemWidth = math.max(itemWidth, width.toInt)
    }
  }

  renderer = new ListView.Renderer[(Seq[TreeSeq.Decoration[A]], A)] {
    def componentFor(list: ListView[_], isSelected: Boolean,
                     focused: Boolean, a: (Seq[TreeSeq.Decoration[A]], A), index: Int): Component =
    {
      if (itemWidth == -1) computeItemWidth()
      new HistViewItem[A](a._1, a._2, isSelected, new Dimension(math.max(itemWidth,bounds.getWidth.toInt),30))
    }
  }
}

object HistView {
  final val ItemFont = new Font("Dialog", AWTFont.PLAIN, 12)
  final val xIncrement = 15.0
}
