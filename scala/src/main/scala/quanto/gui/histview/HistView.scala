package quanto.gui.histview

import swing._
import quanto.util._
import java.awt.{FontMetrics, Graphics, Font => AWTFont}

import scala.swing.ListView.IntervalMode


class HistView[A <: HistNode](data: TreeSeq[A]) extends ListView[(Seq[TreeSeq.Decoration[A]],A)](data.flatten) {
  private def scale(d: Double) : Double = UserOptions.scale(d)
  private def scaleInt(n: Int) : Int = UserOptions.scaleInt(n)
  private var itemWidth = -1 // -1 used as "to initialise" value
  private var _treeData = data
  selection.intervalMode = IntervalMode.Single // multiple selection doesn't make sense for history view

  def treeData = _treeData
  def treeData_=(data: TreeSeq[A]) {
    _treeData = data
    itemWidth = -1
    listData = data.flatten
  }

  def computeItemWidth() {
    val gr = peer.getGraphics
    for ((dec,value) <- listData) {
      val bds = gr.getFontMetrics(HistView.ItemFont).getStringBounds(value.toString, gr)
      val width = (TreeSeq.decorationWidth(dec) + scaleInt(1)) * HistView.xIncrement + bds.getWidth
      itemWidth = math.max(itemWidth, width.toInt)
    }
  }

  renderer = new ListView.Renderer[(Seq[TreeSeq.Decoration[A]], A)] {
    override def componentFor(list: ListView[_ <: (Seq[TreeSeq.Decoration[A]], A)], isSelected: Boolean, focused: Boolean, a: (Seq[TreeSeq.Decoration[A]], A), index: Int): Component =
    {
        if (itemWidth == -1) computeItemWidth()
        new HistViewItem[A](a._1, a._2, isSelected, new Dimension(itemWidth,scaleInt(30)))
    }
  }

  def selectedNode_=(aOpt: Option[A]) = aOpt match {
    case Some(a) =>
      val i = treeData.indexOf(a)
      if (i != -1) selectIndices(i)
      else selectIndices()
    case None =>
      selectIndices()
  }

  def selectedIndex() : Int = {
    peer.getSelectedIndex
  }

  def selectedNode: Option[A] =
    if (selection.indices.isEmpty) None
    else Some(treeData.toSeq(selection.indices.head))
}

object HistView {
  final def ItemFont = new Font("Dialog", AWTFont.PLAIN, UserOptions.fontSize)
  final def xIncrement = UserOptions.scaleInt(15)
}
