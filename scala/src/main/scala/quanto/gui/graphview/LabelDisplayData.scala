package quanto.gui.graphview

import java.awt.geom.Rectangle2D
import java.awt.{FontMetrics,Color}
import math._

case class LabelDisplayData(
  text: String,
  bounds: Rectangle2D,
  baseline: Double,
  foregroundColor: Color = Color.BLACK,
  backgroundColor: Option[Color] = None)

object LabelDisplayData {
  def apply(text: String,
            centerPoint: (Double,Double),
            fm: FontMetrics,
            foregroundColor: Color,
            backgroundColor: Option[Color]): LabelDisplayData =
  {
    val (x,y) = centerPoint
    val arr = text.toCharArray
    val h = fm.getHeight.toDouble
    //val w = max(fm.charsWidth(arr, 0, arr.size).toDouble, h)
    val w = fm.charsWidth(arr, 0, arr.size).toDouble
    val bds = new Rectangle2D.Double(x - (w/2), y - (h/2), w, h)
    LabelDisplayData(text, bds, bds.getMaxY - fm.getDescent, foregroundColor, backgroundColor)
  }
}
