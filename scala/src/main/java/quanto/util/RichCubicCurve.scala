package quanto.util

import java.awt.geom.CubicCurve2D

class RichCubicCurve(curve: CubicCurve2D) {
  import RichCubicCurve._
  def pointAt(dist: Double) = (
    bezierInterpolate(dist, curve.getX1, curve.getCtrlX1, curve.getCtrlX2, curve.getX2),
    bezierInterpolate(dist, curve.getY1, curve.getCtrlY1, curve.getCtrlY2, curve.getY2)
  )
}

object RichCubicCurve {
  def bezierInterpolate(dist: Double, c0: Double, c1: Double, c2: Double, c3: Double) = {
    val distp = 1 - dist

    (distp*distp*distp) * c0 +
      3.0 * (distp*distp) * dist * c1 +
      3.0 * (dist*dist) * distp * c2 +
      (dist*dist*dist) * c3
  }

  implicit def cubicCurveToRichCubicCurve(curve: CubicCurve2D): RichCubicCurve = new RichCubicCurve(curve)
}
