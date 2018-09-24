package quanto.util

import scala.math.{max, min}

class RichPt(val p: Geometry.Pt) {
  def +(p1: Geometry.Pt): (Double, Double) = (p._1 + p1._1, p._2 + p1._2)

  def -(p1: Geometry.Pt): (Double, Double) = (p._1 - p1._1, p._2 - p1._2)

  def unary_-(p1: Geometry.Pt): (Double, Double) = (-p._1, -p._2)
}

object RichPt {
  implicit def richPtToPt(rp: RichPt): (Double, Double) = rp.p

  implicit def ptToRichPt(p: Geometry.Pt): RichPt = new RichPt(p)
}

class RichRect(val r: Geometry.Rect) {
  def pad(padding: Double): ((Double, Double), (Double, Double)) =
    r match {
      case (((lx, ly), (ux, uy))) =>
        ((lx - padding, ly - padding), (ux + padding, uy + padding))
    }

  def coords: ((Double, Double), (Double, Double)) = (r._1, r._2)

  def center: Geometry.Pt = r match {
    case (((lx, ly), (ux, uy))) => ((lx + ux) / 2.0, (ly + uy) / 2.0)
  }

  def size: (Double, Double) = (width, height)

  def width: Double = r match {
    case ((lx, ux), _) => ux - lx
  }

  def height: Double = r match {
    case (_, (ly, uy)) => uy - ly
  }
}

object RichRect {
  implicit def richRectToRect(rr: RichRect): ((Double, Double), (Double, Double)) = rr.r

  implicit def rectToRichRect(r: Geometry.Rect): RichRect = new RichRect(r)
}

object Geometry {
  type Pt = (Double, Double)
  type Rect = (Pt, Pt)

  def bounds(ps: Iterable[Pt]): Option[(Pt, Pt)] = {
    val it = ps.iterator

    if (it.hasNext) {
      var upper = it.next()
      var lower = upper
      for (p <- it) {
        lower = (min(lower._1, p._1), min(lower._2, p._2))
        upper = (max(upper._1, p._1), max(upper._2, p._2))
      }
      Some(lower, upper)
    } else None
  }

  // implicit conversions for various geometric classes
  implicit def ptToPoint(p: Pt): java.awt.Point = new java.awt.Point(p._1.toInt, p._2.toInt)

  implicit def ptToPoint2D(p: Pt): java.awt.geom.Point2D = new java.awt.geom.Point2D.Double(p._1, p._2)

  implicit def pointToPt(p: java.awt.Point): Pt = (p.getX, p.getY)

  implicit def point2DToPt(p: java.awt.geom.Point2D): Pt = (p.getX, p.getY)
}
