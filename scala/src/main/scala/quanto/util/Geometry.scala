package quanto.util

import math.{min,max}

object Geometry {
  type Point = (Double,Double)
  def bounds(ps: Iterable[Point]): Option[(Point,Point)] = {
    val it = ps.iterator
    if (it.hasNext) {
      var upper,lower = it.next()
      for (p <- it) {
        lower = (min(lower._1,p._1),min(lower._2,p._2))
        upper = (max(upper._1,p._1),max(upper._2,p._2))
      }
      Some(lower,upper)
    } else None
  }
}
