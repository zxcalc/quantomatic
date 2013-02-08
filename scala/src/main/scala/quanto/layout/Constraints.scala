package quanto.layout

import quanto.data._

class ConstraintException(msg: String) extends Exception(msg)

/**
 * A mixin for GraphLayouts which provides distance-based constraint functionality as in
 *   [1] "Scalable, Versatile and Simple Constrained Graph Layout", Dwyer 2009
 */
trait Constraints extends GraphLayout {
  var constraintIterations = 10
  var constraints = Seq[Constraint]()

  def projectConstraints() {
    for (_ <- 1 to constraintIterations; c <- constraints) {
      val (p1,p2) = (coord(c.v1), coord(c.v2))
      val (dx,dy) = (p2._1 - p1._1, p2._2 - p1._2)
      val length = math.sqrt(dx*dx + dy*dy)

      if ((c.order ==  0 && length != c.length) ||
          (c.order == -1 && length >  c.length) ||
          (c.order ==  1 && length <  c.length))
      {
        val offset = (c.length - length) / 2
        val shift = c.direction match {
          case Some(d) => (offset * d._1, offset * d._2)
          case None => if (length != 0) (offset * dx/length, offset * dy/length) else (offset,0.0)
        }

        setCoord(c.v1, (p1._1 - shift._1, p1._2 - shift._2))
        setCoord(c.v2, (p2._1 - shift._1, p2._2 - shift._2))
      }
    }
  }
}

case class Constraint(v1: VName, v2: VName, direction: Option[(Double,Double)], length: Double, order: Int)

object Constraint {
  case class DistanceExpr(v1: VName, v2: VName, direction: Option[(Double,Double)] = None) {
    def along(dir: (Double,Double)) = copy(direction = Some(dir))

    private def normalizedDir = direction.map {
      case (0.0,0.0) => throw new ConstraintException("'along' direction must be a non-zero vector")
      case (x,y) =>
        val length = math.sqrt(x*x + y*y)
        (x / length, y / length)
    }

    def <= (len: Double) = Constraint(v1,v2,normalizedDir,len,-1)
    def <=>(len: Double) = Constraint(v1,v2,normalizedDir,len,0)
    def >= (len: Double) = Constraint(v1,v2,normalizedDir,len,1)
  }

  object distance {
    def from(v1: VName) = new AnyRef { def to(v2: VName) = DistanceExpr(v1,v2) }
  }
}