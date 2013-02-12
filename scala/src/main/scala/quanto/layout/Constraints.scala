package quanto.layout

import quanto.data._

class ConstraintException(msg: String) extends Exception(msg)

/**
 * A mixin for GraphLayouts which provides distance-based constraint functionality as in
 *   [1] "Scalable, Versatile and Simple Constrained Graph Layout", Dwyer 2009
 */
trait Constraints extends GraphLayout {
  var constraintIterations = 10
  val constraints = new ConstraintSeq

  def projectConstraints() {
    for (_ <- 1 to constraintIterations; c <- constraints) {
      val (p1,p2) = (coord(c.v1), coord(c.v2))

      val ((dx,dy), direction) = c.direction match {
        case Some(dir) =>
          val (dx,dy) = ((p2._1 - p1._1) * dir._1, (p2._2 - p1._2) * dir._2)
          //val len = math.sqrt(dx*dx + dy*dy)
          ((dx,dy), dir)
        case None =>
          val (dx,dy) = (p2._1 - p1._1, p2._2 - p1._2)
          val len = math.sqrt(dx*dx + dy*dy)
          ((dx,dy), if (len != 0) (dx/len, dy/len) else (1.0,0.0))
      }

      val ideal = (direction._1 * c.length, direction._2 * c.length)

      val shiftX =
        if ((c.order ==  0 && dx != ideal._1) ||
            (c.order == -1 && dx >  ideal._1) ||
            (c.order ==  1 && dx <  ideal._1))
        dx - ideal._1
        else 0
      val shiftY =
        if ((c.order ==  0 && dy != ideal._2) ||
            (c.order == -1 && dy >  ideal._2) ||
            (c.order ==  1 && dy <  ideal._2))
        dy - ideal._2
        else 0

      setCoord(c.v1, (p1._1 + (0.5 * shiftX), p1._2 + (0.5 * shiftY)))
      setCoord(c.v2, (p2._1 - (0.5 * shiftX), p2._2 - (0.5 * shiftY)))
    }
  }
}

class ConstraintSeq extends Iterable[Constraint] {
  private val cs = collection.mutable.ListBuffer[() => Iterable[Constraint]]()
  def clear() { cs.clear() }
  def +=(c: Constraint) { cs += (() => Seq(c)) }
  def ++=(cf: => Iterable[Constraint]) { cs += (() => cf) }

  def iterator = cs.iterator.map(x => x().iterator).foldLeft(Iterator[Constraint]())(_ ++ _)
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