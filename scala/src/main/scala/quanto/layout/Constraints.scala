package quanto.layout

import quanto.data._

class ConstraintException(msg: String) extends Exception(msg)

/**
 * A mixin for GraphLayouts which provides distance-based constraint functionality as in
 *   [1] "Scalable, Versatile and Simple Constrained Graph Layout", Dwyer 2009
 */
trait Constraints extends GraphLayout {
  def alpha: Double // cooling factor for soft constraints

  var constraintIterations = 10
  val constraints = new ConstraintSeq

  def projectConstraints() {
    var feasible = false // flag for all constraints satisfied
    var maxLayer = constraints.currentLayer
    var iteration = 0

    while (!feasible && maxLayer >= 0) {
      feasible = true
      if (iteration > constraintIterations) {
        maxLayer -= 1
        iteration = 0
      }

      for ((constraint,layer) <- constraints; if layer <= maxLayer) {
        val (p1,p2) = (coord(constraint.v1), coord(constraint.v2))

        val shift = constraint.direction match {
          case Some(dir) =>
            val (dx,dy) = ((p2._1 - p1._1) * dir._1, (p2._2 - p1._2) * dir._2)
            val ideal = (dir._1 * constraint.length, dir._2 * constraint.length)

            (
              if ((constraint.order ==  0 && dx != ideal._1) ||
                  (constraint.order == -1 && dx >  ideal._1) ||
                  (constraint.order ==  1 && dx <  ideal._1))
                dx - ideal._1
              else 0,

              if ((constraint.order ==  0 && dy != ideal._2) ||
                  (constraint.order == -1 && dy >  ideal._2) ||
                  (constraint.order ==  1 && dy <  ideal._2))
                dy - ideal._2
              else 0
            )

          case None =>
            val (dx,dy) = (p2._1 - p1._1, p2._2 - p1._2)
            val length = math.sqrt(dx*dx + dy*dy)
            val dir = if (length != 0) (dx/length, dy/length) else (1.0,0.0)

            if ((constraint.order ==  0 && length != constraint.length) ||
                (constraint.order == -1 && length >  constraint.length) ||
                (constraint.order ==  1 && length <  constraint.length))
              (dir._1 * (length - constraint.length), dir._2 * (length - constraint.length))
            else (0.0,0.0)
        }

        val (shiftX, shiftY) = shift match { case (x,y) => if (constraint.soft) (x * alpha, y * alpha) else (x,y) }

        if (shiftX != 0.0 || shiftY != 0.0) {
          feasible = false
          setCoord(constraint.v1, (p1._1 + (constraint.mv1 * shiftX), p1._2 + (constraint.mv1 * shiftY)))
          setCoord(constraint.v2, (p2._1 - (constraint.mv2 * shiftX), p2._2 - (constraint.mv2 * shiftY)))
        }
      }

      iteration += 1
    }

//    if (feasible) {
//      println("feasible solution found after " + iteration + " iterations")
//    } else {
//      println("no feasible solution")
//    }
  }
}

class ConstraintSeq extends Iterable[(Constraint,Int)] {
  private var _currentLayer = 0
  def currentLayer = _currentLayer
  private val cs = collection.mutable.ListBuffer[() => Iterator[(Constraint,Int)]]()

  def nextLayer() { _currentLayer += 1 }
  def clear() { cs.clear() }

  def +=(c: Constraint) {
    val layer = _currentLayer
    cs += (() => Iterator((c,layer)))
  }

  def ++=(cf: => Iterable[Constraint]) {
    val layer = _currentLayer
    cs += (() => cf.iterator.zip(Iterator.continually(layer)))
  }

  def iterator = cs.iterator.map(x => x()).foldLeft(Iterator[(Constraint,Int)]())(_ ++ _)
}

case class Constraint(v1: VName, v2: VName, direction: Option[(Double,Double)], length: Double, w1: Double, w2: Double, order: Int, soft: Boolean) {
  lazy val mv1 = if (w1 + w2 != 0.0) w2 / (w1 + w2) else 0.5
  lazy val mv2 = 1.0 - mv1
}


object Constraint {
  case class DistanceExpr(v1: VName, v2: VName, direction: Option[(Double,Double)] = None, w1: Double = 1.0, w2: Double = 1.0) {
    def along(dir: (Double,Double)) = copy(direction = Some(dir))
    def weighted(w: (Double,Double)) = copy(w1 = w._1, w2 = w._2)

    private def normalizedDir = direction.map {
      case (0.0,0.0) => throw new ConstraintException("'along' direction must be a non-zero vector")
      case (x,y) =>
        val length = math.sqrt(x*x + y*y)
        (x / length, y / length)
    }

    def <= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,-1,soft=false)
    def ===(len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,0,soft=false)
    def >= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,1,soft=false)

    def ~<= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,-1,soft=true)
    def ~==(len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,0,soft=true)
    def ~>= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,1,soft=true)
  }

  object distance {
    def from(v1: VName) = new AnyRef { def to(v2: VName) = DistanceExpr(v1,v2) }
  }
}