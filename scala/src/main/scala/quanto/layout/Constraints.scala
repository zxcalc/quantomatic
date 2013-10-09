package quanto.layout

import quanto.data._
import quanto.data.Graph
import quanto.data.VName

class ConstraintException(msg: String) extends Exception(msg)

/**
 * A mixin for GraphLayouts which provides distance-based constraint functionality as in
 *   [1] "Scalable, Versatile and Simple Constrained Graph Layout", Dwyer 2009
 */
trait Constraints extends GraphLayout {
  def alpha: Double // cooling factor for soft constraints
  var constraintIterations = 10
  var bug = false
  val constraints = new ConstraintSeq
  def isConstraintSatisfied (c: Constraint): Boolean = {
    val (v1x,v1y) = coord(c.v1)
    val (v2x,v2y) = coord(c.v2)
    val (v1v2x,v1v2y) = (v2x-v1x, v2y-v1y)
    
    val dir = c.direction match  {
      case Some(dir) => dir
      case None => (0.0,0.0)
    }
    
    val length = projectLength((v1v2x,v1v2y),dir)
    if (c.order == 0){
    	return projectLength((v1v2x,v1v2y),dir) == c.length
    }
    
    else if(c.order > 0){
    	return projectLength((v1v2x,v1v2y),dir) >= c.length
    }
    
    else {
    	return projectLength((v1v2x,v1v2y),dir) <= c.length
    }
    
  }
  // project v1 on v2 // direction is unit vector
  def projectVector (v1 :(Double,Double), v2 : (Double,Double)) : (Double,Double)= {
    	val length = vectorProduct(v1,v2)
    	(length * v2._1, length * v2._2)
  }

  def projectLength (v1 :(Double,Double), v2 : (Double,Double)) : Double = {
    	val length = vectorProduct(v1,v2)
    	length
  }

  def vectorProduct (v1 : (Double,Double), v2 : (Double, Double)) : Double = {
    	(v1._1*v2._1+v1._2*v2._2)
  }
  
  
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
           // val (dx,dy) = ((p2._1 - p1._1) * dir._1, (p2._2 - p1._2) * dir._2)

            // the coordinates of vector projected on direction
            val p1p2 = ((p2._1 - p1._1),(p2._2 - p1._2))
            val (dx,dy) = projectVector((p1p2._1, p1p2._2), dir)
            
            // Add direction to the distance
            // if the angle is acute angle then we need to do nothing
            // if the angle is obtuse angle then we need to move on of the vertex.
           if (vectorProduct(p1p2,dir)<0) {
              // swap the two nodes
//            	val temp = coord(constraint.v1);
//            	setCoord(constraint.v1, coord(constraint.v2))
//            	setCoord(constraint.v2,temp)
             // or we can just move p2 to another side
	              val (nx,ny) = (2*p1._1 - p2._1, 2*p1._2 - p2._2);
	              setCoord(constraint.v2, (nx,ny))
//              
            }
//            // if it is 0 then they are perpendicular
            // this constraint cannot be projected.
            if ((vectorProduct(p1p2,dir)==0.0)&& !(isConstraintSatisfied(constraint))){
            	if (bug) println("constraint " + constraint)
            	if (bug) println("vector p1 p2 is " + p1p2)
            	if (bug) println("vector direction " + dir)
            	
            	val (v2x,v2y) = coord(constraint.v2)
            	setCoord(constraint.v2, (v2x+dir._1/50,v2y+dir._2/50))
            }
            
            val ideal = (dir._1 * constraint.length, dir._2 * constraint.length)
            (
              if ((constraint.order ==  0 && dx != ideal._1) ||
                  (constraint.order == -1 && dx >  ideal._1) ||
                  (constraint.order ==  1 && dx <  ideal._1))
              //if (!isConstraintSatisfied(constraint))
                   
                dx - ideal._1
                //ideal._1 - dx
              else 0,

              if ((constraint.order ==  0 && dy != ideal._2) ||
                  (constraint.order == -1 && dy >  ideal._2) ||
                  (constraint.order ==  1 && dy <  ideal._2))
                dy - ideal._2
                //ideal._2 - dy
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
							// order: which relation with d > < or =
case class Constraint(v1: VName, v2: VName, direction: Option[(Double,Double)], length: Double, w1: Double, w2: Double, order: Int, soft: Boolean) {
  lazy val mv1 = if (w1 + w2 != 0.0) w2 / (w1 + w2) else 0.5
  lazy val mv2 = 1.0 - mv1
  
}


object Constraint {
  object distance {
    def from(v1: VName) = new DistanceFromExpr(v1)
  }
  
  class DistanceFromExpr(v1: VName) {
    def to(v2: VName) = DistanceExpr(v1,v2)
  }
  
  case class DistanceExpr(v1: VName, v2: VName, direction: Option[(Double,Double)] = None, w1: Double = 1.0, w2: Double = 1.0) {
    def along(dir: (Double,Double)) = copy(direction = Some(dir))
    
    def weighted(w: (Double,Double)) = copy(w1 = w._1, w2 = w._2)

    private def normalizedDir = direction.map {
      case (0.0,0.0) => throw new ConstraintException("'along' direction must be a non-zero vector")
      case (x,y) =>
        val length = math.sqrt(x*x + y*y)
        // return the normal
        (x / length, y / length)
    }
    
    
    def <= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,-1,soft=false)
    def ===(len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,0,soft=false)
    def >= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,1,soft=false)

    def ~<= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,-1,soft=true)
    def ~== (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,0,soft=true)
    def ~>= (len: Double) = Constraint(v1,v2,normalizedDir,len,w1,w2,1,soft=true)
  }
}