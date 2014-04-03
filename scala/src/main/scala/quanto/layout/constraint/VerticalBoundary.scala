package quanto.layout.constraint

import quanto.layout._
import quanto.data._

trait VerticalBoundary extends Constraints {
  import Constraint.distance
  
  override def initialize(g: Graph, randomCoords: Boolean = true) {
    super.initialize(g, randomCoords)
    constraints.nextLayer()
    //println("VerticalBoundary at layer " + constraints.currentLayer)
    
    for (bnd <- Iterator(g.inputs,g.outputs); if !bnd.isEmpty) {
      val it = bnd.iterator
      val v1 = it.next()
      for (v2 <- it) {
//    	if (g.isBBoxed(v1) && !g.isBBoxed(v2)) {
//
//    	}
    	coord(v1)
        if (g.isBBoxed(v1) || g.isBBoxed(v2))
          constraints += { (distance from v1 to v2 along (0.0,1.0)) ~== 0.0 }
          
        
        else
          //constraints += { (distance from v1 to v2 along (0.0,1.0)) === 0.0 }
          constraints += { (distance from v1 to v2 along (0.0,1.0)) ~== 0.0 }
        //  v1 = v2
      }
    }

    g.edges.foreach { e =>
      val (s,t) = (g.source(e), g.target(e))
      if (g.isInput(s) || g.isOutput(t)) {
        if (g.isBBoxed(s) || g.isBBoxed(t)){
          constraints += { (distance from s to t along (1.0,0.0)) ~== 0.0 }
        }
          
        else
          constraints += { (distance from s to t along (1.0,0.0)) ~== 0.0 }
        
      }
    }
  }
  
//  override def projectConstraints(){
//	  
//    
//  }
  
}
