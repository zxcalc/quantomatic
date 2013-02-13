package quanto.layout.constraint

import quanto.layout._
import quanto.data._

trait VerticalBoundary extends Constraints {
  import Constraint.distance

  override def initialize(g: Graph) {
    super.initialize(g)
    constraints.nextLayer()
    println("VerticalBoundary at layer " + constraints.currentLayer)

    for (bnd <- Iterator(g.inputs,g.outputs); if !bnd.isEmpty) {
      val it = bnd.iterator
      var v1 = it.next()
      for (v2 <- it) {
        constraints += { (distance from v1 to v2 along (0.0,1.0)) === 0.0 }
        v1 = v2
      }
    }

    g.edges.foreach { e =>
      if (g.isInput(g.source(e)) || g.isOutput(g.target(e))) {
        constraints += { (distance from g.source(e) to g.target(e) along (1.0,0.0)) === 0.0 }
      }
    }
  }
}
