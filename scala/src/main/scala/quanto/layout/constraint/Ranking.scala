package quanto.layout.constraint

import quanto.data._
import quanto.layout._

/**
 * Mix in to add ranking constraints to initialization
 */
trait Ranking extends Constraints {
  import Constraint.distance
  var rankSep: Double = 1.0

  override def initialize(g: Graph) {
    super.initialize(g)
    constraints.nextLayer()
    println("Ranking at layer " + constraints.currentLayer)

    val dag = g.dagCopy
    for (e <- dag.edges)
      constraints += {(distance from (dag.source(e)) to (dag.target(e)) along (0,1)) ~>= rankSep}
  }
}
