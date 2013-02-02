package quanto.layout

import quanto.util._
import quanto.data._


class ForceLayout extends GraphLayout {
  // repulsive force between vertices
  var charge = 30.0

  // (small) attractive force toward center of bounds
  var gravity = 0.1

  // Barnes-Hut approximation constant. Higher = coarser
  var theta = 0.8

  // used in Verlet integration
  var friction = 0.9

  // step size. recomputed on-the-fly using trust-region heuristic
  var alpha: Double = _

  // compute the equivalent point charge for every region of space in the quad tree
  def computeCharges(tr: QuadTree[(VName,Double)]): QuadTree[Double] = tr match {
    case leaf: QuadLeaf[_] => leaf.asInstanceOf[QuadLeaf[(VName,Double)]].mapValue(x => x._2)
    case node: QuadNode[_] =>
      val nCharge = node.value match { case Some((_,c)) => c ; case None => 0.0 }
      val nw = computeCharges(node.nw)
      val ne = computeCharges(node.ne)
      val sw = computeCharges(node.sw)
      val se = computeCharges(node.se)
      val (p,totalCharge) = Iterator(nw,ne,sw,se).foldLeft((node.p._1 * nCharge, node.p._2 * nCharge), nCharge) {
        case ((pSum,cSum), child) =>
          val c = child.value.getOrElse(0.0)
          ((child.p._1 * c + pSum._1, child.p._2 * c + pSum._2), c + cSum)
      }

      val center = if (totalCharge != 0.0) (p._1 / totalCharge, p._2 / totalCharge) else (0.0,0.0)
      QuadNode(node.x1,node.y1,node.x2,node.y2,Some(totalCharge),center,nw,ne,sw,se)
  }

  // take an unconstrained step in the direction of steepest descent
  def relax() {

  }

  def compute() {

  }
}
