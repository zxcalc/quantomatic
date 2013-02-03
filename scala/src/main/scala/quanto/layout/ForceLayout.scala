package quanto.layout

import quanto.util._
import quanto.data._


class ForceLayout extends GraphLayout {
  // repulsive force between vertices
  var charge = -1.0

  // spring strength on edges
  var strength = 1.0

  // preferred length of edge
  var edgeLength = 0.5

  // (small) attractive force toward center of bounds
  var gravity = 0.1

  // Barnes-Hut approximation constant. Higher = coarser
  var theta = 0.8

  // used in Verlet integration
  var friction = 0.9

  // step size. recomputed on-the-fly using trust-region heuristic
  var alpha: Double = _

  // compute the equivalent point charge for every region of space in the quad tree
  def computeCharges(tr: QuadTree[(Option[VName],Double)]): QuadTree[(Option[VName],Double)] = tr match {
    case leaf: QuadLeaf[_] => leaf
    case _: QuadNode[_] =>
      val node = tr.asInstanceOf[QuadNode[(Option[VName],Double)]]

      val (v,nCharge) = node.value.getOrElse((None,0.0))
      val nw = computeCharges(node.nw)
      val ne = computeCharges(node.ne)
      val sw = computeCharges(node.sw)
      val se = computeCharges(node.se)
      val (p,totalCharge) = Iterator(nw,ne,sw,se).foldLeft((node.p._1 * nCharge, node.p._2 * nCharge), nCharge) {
        case ((pSum,cSum), child) =>
          val (_,c) = child.value.getOrElse((None,0.0))
          ((child.p._1 * c + pSum._1, child.p._2 * c + pSum._2), c + cSum)
      }

      val center = if (totalCharge != 0.0) (p._1 / totalCharge, p._2 / totalCharge) else (0.0,0.0)
      QuadNode(node.x1,node.y1,node.x2,node.y2,Some((v,totalCharge)),center,nw,ne,sw,se)
  }

  // take an unconstrained step in the direction of steepest descent in energy
  def relax() {
    // apply spring forces
    for (e <- graph.edges) {
      val sp = coord(graph.source(e))
      val tp = coord(graph.target(e))
      val (dx,dy) = (tp._1 - sp._1, tp._2 - sp._2)
      val d2 = dx*dx + dy*dy
      if (d2 != 0.0) {
        val k = (alpha * strength * (math.sqrt(d2) - edgeLength)) / d2
        val shift = (dx * k, dy * k)
        setCoord(graph.source(e), (sp._1 + shift._1, sp._2 + shift._2))
        setCoord(graph.target(e), (tp._1 - shift._1, tp._2 - shift._2))
      }
    }

    // apply gravity
    for (v <- graph.verts) {
      val p = coord(v)
      setCoord(v, (
        p._1 * (1 - alpha * gravity),
        p._2 * (1 - alpha * gravity)
      ))
    }

    // compute charges
    val quad = computeCharges(QuadTree(graph.verts.toSeq.map { v => (coord(v), (Some(v),charge)) }))

    // apply charge forces
    for (v <- graph.verts) {
      var p = coord(v)

      quad.visit { nd =>
        nd.value match {
          case Some((optV,nodeCharge)) =>
            val (dx,dy) = (nd.p._1 - p._1, nd.p._2 - p._2)
            val d2 = dx*dx + dy*dy

            if (d2 == 0.0) false
            else {
              // if the Barnes-Hut criterion is satisfied, act with the total charge of this region
              if ((nd.x2 - nd.x1) / math.sqrt(d2) < theta) {
                val k = nodeCharge / d2
                p = (p._1 - dx*k, p._2 - dy*k)
                true
              } else {
                // if !B-H, but there is a (different) vertex here, act with the point charge
                optV match {
                  case Some(v1) if v1 != v =>
                    val k = charge / d2
                    p = (p._1 - dx*k, p._2 - dy*k)
                  case _ =>
                }

                false
              }
            }
          case None => false
        }
      }

      setCoord(v, p)
    }

  }

  def compute() {
    alpha = 1.0
    while (alpha > 0.005) {
      alpha *= 0.99
      relax()
    }
  }
}
