package quanto.layout

import quanto.data._
import quanto.layout.constraint._
import quanto.util._

import scala.math.abs

/**
  * Force-directed layout algorithm. Parts are based on:
  * [1] force.js from the D3 javascript library (see d3js.org)
  * [2] "Scalable, Versatile and Simple Constrained Graph Layout", Dwyer 2009
  * [3] "Efficient and High Quality Force-Directed Graph Drawing", Hu 2006
  */
class ForceLayout extends GraphLayout with Constraints {
  // repulsive force between vertices
  //var charge: VName => Double = (v => if (graph.vdata(v).isWireVertex) 3.0 else 5.0)
  var nodeCharge = 5.0
  // spring strength on edges
  var strength = 2.5
  // preferred length of edge
  var edgeLength = 0.5
  // (small) attractive force toward center of bounds
  var gravity = 1.0
  // Barnes-Hut approximation constant. Higher = coarser
  var theta = 0.8
  // used in Verlet integration
  var friction = 0.9
  // initial step size
  var alpha0: Double = 1.0
  // increase or decrease step size by this amount
  var alphaAdjust = 0.7
  // maximum iterations
  var maxIterations = 3000
  // re-center graph after each iteration
  var keepCentered = true
  // step size alpha is re-computed on the fly using trust region heuristic
  var alpha: Double = _
  var prevEnergy: Double = _
  var energy: Double = _
  var progress: Int = _
  var iteration = 0

  def charge(v: VName): Double = if (graph.vdata(v).isWireVertex && nodeCharge != 0.0) 1.0 else nodeCharge

  override def initialize(g: Graph, randomCoords: Boolean = true) {
    super.initialize(g, randomCoords)
    alpha = alpha0
    prevEnergy = 0.0
    energy = 0.0
  }

  // compute the equivalent point charge for every region of space in the quad tree
  def computeCharges(tr: QuadTree[(Option[VName], Double)]): QuadTree[(Option[VName], Double)] = tr match {
    case leaf: QuadLeaf[_] => leaf
    case _: QuadNode[_] =>
      val node = tr.asInstanceOf[QuadNode[(Option[VName], Double)]]

      val (v, nCharge) = node.value.getOrElse((None, 0.0))
      val nw = computeCharges(node.nw)
      val ne = computeCharges(node.ne)
      val sw = computeCharges(node.sw)
      val se = computeCharges(node.se)
      val (p, totalCharge) = Iterator(nw, ne, sw, se).foldLeft((node.p._1 * nCharge, node.p._2 * nCharge), nCharge) {
        case ((pSum, cSum), child) =>
          val (_, c) = child.value.getOrElse((None, 0.0))
          ((child.p._1 * c + pSum._1, child.p._2 * c + pSum._2), c + cSum)
      }

      val center = if (totalCharge != 0.0) (p._1 / totalCharge, p._2 / totalCharge) else (0.0, 0.0)
      QuadNode(node.x1, node.y1, node.x2, node.y2, Some((v, totalCharge)), center, nw, ne, sw, se)
  }

  // take an unconstrained step in the direction of steepest descent in energy
  def relax() {
    if (energy < prevEnergy) {
      progress += 1
      if (progress >= 15) {
        progress = 0
        alpha /= alphaAdjust
      }
    } else {
      progress = 0
      alpha *= alphaAdjust
    }

    prevEnergy = energy
    energy = 0

    val oldCoords = coords

    // shake overlapping elements slightly
    val vertexList: List[VName] = graph.verts.toList
    for (i <- vertexList; j <- vertexList if i != j) {
      val p1 = coord(i)
      val p2 = coord(j)
      val shake = 0.2
      if (p1 == p2) {
        setCoord(i, (
          p1._1 + shake * Math.random(),
          p1._2 + shake * Math.random()
        ))
      }
    }


    // apply spring forces
    for (e <- graph.edges) {
      val sp = coord(graph.source(e))
      val tp = coord(graph.target(e))
      val (dx, dy) = if (this.isInstanceOf[Ranking] || this.isInstanceOf[IRanking]) (2.0 * (tp._1 - sp._1), tp._2 - sp._2)
      else (tp._1 - sp._1, tp._2 - sp._2)
      val d = math.sqrt(dx * dx + dy * dy)
      if (d != 0.0) {
        val displacement = d - edgeLength
        val k = (alpha * strength * displacement) / d
        energy += 0.5 * strength * displacement * displacement
        val shift = (dx * k, dy * k)

        setCoord(graph.source(e), (sp._1 + shift._1, sp._2 + shift._2))
        setCoord(graph.target(e), (tp._1 - shift._1, tp._2 - shift._2))
      }
    }

    // apply gravity
    for (v <- graph.verts) {
      val p = coord(v)
      energy += gravity * math.sqrt(p._1 * p._1 + p._2 * p._2)
      setCoord(v, (
        p._1 * (1 - alpha * gravity),
        p._2 * (1 - alpha * gravity)
      ))
    }

    // compute charges
    val quad = computeCharges(QuadTree(graph.verts.toSeq.map { v => (coord(v), (Some(v), charge(v))) }))

    // apply charge forces
    for (v <- graph.verts if !lockedVertices.contains(v)) {
      var p = coord(v)
      quad.visit { nd =>
        nd.value match {
          case Some((optV, nodeCharge1)) =>
            val (dx1, dy1) = (nd.p._1 - p._1, nd.p._2 - p._2)
            val dx = if (abs(dx1) < 0.01) 0.01 else dx1
            val dy = if (abs(dy1) < 0.01) 0.01 else dy1
            val d2 = dx * dx + dy * dy

            if (d2 == 0.0) false
            else {
              // if the Barnes-Hut criterion is satisfied, act with the total charge of this region
              if ((nd.x2 - nd.x1) / math.sqrt(d2) < theta) {
                energy += (charge(v) + nodeCharge1) / d2
                val kx = alpha * nodeCharge1 / d2
                val ky = if (this.isInstanceOf[Ranking] || this.isInstanceOf[IRanking]) kx * 1.5 else kx
                p = (p._1 - dx * kx, p._2 - dy * ky)
                true
              } else {
                // if !B-H, but there is a (different) vertex here, act with the point charge
                optV match {
                  case Some(v1) if v1 != v =>
                    energy += (charge(v) + charge(v1)) / d2
                    val k = alpha * charge(v1) / d2
                    p = (p._1 - dx * k, p._2 - dy * k)
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

    // position verlet integration
    for (v <- graph.verts) {
      val (px, py) = oldCoords(v)
      val (x, y) = coord(v)
      setCoord(v, (x - ((px - x) * friction), y - ((py - y) * friction)))
    }

  }

  def recenter() {
    val (sumCoordX, sumCoordY) = graph.verts.foldLeft(0.0, 0.0)((pos, name)
    => (pos._1 + coord(name)._1, pos._2 + coord(name)._2))
    val (centerX, centerY) = (sumCoordX / graph.verts.size, sumCoordY / graph.verts.size)

    //	  if(abs(centerX)> 5|| abs(centerY)> 5){
    graph.verts.foreach(name => {
      val (px, py) = coord(name)
      setCoord(name, (px - centerX, py - centerY))
    })
    //  }
  }

  def step() {
    relax()
    projectConstraints()
    if (lockedVertices.isEmpty && keepCentered) recenter()
  }

  def compute() {
    iteration = 0
    while (iteration < maxIterations && alpha >= 0.0001) {
      step()
      iteration += 1
    }
  }
}
