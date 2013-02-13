package quanto.layout.constraint

import quanto.data._
import collection.mutable.ListBuffer
import quanto.layout._

import math.{min,max,abs}
import quanto.util.QuadTree
import quanto.data.VName

/**
 * Constraints to force nodes not in the given clusters outside of the bounding box
 */
trait Clusters extends Constraints {
  import Constraint.distance

  var clusters = ListBuffer[Set[VName]]()
  var clusterPadding = 0.1
  var clusterRadiusPerVertex = 2.0

  override def initialize(g: Graph) {
    super.initialize(g)
    constraints.nextLayer()
    println("Clusters at layer " + constraints.currentLayer)

    g.bboxes.foreach(bb => clusters += g.contents(bb))

    constraints ++= {

      val coordTree = QuadTree(graph.verts.toSeq.map { v => (coord(v), v) })

      val clusterBounds = clusters.filter(x => !x.isEmpty).map { cluster =>
        val it = cluster.iterator
        val firstCoord = coord(it.next())
        (
          cluster,
          it.foldLeft((firstCoord,firstCoord)) { case ((lc,uc), v) =>
            val c = coord(v)
            ((min(lc._1,c._1), min(lc._2,c._2)), (max(uc._1,c._1), max(uc._2,c._2)))
          } match { case ((lx,ly),(ux,uy)) =>
            ((lx - clusterPadding, ly - clusterPadding), (ux + clusterPadding, uy + clusterPadding))
          }
        )
      }

      val cons = ListBuffer[Constraint]()

      for ((cluster,(lb,ub)) <- clusterBounds) {
        val clusterSize = cluster.size
        for (v1 <- cluster; v2 <- cluster; if v1 != v2)
          cons += { (distance from v1 to v2) <= clusterRadiusPerVertex * clusterSize }

        val verts = coordTree.query(lb._1, lb._2, ub._1, ub._2)
        for (v <- verts; if !cluster.contains(v)) {
          val vc = coord(v)
          // work out the most efficient shift
          val xShift = if (vc._1 >= lb._1 && vc._1 <= ub._1) {
            val left  = lb._1 - vc._1
            val right = ub._1 - vc._1
            if (-left < right) left else right
          } else 0

          val yShift = if (vc._2 >= lb._2 && vc._2 <= ub._2) {
            val down  = lb._2 - vc._2
            val up = ub._2 - vc._2
            if (-down < up) down else up
          } else 0

          if (xShift != 0 && yShift != 0) {

            for (v1 <- cluster) {
              val v1c = coord(v1)
              val (len,dir) = if (abs(xShift) < abs(yShift)) {
                if (v1c._1 - vc._1 + xShift > 0.0) (v1c._1 - vc._1 + xShift, (1.0,0.0))
                else (-(v1c._1 - vc._1 + xShift), (-1.0,0.0))
              } else {
                if (v1c._2 - vc._2 + yShift > 0.0) (v1c._2 - vc._2 + yShift, (0.0,1.0))
                else (-(v1c._2 - vc._2 + yShift), (0.0,-1.0))
              }

              cons += { (distance from v to v1 along dir weighted (1.0, cluster.size.toDouble)) >= len }
            }
          }
        }
      }

      cons
    }
  }
}
