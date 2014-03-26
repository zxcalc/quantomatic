package quanto.layout.constraint

import quanto.data._
import collection.mutable.ListBuffer
import quanto.layout._

import math.{min,max,abs}
import quanto.util.QuadTree
import quanto.data.VName
import quanto.util._
import Geometry._
import Names._
/**
 * Constraints to force nodes not in the given clusters outside of the bounding box
 */
trait Clusters extends Constraints {
  import Constraint.distance
  var debug = false
  var clusters = ListBuffer[Set[VName]]()
  var clusterPadding = 0.5
  var clusterRadiusPerVertex = 2.0
  
  // right code?
  def inRect(v:VName, r : ((Double,Double),(Double,Double))) : Boolean =  {
    val (px,py) = coord(v)
    val ((lx,ly),(ux,uy)) = r
    (px > lx) && (px < ux) && (py > ly) && (py < uy)
  }
  
  override def initialize(g: Graph, randomCoords: Boolean = true) {
    super.initialize(g, randomCoords)
    constraints.nextLayer()
    //println("Clusters at layer " + constraints.currentLayer)
    
    // take each bbox as a cluster
    clusters.clear
    g.bboxes.foreach(bb => clusters += g.contents(bb))
    //println("cluster size " + clusters.size);
    constraints ++= {

      val coordTree = QuadTree(graph.verts.toSeq.map { v => (coord(v), v) })

      clusters.foldLeft(List[Constraint]()) { (constraints,cluster) =>
        bounds(cluster.map(coord(_))) match {
          case Some(rect) =>
            
            val bbox = new RichRect(rect) 
            val (lb,ub) = bbox.pad(clusterPadding)
            //if (debug) println("the bound bbox boundary is " +(lb,ub))
            
            // centre of the bbox
            val (cx,cy) = bbox.center
            //if (debug) (println("the center is " + bbox.center))
            val (wth,hgt) = bbox.size

            val clusterSize = cluster.size
            var cons = constraints
            for (v1 <- cluster; v2 <- cluster; if v1 != v2) {
             cons ::= { (distance from v1 to v2) <= (clusterRadiusPerVertex * (clusterSize)-1) } // * 0.95
//             cons ::= { (distance from v1 to v2 along (1.0,0.0)) <= (clusterRadiusPerVertex * (clusterSize)) } // * 0.95
//             cons ::= { (distance from v1 to v2 along (0.0,1.0)) <= (clusterRadiusPerVertex * (clusterSize)) } // * 0.95
            }
            
            //val verts = g.verts.filter(vname => inRect(vname,(lb,ub)))
            val verts = coordTree.query(lb._1, lb._2, ub._1, ub._2)

            //  quadtree is not right
            //println("quadTree is " + verts)
//            if (debug) println ("v3 's coordinate is " + coord("v3"))
//            if (debug) println ("b0 's coordinate is " + coord("b0"))
//            if (debug) println ("v1 's coordinate is " + coord("v1"))
           // println("v0 " + "is " +  coord("v0"))
           // println("box " + (lb,ub))
            //if (debug) println ("all vertices in the bbox\n\t" + verts)

            for (v <- verts; if !cluster.contains(v)) {
              val vc = coord(v)
              
             // println(v + " is in the bbox but not belongs it");
              // work out the most efficient shift

              ///// 
              /*
              here I think we need to compute that on which side has more vertices that
              is connected with it. the move to that direction.
              
              */
              // need a better way to shift.
              
              // move to the side that has more force to pull it
              val (shiftDirX, shiftDirY) = (g.succVerts(v)++g.predVerts(v)).
            		  	foldLeft(0.0,0.0)((pos,name) => (pos._1+coord(name)._1-vc._1,pos._2+coord(name)._2-vc._2))              
              //println("node " + v + " should move direction" +  (shiftDirX, shiftDirY))
//              val xShift = if (vc._1 >= lb._1 && vc._1 <= ub._1) {
//                if(shiftDirX > 0) ub._1 - vc._1 else lb._1 - vc._1
//                
//              } else 0
//                            
//              val yShift = if (vc._2 >= lb._2 && vc._2 <= ub._2) {
//                if(shiftDirY > 0){
//                  ub._2 - vc._2
//                }
//                else{
//                  lb._2 - vc._2
//                }
//              } else 0
//
//              if (xShift != 0 && yShift != 0){
//                for (v1 <- cluster) {
//                  val v1c = coord(v1)
//                  val (len,dir) = if (abs(shiftDirX) > abs(shiftDirY)) {
//                    if (xShift < 0.0) (abs((vc._1 - v1c._1) + (xShift)), (1.0,0.0))
//                    else (abs((vc._1 - v1c._1) + xShift), (-1.0,0.0))
//                  } else {
//                    if (yShift < 0.0) (abs((vc._2 - v1c._2) + (yShift)), (0.0,1.0))
//                    else (abs((vc._2 - v1c._2) + (yShift)), (0.0,-1.0))
//                  }
//                  
//                  cons ::= { (distance from v to v1 along dir weighted (1.0, 1.0)) >= len } //cluster.size.toDouble+1
//                }
//              }
              
              // not sure if we ned the if condition 
              // since quadtree will make sure the condition is true
              val xShift = if (vc._1 >= lb._1 && vc._1 <= ub._1) {
                val left  = lb._1 - vc._1
                val right = ub._1 - vc._1
                if (abs(left) < right) left else right
              } else 0

              val yShift = if (vc._2 >= lb._2 && vc._2 <= ub._2) {
                val down  = lb._2 - vc._2
                val up = ub._2 - vc._2
                if (-down < up) down else up
              } else 0
              
               //questions about following code
              if (xShift != 0 && yShift != 0) {
                // val soft = g.isBBoxed(v)
                for (v1 <- cluster) {
                  val v1c = coord(v1)
                  val (len,dir) = if (abs(xShift) < abs(yShift)) {
                    if (xShift < 0.0) (abs((vc._1 - v1c._1) + (xShift)), (1.0,0.0))
                    else (abs((vc._1 - v1c._1) + xShift), (-1.0,0.0))
                  } else {
                    if (yShift < 0.0) (abs((vc._2 - v1c._2) + (yShift)), (0.0,1.0))
                    else (abs((vc._2 - v1c._2) + (yShift)), (0.0,-1.0))
                  }
                  
                  cons ::= { (distance from v to v1 along dir weighted (1.0, 1.0)) >= len } //cluster.size.toDouble+1
//                  cons ::= { (distance from v to v1 along dir weighted (1.0, cluster.size.toDouble+1)) >= len } //
                }
              }
            }

            cons
          case None => constraints
        }
        
      }
    }
  }
}
