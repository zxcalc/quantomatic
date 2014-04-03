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
//import quanto.layout.distance


trait NoTweaks extends Constraints {
	val radius = 0.2
	override def initialize(g: Graph, randomCoords: Boolean = true) {
	    super.initialize(g, randomCoords)
	    constraints.nextLayer()
	    println("Clusters at layer " + constraints.currentLayer)

	    // for edges we get rid of the
	       
	    for(v <- g.verts){
	    	val edges = g.inEdges(v) ++ g.outEdges(v)
	    	val compEdges = g.edges -- edges
	    	
	    	// for each vertex, the distance to each compEdges
	    	// is greater than its radius.
	    	
	    	for (e <- compEdges ){
	    		val s = graph.source(e)
	    		val t = graph.target(e)
	    		val (sx,sy) = coord(s)
	    		val (tx,ty) = coord(t)
	    		val (pqx,pqy) = (sx-tx, sy-ty)
	    		var par = (0.0, 0.0)
	    		if (pqy == 0){
	    		  par = (0.0,1.0) 
	    		}
	    		else if (pqx == 0){
	    			par = (1.0, 0.0)
	    		}
	    		else {
	    		   par = (-pqy/pqx,1.0)
	    		}
	    		
	    		constraints += { (Constraint.distance from v to s along par) >= radius }
	    	}
	    	
	    }	    
	}
}