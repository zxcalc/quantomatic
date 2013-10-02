package quanto.layout

import quanto.data.Graph
import quanto.data.VData
// transfer each bbox to a node

class GraphTransform (g:Graph){
  
	//val numOfBBox = g.bboxes.size
	val bs = collection.mutable.ListBuffer[() => Iterator[VData]]()
	
	def transform {
		g.bboxes.foreach(bb => 
		  for(ver <- g.contents(bb)){
			  val target = g.succVerts(ver)
			  
			  
		  
		})
		
		
		
	}

	def restore{
		
	}
}