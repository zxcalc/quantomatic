package quanto.layout

import quanto.data.{Graph, VData}

import scala.collection.mutable.ListBuffer
// transfer each bbox to a node

class GraphTransform(g: Graph) {

  //val numOfBBox = g.bboxes.size
  val bs: ListBuffer[() => Iterator[VData]] = collection.mutable.ListBuffer[() => Iterator[VData]]()

  def transform() {
    g.bboxes.foreach(bb =>
      for (ver <- g.contents(bb)) {
        val target = g.succVerts(ver)


      })
		

  }

  def restore() {

  }
}