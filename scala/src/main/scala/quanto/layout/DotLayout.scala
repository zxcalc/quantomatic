package quanto.layout

import sys.process._
import quanto.data._
import java.io._
import quanto.data.VName

class DotLayout extends GraphLayout {
  var dotString = ""
  var dotProcess: Process = null
  var dotIn: BufferedReader = _
  var dotOut: BufferedWriter = _

  private def generateDot(graph: Graph) = {
    val sb = new StringBuilder

    var vid = Map[VName,Int]()
    var i = 0

    sb ++= "digraph {\n"

    graph.vdata.foreach { case (v,d) =>
      sb ++= "  " + i + " [width=14,height=14]\n"
      vid += v -> i
      i += 1
    }

    graph.edata.foreach { case (e,d) =>
      sb ++= "  %d %s %d\n".format(
        vid(graph.source(e)),
        if (d.isDirected) "->" else "--",
        vid(graph.target(e)))
    }

    i = 0

    graph.bbdata.foreach { case(bb,d) =>
      sb ++= "  subgraph \"cluster_" + i + "\" { \n"
      graph.contents(bb).foreach { v => sb ++= "    " + vid(v) + "\n" }
      sb ++= "  }\n"
      i += 1
    }

    sb ++= "}\n"

    (vid,sb.toString)
  }

  def layout(graph: Graph) = {
    val (vid,dotStr) = generateDot(graph)
    dotString = dotStr
    var xMax = 0.0
    var yMax = 0.0

    val Graph = """graph \d+ (\d+(\.\d+)?) (\d+(\.\d+)?)""".r
    val Node  = """node (\d+) (\d+(\.\d+)?) (\d+(\.\d+)?) .*""".r

    var coordMap = Map[Int,(Double,Double)]()

    ("dot -Tplain" #< new ByteArrayInputStream(dotString.getBytes("UTF-8"))).lines_!.foreach { line =>
      line match {
        case Graph(xbd,_, ybd,_) =>
          xMax = xbd.toDouble
          yMax = ybd.toDouble
        case Node(id, x,_, y,_) =>
          coordMap += id.toInt -> ((x.toDouble - 0.5 * xMax) / 10, (0.5 * yMax - y.toDouble) / 10)
        case _ => ()
      }
    }

    graph.verts.foldLeft(graph) { (g, v) => g.updateVData(v) { d =>
      d.withCoord(coordMap(vid(v)))
    }}
  }
}
