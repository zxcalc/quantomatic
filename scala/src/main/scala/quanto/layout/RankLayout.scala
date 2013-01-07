package quanto.layout

import quanto.data._
import util.Sorting
import JaCoP.constraints.netflow._

class RankLayout extends GraphLayout with ConstraintSolver {
  var precision = 100
  var minNodeSep = 80
  var maxNodeSep = 200

  var crossingIterations = 10

  private var rankMap = PFun[VName, Int]()
  private var posMap = Map[VName,Int]()
  var modelGraph = QGraph()

  def weight(graph: QGraph, e: EName) = 1
  def minLength(graph: QGraph, e: EName) = 1

  def rank(v: VName): Int = rankMap(v)
  def pos(v: VName): Int = posMap(v)
  def maxRank: Int = rankMap.cod.max
  def maxRankSize: Int = rankMap.codf.values.map(_.size).max

//  private def distMap(v: VName, direction: Int,
//                      mp: Map[VName, Int]=Map(v->0),
//                      prev: Set[VName]=Set(),
//                      dist: Int=1): Map[VName, Int] = {
//    val next = if (direction == -1) prev.map(modelGraph.predVerts(_)).reduce(_ ++ _)
//               else prev.map(modelGraph.succVerts(_)).reduce(_ ++ _)
//    distMap(v, direction, next.foldLeft(mp) { (m,v) => m + (v -> dist) }, next, dist + 1)
//  }
//
//  private def edgeRankCost(e: EName) = {
//    val predMap = distMap(modelGraph.source(e), -1)
//    val succMap = distMap(modelGraph.target(e), 1)
//    modelGraph.edges.fold(0) { (w, e) =>
//      w + ((predMap.get(modelGraph.source(e)), succMap.get(modelGraph.target(e))) match {
//        case (Some(d1), Some(d2)) => 1 + d1 + d2 - minLength(e)
//        case _ => 0
//      })
//    }
//  }

  private def orderByMedians(iteration: Int) {
    val (rseq, offset) = if (iteration % 2 == 0) (1 to maxRank, -1) else ((maxRank-1) to 0, 1)
    for (r <- rseq) {
      val medianMap = rankMap.codf(r).foldLeft(Map[VName,Int]()) { (m, v) =>
        val adj = (if (offset == -1) modelGraph.predVerts(v) else modelGraph.succVerts(v)).map(posMap(_)).toArray.sorted
        m + (v -> (if (adj.size == 0) 0 else adj(adj.size / 2)))
      }

      val sorted = rankMap.codf(r).toArray.sortWith((x,y) => medianMap(x) < medianMap(y))
      for ((v,i) <- sorted.view.zipWithIndex) posMap += v -> i
    }
  }

  private def sgn(i: Int) = if (i < 0) -1 else if (i > 0) 1 else 0


  private def crossingsForRank(r: Int, offset: Int, tryPos: Map[VName, Int]): Int = {
    val es = if (offset == -1) rankMap.codf(r).foldLeft(Set[EName]()) { (s,v) => s ++ modelGraph.inEdges(v) }
             else rankMap.codf(r).foldLeft(Set[EName]()) { (s,v) => s ++ modelGraph.outEdges(v) }
    var c = 0
    for (d <- es; e <- es
         if sgn( tryPos(modelGraph.source(d)) - tryPos(modelGraph.source(e)) ) !=
            sgn( tryPos(modelGraph.target(d)) - tryPos(modelGraph.target(e)) )
    ) c += 1
    c
  }

  private def orderByCrossings(iteration: Int) {
    val (rseq, offset) = if (iteration % 2 == 0) (1 to maxRank, -1) else ((maxRank-1) to 0, 1)
    for (r <- rseq) {
      val vseq = if (iteration % 4 == 0 || iteration % 4 == 1) rankMap.codf(r).toSeq
                 else rankMap.codf(r).toSeq.reverse

      for (v <- vseq; w <- vseq; if v != w) {
        val pos1 = posMap + (v -> posMap(w), w -> posMap(v))

        val (x,y) = (crossingsForRank(r, offset, pos1), crossingsForRank(r, offset, posMap))

        if (
          x < y || (x == y &&
          crossingsForRank(r, -offset, pos1) <= crossingsForRank(r, -offset, posMap))
        ) posMap = pos1
      }
    }
  }

  def layout(graph: QGraph): QGraph = {
    modelGraph = graph.dagCopy

    // assign ranks to minimise edge length
    val rankVars = graph.verts.foldLeft(Map[VName,IntVar]())
      { (m, v) => m + (v -> IntVar("r_" + v, 0, graph.vdata.size)) }

    val weights = for (e <- modelGraph.edges) yield {
      constraints += { rankVars(modelGraph.target(e)) #>= (rankVars(modelGraph.source(e)) + minLength(graph, e)) }
      weight(graph, e) match {
        case 1 => rankVars(modelGraph.target(e)) - rankVars(modelGraph.source(e))
        case w => (rankVars(modelGraph.target(e)) - rankVars(modelGraph.source(e))) * w
      }
    }

    modelGraph.edges.foreach{ e =>
      constraints += { rankVars(modelGraph.target(e)) #>= (rankVars(modelGraph.source(e)) + minLength(graph, e)) }
    }

//    val net = new NetworkBuilder
//    val nodeMap = modelGraph.verts.fold(Map[VName,simplex.Node]) { (m,v) => m + (v -> net.addNode(v,0)) }



    minimize(rankVars.values, sum(weights))
    rankMap = rankVars.foldLeft(PFun[VName,Int]()) { case (pf, (k,vr)) => pf + (k -> vr.value) }

    posMap = Map[VName,Int]()
    for (vset <- rankMap.codf.values; (v,i) <- vset.view.zipWithIndex) { posMap += v -> i }

    // update model graph to include virtual vertices at intermediate ranks
    for (e <- modelGraph.edges) {
      val vChain = (for (i <- (rankMap(modelGraph.source(e)) + 1) to (rankMap(modelGraph.target(e)) - 1)) yield {
        val (g,v) = modelGraph.newVertex(WireV())
        modelGraph = g
        rankMap += v -> i
        v
      }) :+ modelGraph.target(e)

      if (vChain.size > 1) {
        var prev = modelGraph.source(e)
        modelGraph = modelGraph.deleteEdge(e)
        for (v <- vChain) {
          modelGraph = modelGraph.newEdge(DirEdge(), (prev, v))
          prev = v
        }
      }
    }

    // order vertices in ranks to minimise crossings
    for (i <- 1 to crossingIterations) orderByMedians(i)
    for (i <- 1 to crossingIterations) orderByCrossings(i)

    var xVars = Map[VName,IntVar]()

    var vertPush = Set[IntVar]()

    for ((r,vs) <- rankMap.codf) {
      val maxX = (maxNodeSep * vs.size) / 2
      val minX = -maxX
      val vArray = Array.ofDim[VName](vs.size)
      vs.foreach {v =>
        vArray(posMap(v)) = v
        xVars += v -> IntVar("x_" + v, minX, maxX)
      }

      for (i <- 1 to (vArray.size - 1)) {
        val dist = xVars(vArray(i)) - xVars(vArray(i-1))
        constraints += { dist #>= minNodeSep }
        constraints += { dist #<= maxNodeSep }
        vertPush += dist
       }
    }

    val edgePull = modelGraph.edges.map {e =>
      xVars(modelGraph.source(e)) distanceTo xVars(modelGraph.target(e))
    }

    minimize(xVars.values, sum(edgePull) - sum(vertPush))

    val xCoords = xVars.mapValues(v => v.value.toDouble / precision)

    val mRank = maxRank

    modelGraph = modelGraph.vdata.foldLeft(modelGraph) { case (gr,(v,_)) =>
      gr.updateVData(v) { _ withCoord
        (xCoords(v), (-0.5 * mRank) + rank(v).toDouble)
      }
    }

    graph.vdata.foldLeft(graph) { case (gr, (v,_)) =>
      gr.updateVData(v) { _ withCoord modelGraph.vdata(v).coord }
    }
  }

}
