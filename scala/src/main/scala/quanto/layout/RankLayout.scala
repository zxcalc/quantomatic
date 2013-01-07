package quanto.layout

import quanto.data._

class RankLayout extends GraphLayout with ConstraintSolver {
  var precision = 1000
  var minNodeSep = 100
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
    for (i <- 1 to crossingIterations) {
      orderByMedians(i)
    }

    for (i <- 1 to crossingIterations) {
      orderByCrossings(i)
    }




    //    // the position of the centers of each vertex
    //    val oldCoord = collection.mutable.Map[VName, (Int,Int)]()
    //    val coord = collection.mutable.Map[VName, (Int,Int)]()
    //
    //    // the dimensions of the bounding box of each vertex
    //    val dim = collection.mutable.Map[VName, (Int,Int)]()

    val mRank = maxRank

    modelGraph = modelGraph.vdata.foldLeft(modelGraph) { case (gr,(v,_)) =>
      gr.updateVData(v) { _ withCoord
        (
          (-0.5 * rankMap.codf(rank(v)).size) + pos(v).toDouble,
          (-0.5 * mRank) + rank(v).toDouble
          )}
    }

    graph.vdata.foldLeft(graph) { case (gr, (v,_)) =>
      gr.updateVData(v) { _ withCoord modelGraph.vdata(v).coord }
    }
  }

}