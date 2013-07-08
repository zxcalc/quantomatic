behavior of "Rank layout"

  var rankLayout: RankLayout = _

  it should "initialise" in {
    rankLayout = new RankLayout
  }

  var randomGraph: Graph = _

  it should "layout a graph" in {
    randomGraph = rankLayout.layout(Graph.random(4,8))
  }

  it should "yield ranks for all verts" in {
    randomGraph.verts.foreach(v => rankLayout.rank(v))

    val totalDist = randomGraph.edges.foldLeft(0) { (x, e) =>
      x + (rankLayout.rank(randomGraph.target(e)) - rankLayout.rank(randomGraph.source(e)))
    }

    println("average distance: " + (totalDist.toDouble / randomGraph.edges.size))
  }

  it should "provide a strict ranking for associated dag" in {
    val dag = randomGraph.dagCopy

    for ((e,_) <- dag.edata) {
      assert(rankLayout.rank(dag.source(e)) < rankLayout.rank(dag.target(e)))
    }
  }