package quanto.data

// a graph homomorphism
case class GraphMap(
                     v: PFun[VName, VName] = PFun(),
                     e: PFun[EName, EName] = PFun(),
                     bb: PFun[BBName, BBName] = PFun()
                   ) {
  // TODO: bbox check
  def isHomomorphism(g: Graph, h: Graph): Boolean =
    e.forall { case (ep, et) =>
      (g.edata(ep).isDirected && h.edata(et).isDirected &&
        v.get(g.source(ep)).contains(h.source(et)) &&
        v.get(g.target(ep)).contains(h.target(et))
        ) ||
        (!g.edata(ep).isDirected && !h.edata(et).isDirected &&
          (
            (
              v.get(g.source(ep)).contains(h.source(et)) &&
                v.get(g.target(ep)).contains(h.target(et))
              ) ||
              (
                v.get(g.source(ep)).contains(h.target(et)) &&
                  v.get(g.target(ep)).contains(h.source(et))
                )
            ))
    }

  def isInjective: Boolean = {
    v.codf.forall(_._2.size == 1) &&
      e.codf.forall(_._2.size == 1) &&
      bb.codf.forall(_._2.size == 1)
  }

  def addVertex(p: (VName,VName)): GraphMap = copy(v = v + p)
  def addEdge(p: (EName,EName)): GraphMap = copy(e = e + p)
  def addBBox(p: (BBName,BBName)): GraphMap = copy(bb = bb + p)

  def isTotal(g: Graph): Boolean =
      v.domSet == g.verts &&
        e.domSet == g.edges &&
        bb.domSet == g.bboxes

  def image(g: Graph): Graph = g.rename(v.toMap, e.toMap, bb.toMap)
}
