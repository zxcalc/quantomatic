package quanto.rewrite

import quanto.data._

object Rewriter {
	def rewrite(m: Match): Graph = {
    // make the pattern graph fresh w.r.t. to the target
    val m1 = m.normalize.freshenPattern
    val boundary = m1.pattern.verts.filter(m1.pattern.isBoundary)
    val interior = m1.pattern.verts -- boundary

    // compute the pushout complement
    val context = m1.target
      .deleteEdges(m1.emap.codSet)
      .deleteVertices(m1.vmap.directImage(interior))

    // quotient the pattern such that pairs of boundaries mapped to the same vertex are identified
    val quotient = m1.vmap.directImage(boundary).foldRight(m1.pattern) { (v, g) =>
      g.mergeVertices(m1.vmap.codf(v), v)
    }

    // compute the pushout as a union of the context with the quotiented domain of the matching
    context.appendGraph(quotient)
  }
}