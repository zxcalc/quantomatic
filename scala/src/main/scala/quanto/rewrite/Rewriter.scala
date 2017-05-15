package quanto.rewrite

import quanto.data._
import quanto.data.Names._

object Rewriter {
	def rewrite(m: Match, rhs: Graph): Graph = {
    // TODO: !-boxes

    // expand bare wires in the match
    val m1 = m.normalize

    // make the pattern graph fresh w.r.t. to the target
    val boundary = m1.pattern.verts.filter(m1.pattern.isBoundary)

    val interiorLhs = m1.pattern.verts -- boundary
    val interiorRhs = rhs.verts -- boundary

    // compute the pushout complement
    val context = m1.target
      .deleteEdges(m1.emap.codSet)
      .deleteVertices(m1.vmap.directImage(interiorLhs))

    val vmap = interiorRhs.foldRight(m1.vmap.restrictDom(boundary)) { (v, mp) =>
      mp + (v -> (context.verts union mp.codSet).freshWithSuggestion(v))
    }

    val emap = rhs.edges.foldRight(PFun[EName,EName]()) { (e, mp) =>
      mp + (e -> (context.edges union mp.codSet).freshWithSuggestion(e))
    }


    // quotient the rhs such that pairs of boundaries mapped to the same vertex are identified
    val quotient = rhs.rename(vmap.toMap, emap.toMap, m1.bbmap.toMap)

    // compute the pushout as a union of the context with the quotiented domain of the matching
    quotient.appendGraph(context)
  }
}