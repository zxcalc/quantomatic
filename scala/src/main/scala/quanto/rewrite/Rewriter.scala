package quanto.rewrite

import quanto.data._
import quanto.data.Names._

object Rewriter {
	def rewrite(m: Match, rhs: Graph): (Graph, Rule) = {
    // TODO: !-boxes

    // expand bare wires in the match
    val m1 = m.normalize

    // make the pattern graph fresh w.r.t. to the target
    val boundary = m1.pattern.verts.filter(m1.pattern.isBoundary)

    val interiorLhs = m1.pattern.verts -- boundary
    val interiorRhs = rhs.verts -- boundary

    // compute the pushout complement
    val context = m1.target
      .deleteEdges(m1.map.e.codSet)
      .deleteVertices(m1.map.v.directImage(interiorLhs))

    val vmap = interiorRhs.foldRight(m1.map.v.restrictDom(boundary)) { (v, mp) =>
      mp + (v -> (context.verts union mp.codSet).freshWithSuggestion(v))
    }

    val emap = rhs.edges.foldRight(PFun[EName,EName]()) { (e, mp) =>
      mp + (e -> (context.edges union mp.codSet).freshWithSuggestion(e))
    }


    // quotient the lhs and rhs such that pairs of boundaries mapped to the same vertex are identified
    val quotientLhs = m.pattern.rename(m.map.v.toMap, m.map.e.toMap, m.map.bb.toMap)
    val quotientRhs = rhs.rename(vmap.toMap, emap.toMap, m1.map.bb.toMap)

    // compute the pushout as a union of the context with the quotiented domain of the matching
    (quotientRhs.appendGraph(context), Rule(quotientLhs, quotientRhs))
  }
}