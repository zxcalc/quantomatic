package quanto.rewrite

import quanto.data._
import quanto.data.Names._

object Rewriter {
  def expandRhs(m: Match, rhs: Graph): Graph = {
    // ensure that *all* boundary names used in expanding bbops are avoided
    val fullBoundary = m.bbops.foldRight(m.pattern0.boundary) { (bbop, vs) =>
      bbop match {
        case BBExpand(_, mp, fresh) =>
          // TODO: fresh
          vs union mp.v.directImage(vs)
        case BBCopy(_, mp) => vs union mp.v.directImage(vs)
        case _ => vs
      }
    }

    val rhs1 = m.bbops.foldRight(rhs) { (bbop, g) => g.applyBBOp(bbop, fullBoundary) }

    val vdata = rhs1.vdata.mapValues {
      case d: NodeV =>
        val data = d.data.setPath("$.value", d.angle.subst(m.subst).toString).asObject
        d.copy(data = data)
      case d: WireV => d
    }

    rhs1.copy(vdata = vdata)
  }

	def rewrite(m: Match, rhs: Graph, desc: RuleDesc = RuleDesc()): (Graph, Rule) = {
    // expand bare wires in the match
    val m1 = m.normalize

    // make the pattern graph fresh w.r.t. to the target
    val boundary = m1.pattern.boundary

    // expand bboxes in RHS
    val rhsE = expandRhs(m1, rhs)

    val interiorLhs = m1.pattern.verts -- boundary
    val interiorRhs = rhsE.verts -- boundary

    // compute the pushout complement
    val context = m1.target
      .deleteEdges(m1.map.e.codSet)
      .deleteVertices(m1.map.v.directImage(interiorLhs))

    val vmap = interiorRhs.foldRight(m1.map.v.restrictDom(boundary)) { (v, mp) =>
      mp + (v -> (context.verts union mp.codSet).freshWithSuggestion(v))
    }

    val emap = rhsE.edges.foldRight(PFun[EName,EName]()) { (e, mp) =>
      mp + (e -> (context.edges union mp.codSet).freshWithSuggestion(e))
    }


    // quotient the lhs and rhs such that pairs of boundaries mapped to the same vertex are identified
    val quotientLhs = m1.pattern.rename(m1.map.v.toMap, m1.map.e.toMap, m1.map.bb.toMap)
    val quotientRhs = rhsE.rename(vmap.toMap, emap.toMap, m1.map.bb.toMap)

    val ruleInst = if (desc.inverse) Rule(quotientRhs, quotientLhs, description = desc)
                   else Rule(quotientLhs, quotientRhs, description = desc)

    // compute the pushout as a union of the context with the quotiented domain of the matching
    (quotientRhs.appendGraph(context), ruleInst)
  }
}