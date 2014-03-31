package quanto.layout

import quanto.data._
import quanto.layout.constraint._


class DeriveLayout {
  def layout(derivation: Derivation) = {
    var steps = Map[DSName, DStep]()

    val layoutProc = new ForceLayout with Clusters
    layoutProc.alpha0 = 0.05
    layoutProc.keepCentered = false

    while (steps.size < derivation.steps.size) derivation.steps.foreach { case (sname, step) =>
      // try to pull a parent that has already been processed, or root if step has no parent
      val parentOpt = derivation.parent.get(sname) match {
        case Some(p) => steps.get(p).map(_.graph)
        case None => Some(derivation.root)
      }

      parentOpt.map { parent: Graph =>
        var g = step.graph
        layoutProc.lockedVertices.clear()

        // transport and lock all the coords from parent (NOTE: this assumes that vertices added with rule application
        //  are fresh for parent)
        for (v <- parent.verts) {
          if (g.verts.contains(v)) {
            g = g.updateVData(v) { _.withCoord(parent.vdata(v).coord) }
            layoutProc.lockedVertices += v
          }
        }

        layoutProc.initialize(g, randomCoords = false)

        // relax a bit to layout new coords
        for (i <- 1 to 20) layoutProc.step()
        layoutProc.updateGraph()

        // layout the graph, and add the step
        steps += sname -> step.copy(graph = layoutProc.graph)
      }
    }

    derivation.copy(steps = steps)
  }
}
