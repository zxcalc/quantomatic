package quanto.layout

import quanto.data._
import quanto.layout.constraint._


class DeriveLayout {
  def layout(derivation: Derivation): Derivation = {
    var steps = Map[DSName, DStep]()

    val layoutProc = new ForceLayout with Clusters
    layoutProc.alpha0 = 0.05
    layoutProc.keepCentered = false

    while (steps.size < derivation.steps.size) derivation.steps.foreach { case (stepName, step) =>
      // try to pull a parent that has already been processed, or root if step has no parent
      val parentOpt = derivation.parentMap.get(stepName) match {
        case Some(p) => steps.get(p).map(_.graph)
        case None => Some(derivation.root)
      }

      parentOpt.foreach { parent: Graph =>
        var g = step.graph
        layoutProc.clearLockedVertices()

        // transport and lock all the coords from parent (NOTE: this assumes that vertices added with rule application
        //  are fresh for parent)
        for (v <- parent.verts) {
          if (g.verts.contains(v)) {
            g = g.updateVData(v) {
              _.withCoord(parent.vdata(v).coord)
            }
            layoutProc.lockVertex(v)
          }
        }

        layoutProc.initialize(g, randomCoords = false)

        // relax a bit to layout new coords
        for (_ <- 1 to 10) layoutProc.step()
        layoutProc.updateGraph()

        // layout the graph, and add the step
        steps += stepName -> step.copy(graph = layoutProc.graph)
      }
    }

    derivation.copy(steps = steps)
  }
}
