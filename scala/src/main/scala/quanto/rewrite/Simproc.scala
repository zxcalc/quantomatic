package quanto.rewrite

import quanto.cosy.{AutoReduce, RuleSynthesis}
import quanto.data.Derivation.DerivationWithHead
import quanto.data._
import quanto.layout.ForceLayout

import scala.util.Random


abstract class Simproc {
  def simp(g: Graph): Iterator[(Graph, Rule)]

  var sourceFile: String = ""
  var sourceCode: String = ""

  // chain two simprocs together
  def >>(t: Simproc) = {
    val s = this
    new Simproc {
      override def simp(g: Graph): Iterator[(Graph, Rule)] = new Iterator[(Graph,Rule)] {
        var iterS: Iterator[(Graph,Rule)] = s.simp(g)
        var iterT: Iterator[(Graph,Rule)] = null
        var lastGraphS = g

        override def hasNext: Boolean =
          if (iterT != null) iterT.hasNext
          else if (iterS.hasNext) true
          else {
            iterT = t.simp(lastGraphS)
            iterT.hasNext
          }

        override def next(): (Graph, Rule) =
          if (iterT != null) {
            iterT.next()
          } else if (iterS.hasNext) {
            val (g1,r1) = iterS.next()
            lastGraphS = g1
            (g1,r1)
          } else {
            iterT = t.simp(lastGraphS)
            iterT.next()
          }
      }
    }
  }

  // jython binding for >>
  def __rshift__(t: Simproc): Simproc = this >> t
}

object Simproc {

  // Converts a (Derivation, Head) pair into an iterated series of steps
  // Allows gluing together of simprocs and derivations
  implicit def fromDerivationWithHead(d: DerivationWithHead): Iterator[(Graph, Rule)] = {
    if (d._2.nonEmpty) {
      d._1.stepsTo(d._2.get).map(d._1.steps).map(step => (step.graph, step.rule)).toIterator
    } else {
      Iterator.empty
    }
  }

  private def layout(gr: (Graph, Rule)) = {
    val (graph, rule) = gr
    val layoutProc = new ForceLayout
    layoutProc.keepCentered = false
    layoutProc.nodeCharge = 0.1

    layoutProc.alpha0 = 0.05
    layoutProc.alphaAdjust = 1.0
    layoutProc.maxIterations = 300
    //layoutProc.keepCentered = false

    val rhsi = rule.rhs.verts.filter(!rule.rhs.isTerminalWire(_))
    //println(rhsi)
    graph.verts.foreach { v => if (!rhsi.contains(v)) layoutProc.lockVertex(v) }
    //graph.verts.foreach { v =>  if (graph.isBoundary(v)) layoutProc.lockVertex(v) }
    (layoutProc.layout(graph, randomCoords = false).snapToGrid(), rule)
    //(graph, rule)
  }

  object EMPTY extends Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = Iterator.empty
  }


  /**
    * Anneals the graph using only the rules (forwards only), using vertex size as the metric
    * No initial heat specified, just accepts worse states with a (decreasing-over-time) random chance
    * @param rules          List of rules, taken forwards only
    * @param steps      Number of steps to be taken
    * @param dilation   How slowly we stop accepting worse states
    * @return   The resulting derivation will appear (all at once) in the side bar
    */
  def ANNEAL(rules: List[Rule],
             steps: Int,
             dilation: Double,
             seed: Random = new Random(),
             vertexLimit: Option[Int] = None) = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = {
      val reduced = AutoReduce.annealingReduce(
        RuleSynthesis.graphToDerivation(g),
        rules,
        steps,
        dilation,
        seed,
        vertexLimit)
      fromDerivationWithHead(reduced)
    }
  }

  // takes a list of rules and rewrites w.r.t. the first that gets a match
  def REWRITE(rules: List[Rule]) = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = {
      for (rule <- rules)
        Matcher.findMatches(rule.lhs, g).headOption.foreach { m =>
            return Iterator.single(layout(Rewriter.rewrite(m, rule.rhs, rule.description)))
        }
      //println("got no match REWRITE: " + rules.map{_.name}.toString())
      Iterator.empty
    }
  }

  // Applies rewrite rules, but only if the rule affects the targeted vertex
  def REWRITE_TARGETED(rule: Rule, vp: VName, targ: Graph => Option[VName]) = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = {
      targ(g).flatMap { vt =>
        //println("REWRITE_TARGETED(" + rule.name + ", " + vt + ")")
        if (g.verts contains vt) {
          val ms = Matcher.initialise(rule.lhs, g, g.verts)
          ms.matchNewNode(vp, vt).flatMap(_.nextMatch())
        } else None
      } match {
        case Some((m,_)) =>
          //println("SUCCESS")
          Iterator.single(layout(Rewriter.rewrite(m, rule.rhs, rule.description)))
        case None =>
          //println("got no match REWRITE_TARGETED: " + rule.name)
          //println("FAILED")
          Iterator.empty
      }
    }
  }

  def REWRITE_TARGET_LIST(rule: Rule, vp: VName, targ: List[VName]): Simproc = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = {
      for(vt <- targ) {
        if (g.verts contains vt) {
          val ms = Matcher.initialise(rule.lhs, g, g.verts)
          ms.matchNewNode(vp, vt).flatMap(_.nextMatch()).map { case (m,_) =>
            return Iterator.single(layout(Rewriter.rewrite(m, rule.rhs, rule.description)))
          }
        }
      }
      Iterator.empty
//      targ(g).flatMap { vt =>
//        //println("REWRITE_TARGETED(" + rule.name + ", " + vt + ")")
//        if (g.verts contains vt) {
//          val ms = Matcher.initialise(rule.lhs, g, g.verts)
//          ms.matchNewNode(vp, vt).flatMap(_.nextMatch())
//        } else None
//      } match {
//        case Some((m,_)) =>
//          //println("SUCCESS")
//          Iterator.single(layout(Rewriter.rewrite(m, rule.rhs, rule.description)))
//        case None =>
//          //println("FAILED")
//          Iterator.empty
//      }
    }
  }

  def REWRITE_METRIC(rules: List[Rule], metric: Graph => Int, target: Int = 0) =
    new Simproc {
      override def simp(g: Graph): Iterator[(Graph, Rule)] = {
        if (metric(g) <= target) return Iterator.empty
        for (rule <- rules) {
          Matcher.findMatches(rule.lhs, g).foreach { m =>
            val (g1,r1) = Rewriter.rewrite(m, rule.rhs, rule.description)
            if (metric(g1) < metric(g)) return Iterator.single(layout((g1,r1)))
          }
        }
        Iterator.empty
      }
    }

  def REWRITE_WEAK_METRIC(rules: List[Rule], metric: Graph => Int) =
    new Simproc {
      override def simp(g: Graph): Iterator[(Graph, Rule)] = {
        if (metric(g) <= 0) return Iterator.empty
        for (rule <- rules) {
          Matcher.findMatches(rule.lhs, g).foreach { m =>
            val (g1,r1) = Rewriter.rewrite(m, rule.rhs, rule.description)
            if (metric(g1) <= metric(g)) return Iterator.single(layout((g1,r1)))
          }
        }
        Iterator.empty
      }
    }

  def REPEAT(s: Simproc): Simproc = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = new Iterator[(Graph, Rule)] {
      var iterS: Iterator[(Graph,Rule)] = s.simp(g)
      var lastGraphS: Graph = g

      override def hasNext: Boolean =
        if (iterS.hasNext) true
        else {
          val iterT = s.simp(lastGraphS)
          if (iterT.hasNext) {
            iterS = iterT
            true
          } else false
        }

      override def next(): (Graph,Rule) =
        if (iterS.hasNext) {
          val (g1,r1) = iterS.next()
          lastGraphS = g1
          (g1,r1)
        } else {
          val iterT = s.simp(lastGraphS)
          if (iterT.hasNext) {
            iterS = iterT
            val (g1,r1) = iterS.next()
            lastGraphS = g1
            (g1,r1)
          } else null
        }
    }
  }
}