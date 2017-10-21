package quanto.rewrite

import quanto.data._


abstract class Simproc {
  def simp(g: Graph): Iterator[(Graph, Rule)]

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
            val (g,r) = iterS.next()
            lastGraphS = g
            (g,r)
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
  object EMPTY extends Simproc { override def simp(g: Graph): Iterator[(Graph,Rule)] = Iterator.empty }

  // takes a list of rules and rewrites w.r.t. the first that gets a match
  def REWRITE(rules: List[Rule]) = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] = {
      for (rule <- rules)
        Matcher.findMatches(rule.lhs, g).headOption.foreach { m =>
          return Iterator.single(Rewriter.rewrite(m, rule.rhs, rule.description))
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
          val (g,r) = iterS.next()
          lastGraphS = g
          (g,r)
        }
        else {
          val iterT = s.simp(lastGraphS)
          if (iterT.hasNext) {
            iterS = iterT
            val (g,r) = iterS.next()
            lastGraphS = g
            (g,r)
          } else null
        }
    }
  }
}