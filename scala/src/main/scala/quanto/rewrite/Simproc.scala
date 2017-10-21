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

  def REWRITE(rule: Rule) = new Simproc {
    override def simp(g: Graph): Iterator[(Graph, Rule)] =
      Matcher.findMatches(rule.lhs, g).headOption match {
        case None => Iterator.empty
        case Some(m) =>
          Iterator.single(Rewriter.rewrite(m, rule.rhs))
      }
  }
}