package quanto.rewrite

import quanto.data._


abstract class Simproc {
  def pull(g: Graph): Option[(DStep,Simproc)]

  def >>(t: Simproc) = {
    val s = this
    new Simproc {
      override def pull(g: Graph): Option[(DStep, Simproc)] = None
    }
  }

  def __rshift__(t: Simproc) = this >> t
}

object Simproc {
  object empty extends Simproc { def pull(g: Graph) = None }
}