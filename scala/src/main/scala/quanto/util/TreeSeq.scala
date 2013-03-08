package quanto.util

sealed abstract class TreeLink[A]
case class NodeLink[A](input: Option[A], outputs: Seq[A]) extends TreeLink[A]
case class WireLink[A](parent: A, shift: Int) extends TreeLink[A]

class TreeSeqFormatException(msg: String) extends Exception(msg)

/**
 * An abstract class representing a tree whose elements also have a sequential ordering, as in a branching history.
 */
abstract class TreeSeq[A] {
  def toSeq: Seq[A]
  def indexOf(a: A): Int
  def parent(a: A): Option[A]
  def children(a: A): Seq[A]


  def flatten: List[(List[TreeLink[A]], A)] =
    toSeq.reverse.foldLeft(List[(List[TreeLink[A]], A)]()) { (rows, a) =>
      val node = NodeLink(parent(a), children(a))
      val prev = rows match { case ((p,_)::_) => p; case _ => List[TreeLink[A]]() }

      var inserted = false
      val shift = node.input.size - node.outputs.size

      val current = prev.reverse.foldLeft(List[TreeLink[A]]()) { (cols, col) =>
        col match {
          case (WireLink(p1, shift1)) =>
            if (inserted) WireLink(p1, shift1 + shift) :: cols
            else {
              if (node.input.exists(p => p1 == p)) {
                inserted = true
                node :: cols
              } else {
                WireLink(p1, shift1) :: cols
              }
            }
          case (NodeLink(_, outs)) =>
            outs.reverse.foldLeft(cols) { (outCols, out) =>
              if (inserted) WireLink(out, shift) :: outCols
              else {
                if (node.input.exists(p => out == p)) {
                  inserted = true
                  node :: outCols
                } else {
                  WireLink(out, 0) :: outCols
                }
              }
            }
        }
      }

      (current, a) :: rows
    }
}
