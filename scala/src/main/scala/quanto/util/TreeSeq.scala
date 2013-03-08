package quanto.util

sealed abstract class TreeLink[A]
case class NodeLink[A](input: Option[A], outputs: Seq[A]) extends TreeLink[A]
case class WireLink[A](dest: A, shift: Int) extends TreeLink[A]

class TreeSeqFormatException(msg: String) extends Exception(msg)

/**
 * An abstract class representing a tree whose elements also have a sequential ordering, as in a branching history.
 */
abstract class TreeSeq[A] {
  def toSeq: Seq[A]
  def indexOf(a: A): Int
  def parent(a: A): Option[A]
  def children(a: A): Seq[A]


  def flatten: Seq[(Seq[TreeLink[A]], A)] =
    (toSeq.foldLeft(Seq[(Seq[TreeLink[A]], A)]()) { (rows, a) =>
      val node = NodeLink(parent(a), children(a))
      val prev = if (rows.isEmpty) Seq[TreeLink[A]]()
                 else rows.last._1

      var inserted = false
      val shift = node.input.size - node.outputs.size

      val current = prev.foldLeft(Seq[TreeLink[A]]()) { (cols, col) =>
        col match {
          case (WireLink(dest, shift1)) =>
            if (inserted) cols :+ WireLink(dest, shift1 + shift)
            else {
              if (dest == a) {
                inserted = true
                cols :+ node
              } else {
                cols :+ WireLink(dest, shift1)
              }
            }
          case (NodeLink(_, outs)) =>
            outs.foldLeft(cols) { (outCols, out) =>
              if (inserted) outCols :+ WireLink(out, shift)
              else {
                if (out == a) {
                  inserted = true
                  outCols :+ node
                } else {
                  outCols :+ WireLink(out, 0)
                }
              }
            }
        }
      }

      rows :+ (if (!inserted) {
        if (node.input.isEmpty) current :+ node
        else throw new TreeSeqFormatException("Node '" + a.toString + "' occurs before its parent")
      } else {
        current
      }, a)
    })
}
