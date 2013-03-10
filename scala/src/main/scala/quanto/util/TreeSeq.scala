package quanto.util

sealed abstract class TreeLink[A]
case class NodeLink[A](input: Option[A], outputs: Seq[A]) extends TreeLink[A]
case class WireLink[A](dest: A) extends TreeLink[A]

// placeholder for whitespace. if collapse is false, the space propagates to the next rank. if it is true, it
// disappears at the next rank.
case class SpaceLink[A](collapseBottom: Boolean, collapseTop: Boolean) extends TreeLink[A]

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
      var pad = Seq[TreeLink[A]]()

      // traverse the previous decoration list from left to right and construct the current decoration list
      val current = prev.foldLeft(Seq[TreeLink[A]]()) { (cols,col) =>
        col match {
          case (WireLink(dest)) =>
            if (inserted) cols :+ WireLink(dest)
            else {
              if (dest == a) {
                inserted = true
                //println("pad size = " + pad.size)
                (cols :+ node) ++ pad
              } else {
                cols :+ WireLink(dest)
              }
            }
          case (NodeLink(_, outs)) =>
            if (outs.isEmpty) {
              pad :+=  SpaceLink[A](collapseBottom = false, collapseTop = true)
              cols :+ SpaceLink[A](collapseBottom = true, collapseTop = false)
            }
            else outs.foldLeft(cols) { (outCols,out) =>
              if (inserted) outCols :+ WireLink(out)
              else {
                if (out == a) {
                  inserted = true
                  //println("pad size = " + pad.size)
                  (outCols :+ node) ++ pad
                } else {
                  outCols :+ WireLink(out)
                }
              }
            }
          case SpaceLink(false,_) =>
            pad :+=  SpaceLink[A](collapseBottom = false, collapseTop = true)
            cols :+ SpaceLink[A](collapseBottom = true, collapseTop = false)
          case SpaceLink(true,_) => cols
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
