package quanto.util

class TreeSeqFormatException(msg: String) extends Exception(msg)

/**
 * An abstract class representing a tree whose elements also have a sequential ordering, as in a branching history.
 */
abstract class TreeSeq[A] {
  import TreeSeq._
  def toSeq: Seq[A]
  def indexOf(a: A): Int
  def parent(a: A): Option[A]
  def children(a: A): Seq[A]

  private def padding(size: Int) =
    Seq.fill[WhiteSpace[A]](size) { WhiteSpace[A](collapseBottom = false, collapseTop = true) }

  def flatten: Seq[(Seq[Decoration[A]], A)] =
    (toSeq.foldLeft(Seq[(Seq[Decoration[A]], A)]()) { (rows, a) =>
      val node = NodeLink(parent(a), children(a))
      val prev = if (rows.isEmpty) Seq[Decoration[A]]()
                 else rows.last._1

      var inserted = false
      var pad = 0

      // traverse the previous decoration list from left to right and construct the current decoration list
      val current = prev.foldLeft(Seq[Decoration[A]]()) { (cols,col) =>
        col match {
          case (WireLink(dest)) =>
            if (inserted) cols :+ WireLink(dest)
            else {
              if (dest == a) {
                inserted = true
                //println("pad size = " + pad.size)
                (cols :+ node) ++ padding(if (node.outputs.size < 2) pad else pad - node.outputs.size)
              } else {
                cols :+ WireLink(dest)
              }
            }
          case (NodeLink(_, outs)) =>
            if (outs.isEmpty) {
              pad += 1
              cols :+ WhiteSpace[A](collapseBottom = true, collapseTop = false)
            }
            else outs.foldLeft(cols) { (outCols,out) =>
              if (inserted) outCols :+ WireLink(out)
              else {
                if (out == a) {
                  inserted = true
                  //println("pad size = " + pad.size)
                  (outCols :+ node) ++ padding(if (node.outputs.size < 2) pad else pad - node.outputs.size)
                } else {
                  outCols :+ WireLink(out)
                }
              }
            }
          case WhiteSpace(false,_) =>
            pad += 1
            cols :+ WhiteSpace[A](collapseBottom = true, collapseTop = false)
          case WhiteSpace(true,_) => cols
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

object TreeSeq {
  sealed abstract class Decoration[A]
  case class NodeLink[A](input: Option[A], outputs: Seq[A]) extends Decoration[A]
  case class WireLink[A](dest: A) extends Decoration[A]

  // placeholder for whitespace. if collapse is false, the space propagates to the next rank. if it is true, it
  // disappears at the next rank.
  case class WhiteSpace[A](collapseBottom: Boolean, collapseTop: Boolean) extends Decoration[A]

  // figure out how wide a given decoration sequence is
  def decorationWidth[A](dec: Seq[Decoration[A]]) = {
    var topIndex = 0
    var bottomIndex = 0
    var sz = 0
    for (d <- dec) d match {
      case WireLink(_) =>
        topIndex += 1
        bottomIndex += 1
        sz = math.max(topIndex,bottomIndex)
      case NodeLink(inputOpt, outputs) =>
        topIndex += inputOpt.size
        bottomIndex += math.max(1, outputs.size)
        sz = math.max(topIndex,bottomIndex)
      case WhiteSpace(collapseBottom, collapseTop) =>
        if (!collapseBottom) bottomIndex += 1
        if (!collapseTop) topIndex += 1
    }

    sz
  }
}
