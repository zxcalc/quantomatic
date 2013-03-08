package quanto.util.test

import org.scalatest._
import quanto.util._

// simple implementation. no complaints if the tree is invalid
case class SimpleTreeSeq[A](seq: Seq[(A,Option[A])] = Seq()) extends TreeSeq[A] {
  def toSeq = seq.map(_._1)
  def indexOf(a: A) = seq.indexWhere(_._1 == a)
  def parent(a: A) = seq.find(_._1 == a) match { case Some((_,p)) => p; case _ => None }
  def children(a: A) = seq.filter(_._2 == Some(a)).map(_._1)

  def :+(a: A, p: Option[A]) = new SimpleTreeSeq(seq :+ (a,p))
}

class TreeSeqSpec extends FlatSpec {
  var tree = new SimpleTreeSeq[Int]()
  behavior of "A tree sequence"

  it should "be constructable" in {
    tree = tree :+ (0, None)
    tree = tree :+ (1, Some(0))
    tree = tree :+ (2, Some(1))
    tree = tree :+ (3, Some(1))
    tree = tree :+ (4, Some(2))
  }
}
