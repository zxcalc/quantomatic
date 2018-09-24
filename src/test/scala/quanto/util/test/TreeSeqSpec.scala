package quanto.util.test

import org.scalatest._
import quanto.util._

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

  it should "flatten" in {
    tree.flatten.foreach{ row => println(row) }
  }
}
