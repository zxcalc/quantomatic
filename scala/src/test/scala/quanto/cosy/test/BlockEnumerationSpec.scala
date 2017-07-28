package quanto.cosy.test

import quanto.cosy._
import org.scalatest.FlatSpec
import quanto.data.Rule
import quanto.rewrite.Matcher

/**
  * Created by hector on 28/06/17.
  */

class BlockEnumerationSpec extends FlatSpec {
  behavior of "Block Enumeration"

  it should "build a small ZW row" in {
    var rowsAllowed = BlockRowMaker(1, allowedBlocks = BlockRowMaker.ZW)
    println(rowsAllowed)
  }

  it should "build bigger ZW rows" in {
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = BlockRowMaker.ZW)
    println(rowsAllowed)
    assert(rowsAllowed.length == 11 * 11 + 11)
  }

  it should "stack rows" in {
    var rowsAllowed = BlockRowMaker(1, allowedBlocks = BlockRowMaker.ZW)
    var stacks = BlockStackMaker(2, rowsAllowed)
    println(stacks)
  }

  it should "limit wires" in {
    var rowsAllowed = BlockRowMaker(2, maxInOut = 2, allowedBlocks = BlockRowMaker.ZW)
    var stacks = BlockStackMaker(2, rowsAllowed)
    println(stacks)
    assert(stacks.forall(s => (s.inputs <= 2) && (s.outputs <= 2)))
  }

  it should "compute tensors" in {
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = BlockRowMaker.ZW)
    var stacks = BlockStackMaker(2, rowsAllowed)
    for (elem <- stacks) {
      println("---\n" + elem.toString + " = \n" + elem.tensor)
    }
  }

  it should "compute cup x id" in {
    var allowedBlocks = List(
      // BOTTOM TO TOP!
      Block(1, 1, " 1 ", Tensor.idWires(1)),
      Block(0, 2, "cup", new Tensor(Array(Array[Complex](1, 0, 0, 1))).transpose)
    )
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = allowedBlocks)
    assert(rowsAllowed.filter(r => r.toString == "cup x  1 ").head.tensor
      == Tensor(Array(Array(1, 0, 0, 0, 0, 0, 1, 0), Array(0, 1, 0, 0, 0, 0, 0, 1))).transpose)
  }

  it should "find wire identities" in {
    var rowsAllowed = BlockRowMaker(1, allowedBlocks = List(
      Block(1, 1, " 1 ", Tensor.idWires(1)),
      Block(1, 1, " w ", new Tensor(Array(Array[Complex](1, 0), Array[Complex](0, -1)))),
      Block(1, 1, " b ", new Tensor(Array(Array[Complex](0, 1), Array[Complex](1, 0))))
    ))
    var stacks = BlockStackMaker(2, rowsAllowed)
    var s11 = stacks.filter(s => s.inputs == 1 && s.outputs == 1 && s.tensor.isRoughly(Tensor.idWires(1)))
    println(s11)
  }

  behavior of "blocks and JSON"

  it should "make blocks into JSON" in {
    var b = new Block(1, 1, " w ", new Tensor(Array(Array[Complex](1, 0), Array[Complex](0, -1))))
    var js1 = b.toJson
    var b2 = Block.fromJson(js1)
    assert(b2.inputs == b.inputs)
    assert(b2.tensor == b.tensor)
    println(b2)
  }

  it should "make rows into JSON" in {
    var r = new BlockRow(List(new Block(1, 1, " w ", new Tensor(Array(Array[Complex](1, 0), Array[Complex](0, -1))))))
    var js1 = r.toJson
    var r2 = BlockRow.fromJson(js1)
    assert(r2.inputs == r.inputs)
    assert(r2.tensor == r.tensor)
    println(r2)
  }

  it should "make stacks into JSON" in {
    var s = new BlockStack(
      List(new BlockRow(List(new Block(1, 1, " w ", new Tensor(Array(Array[Complex](1, 0), Array[Complex](0, -1)))))))
    )
    var js1 = s.toJson
    var s2 = BlockStack.fromJson(js1)
    assert(s2.inputs == s.inputs)
    assert(s2.tensor == s.tensor)
    println(s2)
  }

  behavior of "Stack to Graph"

  it should "convert a block to a graph" in {
    var b = BlockRowMaker.Bian2Qubit(2) // T-gate
    var g = BlockRowMaker.Bian2QubitToGraph(b)
    println(g.toString)
  }

  it should "convert a row to a graph" in {
    var B2 = BlockRowMaker.Bian2Qubit
    var r = new BlockRow(List(B2(2), B2(3))) // T x H
    var g = BlockRowMaker.rowToGraph(r, BlockRowMaker.Bian2QubitToGraph)
    println(g.toString)
  }

  it should "convert a stack to a graph" in {
    var B2 = BlockRowMaker.Bian2Qubit
    var r = new BlockRow(List(B2(2), B2(3))) // T x H
    var g = BlockRowMaker.stackToGraph(
      new BlockStack(List(r, r)),
      BlockRowMaker.Bian2QubitToGraph)
    println(g)
  }

  behavior of "qutrits and qudits"

  it should "generate enough qutrit generators" in {
    assert(BlockRowMaker.ZXQutrit(9).length == (10 + 2*81))
  }

  it should "generate enough qudit generators" in {
    assert(BlockRowMaker.ZXQudit(3,9).length == (10 + 2*81))
    // And check it is the correct swap tensor:
    assert(BlockRowMaker.ZXQudit(3,9)(1).tensor == BlockRowMaker.ZXQutrit(9)(1).tensor)
    assert(BlockRowMaker.ZXQudit(4,8).length == (10 + 2*math.pow(8,4-1)).toInt)
  }

  it should "have spider rules for qudits" in {
    var Q4 = BlockRowMaker.ZXQudit(4,8)
    var r760 = Q4.find(p =>   p.name=="r|7|6|0")
    var r230 = Q4.find(p =>   p.name=="r|2|3|0")
    var r110 = Q4.find(p =>   p.name=="r|1|1|0")
    if(r760.isDefined && r230.isDefined && r110.isDefined) {
      var rsum = r760.get.tensor o r230.get.tensor
      assert(rsum.isRoughly(r110.get.tensor))
    } else fail("Did not generate the required spiders")
  }
}