package quanto.cosy.test

import quanto.cosy._
import org.scalatest.FlatSpec
import quanto.data.{Graph, GraphTikz, Rule}
import quanto.rewrite.Matcher
import quanto.cosy.BlockRowMaker._
import quanto.util.FileHelper

/**
  * Created by hector on 28/06/17.
  */


class BlockEnumerationSpec extends FlatSpec {

  implicit def quickList(n: Int): List[Int] = {
    n match {
      case 0 => List()
      case 1 => List(0)
      case m => quickList(m - 1) ::: List(0)
    }
  }

  behavior of "Block Enumeration"

  it should "build a small ZW row" in {
    var rowsAllowed = BlockRowMaker(1, allowedBlocks = BlockGenerators.ZW)
    println(rowsAllowed)
  }

  it should "build bigger ZW rows" in {
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = BlockGenerators.ZW)
    println(rowsAllowed)
    assert(rowsAllowed.length == 11 * 11 + 11)
  }

  it should "stack rows" in {
    var rowsAllowed = BlockRowMaker(1, allowedBlocks = BlockGenerators.ZW)
    var stacks = BlockStackMaker(2, rowsAllowed)
    println(stacks)
  }

  it should "limit wires" in {
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = BlockGenerators.ZW, maxInOut = Option(2))
    var stacks = BlockStackMaker(2, rowsAllowed)
    println(stacks)
    assert(stacks.forall(s => (s.inputs.length <= 2) && (s.outputs.length <= 2)))
  }

  it should "compute tensors" in {
    var rowsAllowed = BlockRowMaker(2, allowedBlocks = BlockGenerators.ZW)
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
      Block(1, 1, " 1 ", Tensor.idWires(1), new Graph()),
      Block(1, 1, " w ", new Tensor(Array(Array[Complex](1, 0), Array[Complex](0, -1)))),
      Block(1, 1, " b ", new Tensor(Array(Array[Complex](0, 1), Array[Complex](1, 0))))
    ))
    var stacks = BlockStackMaker(2, rowsAllowed)
    var s11 = stacks.filter(s => s.inputs.length == 1 && s.outputs.length == 1 && s.tensor.isRoughly(Tensor.idWires(1)))
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
    var b = BlockGenerators.Bian2Qubit(2) // T-gate
    var g = BlockGenerators.Bian2QubitToGraph(b)
    println(g.toString)
  }

  it should "convert a row to a graph" in {
    var B2 = BlockGenerators.Bian2Qubit
    var r = new BlockRow(List(B2(2), B2(3))) // T x H
    var g = BlockRowMaker.predicateRowToGraph(r, BlockGenerators.Bian2QubitToGraph)
    println(g.toString)
  }

  it should "convert a stack to a graph" in {
    var B2 = BlockGenerators.Bian2Qubit
    var r = new BlockRow(List(B2(2), B2(3))) // T x H
    var g = BlockRowMaker.predicateStackToGraph(
      new BlockStack(List(r, r)),
      BlockGenerators.Bian2QubitToGraph)
    println(g)
  }

  behavior of "qutrits and qudits"

  it should "generate enough qutrit generators" in {
    assert(BlockGenerators.ZXQutrit(9).length == (10 + 2 * 81))
  }

  it should "generate enough qudit generators" in {
    assert(BlockGenerators.ZXQudit(3, 9).length == (10 + 2 * 81))
    // And check it is the correct swap tensor:
    assert(BlockGenerators.ZXQudit(3, 9)(1).tensor == BlockGenerators.ZXQutrit(9)(1).tensor)
    assert(BlockGenerators.ZXQudit(4, 8).length == (10 + 2 * math.pow(8, 4 - 1)).toInt)
  }

  it should "have spider rules for qudits" in {
    var Q4 = BlockGenerators.ZXQudit(4, 8)
    var r760 = Q4.find(p => p.name == "r|7|6|0")
    var r230 = Q4.find(p => p.name == "r|2|3|0")
    var r110 = Q4.find(p => p.name == "r|1|1|0")
    if (r760.isDefined && r230.isDefined && r110.isDefined) {
      var rsum = r760.get.tensor o r230.get.tensor
      assert(rsum.isRoughly(r110.get.tensor))
    } else fail("Did not generate the required spiders")
  }

  behavior of "Bell Simple"

  it should "display quantum teleportation" in {
    var BSRow = BlockRowMaker(2, BlockGenerators.BellTeleportation, Option(3))
    var BSStacks = BlockStackMaker(4, BSRow)
    var tp = BSStacks.
      //filterNot(x => x.toString.matches(raw".*\(w\d \).*")).
      //filterNot(x => x.toString.matches(raw".*\( (A|B) \).*")).
      filter(x => x.inputs == List(0)).
      filter(x => x.outputs == List(-1)).
      filter(x => x.tensor.isSameShapeAs(Tensor.id(2))).
      filter(x => x.tensor.isRoughlyUpToScalar(Tensor.id(2)))
    tp.foreach(x => println("---- \n " + x.toString + "\n" + x.tensor))
    assert(tp.length == 4)
  }

  behavior of "ZX Clifford"

  it should "Find CZ gate" in {
    var BSRow = BlockRowMaker(2, BlockGenerators.ZXClifford, Option(2))
    var BSStacks = BlockStackMaker(3, BSRow)
    var tp = BSStacks.
      filter(x=> x.tensor.isSameShapeAs(Tensor.idWires(2))).
      filter(x => x.tensor.isRoughlyUpToScalar(
        Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 1, 0), Array(0, 0, 0, -1))))
      )
    tp.foreach(x => println("---- \n " + x.toString + "\n" + x.tensor))
    assert(tp.nonEmpty)
  }

  behavior of "graph generation"

  val ZXClifford = BlockGenerators.ZXClifford

  it should "make a pair horizontally" in {
    var row = new BlockRow(List(ZXClifford(0), ZXClifford(1)))
    var g = row.graph
    assert(g.verts.size == 6)
  }

  it should "make a pair vertically" in {
    var row = new BlockRow(List(ZXClifford(0)))
    var g = BlockStack(List(row, row)).graph
    assert(g.verts.size == 4)
    assert(g.edges.size == 3)
    assert(g.minimise.edges.size == 1)
  }

  it should "make a 2x2" in {
    var row = new BlockRow(List(ZXClifford(3), ZXClifford(1)))
    var row2 = new BlockRow(List(ZXClifford(1), ZXClifford(3)))
    var g = BlockStack(List(row, row2)).graph
    assert(g.verts.size == 20)
  }

  it should "cache rows" in {
    var row = new BlockRow(List(ZXClifford(0), ZXClifford(1)))
    var row2 = new BlockRow(List(ZXClifford(3), ZXClifford(0)))
    var row3 = new BlockRow(List(ZXClifford(0), ZXClifford(3)))
    var b1 = BlockStack(List(row, row2, row3))
    var b2 = BlockStack(List(row2, row3)).append(row)
    var b3 = BlockStack(List()).append(row3).append(row2).append(row)
    var g1 = b1.graph
    var g2 = b2.graph
    assert(b1.tensor == b2.tensor)
    assert(b1.tensor == b3.tensor)
    assert(g1.edges.size == g2.edges.size)
    assert(g1.verts.toList.sorted == g2.verts.toList.sorted)
  }

  /*
  it should "make lots of graphs" in {
    val bs = BlockStackMaker(2, BlockRowMaker.makeRowsOfSize(2,allowedBlocks = ZXClifford, maxInOut = Some(3)))
    bs.foreach(b => FileHelper.printJson("./blockstacks/" + b.graph.hashCode + ".qgraph",Graph.toJson(b.graph)))
  }
  */
}