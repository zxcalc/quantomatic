package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.BlockGenerators._
import quanto.cosy._

import scala.concurrent.duration.Duration
import java.io.File

import quanto.data._
import quanto.rewrite.Matcher
import quanto.util.{FileHelper, Rational}
import quanto.util.json.Json
import quanto.data.Names._

import scala.util.Random

class BlockGeneratorsSpec extends FlatSpec {


  behavior of "ZX generators"

  it should "make the right number of generators" in {
    // CNOTs start at width 2
    assert(zxCNOTs(2).size == 1)
    assert(zxCNOTs(4).size == 3)
    assert(zxTONCs(4).size == 3)
    // Make X and Z twists
    val twists = zxQubitTwists(8)
    assert(twists.size == 16)
  }

  it should "make the right angles" in {
    val twists = zxQubitTwists(8)
    assert(twists(2).graph.vdata("X").asInstanceOf[NodeV].phaseData.values.head.constant == Rational(1, 4))
    assert(twists(15).graph.vdata("Z").asInstanceOf[NodeV].phaseData.values.head.constant == Rational(7, 4))
  }

  it should "make hadamards" in {
    val twists = zxQubitTwists(8)
    assert((zxQubitHadamard o twists(2) o zxQubitHadamard).isRoughlyUpToScalar(twists(3)))
  }

  it should "make correct CNOTs and TNOCs" in {
    assert((zxCNOT(4) o zxCNOT(4)).isRoughlyUpToScalar(Tensor.idWires(4)))
    assert((zxTONC(4) o zxTONC(4)).isRoughlyUpToScalar(Tensor.idWires(4)))
    assert((zxCNOT(4) o zxTONC(4) o zxCNOT(4)).isRoughlyUpToScalar(Tensor.swap(List(3, 1, 2, 0))))
    val c4 = zxCNOT(4)
    val c4graphInterpretation = Interpreter.interpretZXGraph(c4.graph,
      List("i-0", "i-1", "i-2", "i-3"),
      List("o-0", "o-1", "o-2", "o-3"))
    assert(c4.tensor.isRoughlyUpToScalar(c4graphInterpretation))

    val t4 = zxTONC(4)
    val t4graphInterpretation = Interpreter.interpretZXGraph(t4.graph,
      List("i-0", "i-1", "i-2", "i-3"),
      List("o-0", "o-1", "o-2", "o-3"))
    assert(t4.tensor.isRoughlyUpToScalar(t4graphInterpretation))

  }

  it should "not have string edges, only rails" in {
    val blocks: List[Block] = BlockGenerators.ZXGates(4, 1)
    val rows: List[BlockRow] = BlockRowMaker.makeRowsUpToSize(1, blocks, Some(1))
    val stacks = BlockStackMaker.makeStacksOfSize(2, rows)
    val graphs = stacks.map(_.graph)
    val graphsWithStrings = graphs.filter(g => g.edata.values.exists(ed => ed.typ == "string"))
    assert(graphsWithStrings.isEmpty)
  }
}
