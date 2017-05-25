package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.Interpreter.AngleMap
import quanto.cosy.{AdjMat, Complex, Graph, Interpreter, Tensor}

/**
  * Created by hector on 24/05/17.
  */
class InterpreterSpec extends FlatSpec {
  behavior of "The Interpreter"
  val pi = math.Pi
  var one = Complex.one

  it should "make hadamards" in {
    // Via "new"
    var h3 = Interpreter.makeHadamards(2)
    assert(h3.toString == " 0.5000000000000001  0.5000000000000001  0.5000000000000001  0.5000000000000001\n" +
      " 0.5000000000000001 -0.5000000000000001  0.5000000000000001 -0.5000000000000001\n" +
      " 0.5000000000000001  0.5000000000000001 -0.5000000000000001 -0.5000000000000001\n" +
      " 0.5000000000000001 -0.5000000000000001 -0.5000000000000001  0.5000000000000001")
    var h33 = h3 o h3
    assert((h33 - Tensor.id(4)).isRoughly(Tensor.zero(4, 4)))
  }

  it should "make Green Spiders" in {
    def gs(angle: Double, in: Int, out: Int) = Interpreter.interpretSpider(true, angle, in, out)

    var g12a0 = Interpreter.interpretSpider(true, 0, 1, 2)
    var g21aPi = Interpreter.interpretSpider(true, math.Pi, 2, 1)
    var g11aPi2 = Interpreter.interpretSpider(true, math.Pi / 2.0, 1, 1)
    var t = gs(math.Pi / 4, 1, 1)
    var g2 = gs(math.Pi / 2, 1, 1)
    assert((t o t).isRoughly(g2))
    assert((g2 o g2).isRoughly(gs(math.Pi, 1, 1)))
  }

  it should "make red spiders" in {
    def rs(angle: Double, in: Int, out: Int) = Interpreter.interpretSpider(false, angle, in, out)

    assert(rs(math.Pi, 1, 1).isRoughly(Tensor(Array(Array(0, 1), Array(1, 0)))))
    var rp2 = rs(math.Pi / 2, 1, 1)
    assert((rp2 o rp2).isRoughly(rs(math.Pi, 1, 1)))
    assert((rp2 o rp2 o rp2 o rp2).isRoughly(Tensor.id(2)))
    assert(Interpreter.cached.contains("true:3.141592653589793:2:1"))
  }

  it should "respect spider law" in {
    var g1 = Interpreter.interpretSpider(true, math.Pi / 8, 1, 2)
    var g2 = Interpreter.interpretSpider(true, math.Pi / 8, 1, 1)
    var g3 = Interpreter.interpretSpider(true, math.Pi / 4, 1, 2)
    assert(((Tensor.id(2) x g2) o g1).isRoughly(g3))
  }

  it should "apply Hadamards" in {
    var h1 = Tensor.hadamard
    var h2 = h1 x h1
    var g = Interpreter.interpretSpider(true, math.Pi / 4, 1, 2)
    var r = Interpreter.interpretSpider(false, math.Pi / 4, 1, 2)
    assert((h2 o r o h1).isRoughly(g))
  }

  it should "process small graphs" in {
    var amat = new AdjMat(numRedTypes = 0, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, true))
    println(amat)
    val i1 = Interpreter.interpretGraph(Graph(amat), Map(0 -> 0), Map())
    assert(i1.isRoughly(Tensor(Array(Array(1, 0, 0, 1)))))
    val i2 = Interpreter.interpretGraph(Graph(amat), Map(0 -> math.Pi), Map())
    assert(i2.isRoughly(Tensor(Array(Array(1, 0, 0, -1)))))
  }
  var zero = Complex.zero
  it should "process entire graphs" in {
    // Simple red and green identities
    var simpleGreenAM: AngleMap = Map(0 -> 0, 1 -> 0)
    var simpleRedAM: AngleMap = Map(0 -> pi)
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, true))
    val i1 = Interpreter.interpretGraph(Graph(amat), simpleGreenAM, simpleRedAM)
    assert(i1.isRoughly(Interpreter.interpretSpider(false, pi, 2, 0)))
  }
  it should "satisfy the Euler identity" in {
    // Euler identity
    var gHAM: AngleMap = Map(0 -> pi / 2, 1 -> pi / 2)
    var rHAM: AngleMap = Map(0 -> pi / 2)
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))
    println(amat)
    val i2 = Interpreter.interpretGraph(Graph(amat), gHAM, rHAM)
    val i3 = Interpreter.interpretSpider(true, 0, 2, 0) o (Tensor.id(2) x Tensor.hadamard)
    println(i3.scaled(i2.contents(0)(0) / i3.contents(0)(0)))
    assert(i2.isRoughly(i3.scaled(i2.contents(0)(0) / i3.contents(0)(0))))
  }

}
