package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.{Complex, Tensor, Interpreter}

/**
  * Created by hector on 24/05/17.
  */
class InterpreterSpec extends FlatSpec {
  behavior of "The Interpreter"
  var one = Complex.one
  var zero = Complex.zero

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
    var g1 = Interpreter.interpretSpider(true,math.Pi/8,1,2)
    var g2 = Interpreter.interpretSpider(true,math.Pi/8,1,1)
    var g3 = Interpreter.interpretSpider(true,math.Pi/4,1,2)
    assert(((Tensor.id(2) x g2) o g1).isRoughly(g3))
  }

  it should "apply Hadamards" in {
    var h1 = Tensor.hadamard
    var h2 = h1 x h1
    var g = Interpreter.interpretSpider(true,math.Pi/4,1,2)
    var r = Interpreter.interpretSpider(false,math.Pi/4,1,2)
    assert((h2 o r o h1).isRoughly(g))
  }

}
