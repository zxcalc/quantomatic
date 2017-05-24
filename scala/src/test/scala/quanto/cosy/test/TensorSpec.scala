package quanto.cosy.test

import org.scalatest._
import quanto.cosy._


class TensorSpec extends FlatSpec {
  behavior of "Tensors"
  var one = Complex.one
  var zero = Complex.zero
  // This values will be overwritten

  it should "be constructable" in {
    // Via "new"
    var t1 = new Tensor(Array(Array(zero, one)))
    // Via Tensor()
    var t2 = Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    assert(t3(1, 1) == Complex(2, 0))
  }

  it should "accept generating functions" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = new Tensor(2, 2, (i, j) => new Complex(i + j))
  }

  it should "compare tensors" in {
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = new Tensor(2, 2, (i, j) => new Complex(i + j))
    assert(t3 == t4)
  }

  it should "make identity matrices" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = new Tensor(2, 2, (i, j) => new Complex(i + j))

    def id = (x: Int) => Tensor.id(x)

    assert((id(2) x id(2)) == id(4))
    assertResult(t4) {
      id(2) o t4
    }
  }

  it should "fail bad compositions" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    intercept[java.lang.IllegalArgumentException] {
      t2 o t3
    }
  }

  it should "make tensor products" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    assert((t1 x t2) == Tensor(Array(Array(zero, zero), Array(zero, one))))
    assert((t1 x t1).width == 4)
  }
}
