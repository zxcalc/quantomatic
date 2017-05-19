package quanto.cosy.test

import org.scalatest._
import quanto.cosy._


class TensorSpec extends FlatSpec {
  behavior of "Tensors"
  var one = Complex.one
  var zero = Complex.zero
  var t1: Tensor = Tensor(Array(Array(zero)))
  var t2: Tensor = Tensor(Array(Array(zero)))
  var t3: Tensor = Tensor(Array(Array(zero)))
  var t4: Tensor = Tensor(Array(Array(zero)))
  // This values will be overwritten

  it should "be constructable" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2,0))))
    assert(t3(1, 1) == Complex(2, 0))
  }

  it should "accept generating functions" in {
    var t4 = new Tensor(2, 2, (i, j) => new Complex(i + j))
  }

  it should "compare tensors" in {
    assert(t3 == t4)
  }

  it should "make identity matrices" in {
    def id = (x: Int) => Tensor.id(x)

    assert((id(2) x id(2)) == id(4))
    assertResult(t4) {
      id(4) o t4
    }
  }

  it should "fail bad compositions" in {
    intercept[java.lang.IllegalArgumentException] {
      t2 o t3
    }
  }

  it should "make tensor products" in {
    assert((t1 x t2) == Tensor(Array(Array(zero, zero), Array(zero, one))))
    assert((t1 x t1).width == 4)
  }
}
