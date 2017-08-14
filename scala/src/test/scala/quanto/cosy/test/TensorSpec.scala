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
    var t4 = Tensor(2, 2, (i, j) => new Complex(i + j))
  }

  it should "make identity matrices" in {
    var t1 = new Tensor(Array(Array(zero, one)))
    var t2 = new Tensor(Array(Array(zero), Array(one)))
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = Tensor(2, 2, (i, j) => new Complex(i + j))

    def id = (x: Int) => Tensor.id(x)

    assert((id(2) x id(2)) == id(4))
    assertResult(t4) {
      id(2) o t4
    }
  }

  it should "print nicely" in {
    var t1 = new Tensor(Array(Array(zero, one), Array(Complex(0, -1), Complex(2, 0))))
    var t2 = new Tensor(Array(Array(Complex(0, -1), Complex(0, 1), Complex(1, -1), Complex(-1, 1), Complex(-1, -1))))
    assert((t1 x t2).toString == "    0     0     0     0     0    -i     i   1-i  -1+i  -1-i\n" +
      "   -1     1  -1-i   1+i  -1+i   -2i    2i  2-2i -2+2i -2-2i")
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

  it should "make transposes" in {
    var t1 = Tensor(Array(Array(0, 1, 2, 3)))
    assert(t1.width == 4)
    assert(t1.height == 1)
    assert(t1.transpose.height == 4)
    assert(t1.transpose.width == 1)
  }

  it should "construct permutation matrices" in {
    var p1 = Tensor.permutation(List(0, 1))
    assert(p1 == Tensor.id(2))
    var p2 = Tensor.permutation(5, x => (x + 1) % 5)
    var l1 = Tensor(Array(Array(0, 1, 2, 3, 4)))
    var l2 = Tensor(Array(Array(4, 0, 1, 2, 3)))
    assert((p2 o l1.t) == l2.t)
  }

  it should "construct swap matrices" in {
    var s1 = Tensor.swap(2, x => 1 - x)
    assert(s1.toString == "1 0 0 0\n0 0 1 0\n0 1 0 0\n0 0 0 1")
    var s2 = Tensor.swap(List(0, 2, 1))
    assert(s2.toStringSparse ==
      "1 . . . . . . .\n. . 1 . . . . .\n. 1 . . . . . .\n. . . 1 . . . ." +
        "\n. . . . 1 . . .\n. . . . . . 1 .\n. . . . . 1 . .\n. . . . . . . 1")
  }

  it should "plug tensors into other tensors" in {
    var t1 = Tensor.swap(2, x => 1 - x)
    var t2 = Tensor.id(1)
    assert(t1.plugAbove(t2, x => x) == t1)
    assert(t2.plugAbove(t1, x => x) == t1)
    assert(t2.plugAbove(t1, x => 1 - x) == Tensor.id(4))
  }

  it should "add tensors" in {
    var t2 = Tensor(Array(Array(0, 1)))
    var t3 = Tensor(Array(Array(1, 0)))
    assert(t2 + t3 == Tensor(Array(Array(1, 1))))
    var t1 = Tensor.id(4)
    assert(t1 - t1 == Tensor(4, 4, (i: Int, j: Int) => Complex.zero))
  }

  it should "make diagonal matrices" in {
    var t1 = Tensor.diagonal(Array[Complex](1, 1))
    assert(t1 == Tensor.id(2))
    var t2 = Tensor.diagonal(Array[Complex](2, 3))
    assert(t2 == Tensor(Array(Array(2, 0), Array(0, 3))))
  }

  it should "create powers" in {
    var t1 = Tensor.diagonal(Array[Complex](1, 2))
    var t2 = Tensor.diagonal(Array[Complex](1, 2, 2, 4))
    assert(t1.power(2) == t2)
  }

  behavior of "Tensor comparison"

  it should "element equality" in {
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = Tensor(2, 2, (i, j) => new Complex(i + j))
    var t5 = Tensor(2, 2, (i, j) => new Complex(i + j + 1))
    assert(t3 == t4)
    assert(!(t4 == t5))
  }

  it should "reject differently sized tensors" in {
    var t1 = Tensor.zero(2, 2)
    var t2 = Tensor.zero(2, 1)
    assert(t1 != t2)
  }

  it should "compare roughly (1e-14)" in {
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = Tensor(2, 2, (i, j) => new Complex(i + j + 1e-15))
    assert(t3 isRoughly t4)
  }

  it should "compare roughly (given tolerance)" in {
    var t3 = new Tensor(Array(Array(zero, one), Array(one, Complex(2, 0))))
    var t4 = Tensor(2, 2, (i, j) => new Complex(i + j + 1e-4))
    assert(!t3.isRoughly(t4, 1e-15))
    assert(t3.isRoughly(t4, 1e-2))
  }

  it should "compare with scaling" in {
    var t1 = new Tensor(Array(Array(one, zero), Array(zero, Complex(0, 1))))
    var t2 = new Tensor(Array(Array(Complex(0, 1), zero), Array(zero, Complex(-1, 0))))
    assert(t1.isRoughlyUpToScalar(t2))
    var t3 = new Tensor(Array(Array(Complex(0, 1), zero), Array(zero, Complex(1, 0))))
    assert(!t1.isRoughlyUpToScalar(t3))
  }
}
