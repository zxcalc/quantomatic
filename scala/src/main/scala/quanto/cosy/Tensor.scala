package quanto.cosy

import quanto.cosy._

/**
  * A tensor-valued interpretation of a graph
  *
  * For now we stick to the ZX-calculus
  *
  * This means that we will deal with AdjMat, not general graphs
  */


class Tensor(c: Array[Array[Complex]]) {
  val contents = c

  def this(height: Int, width: Int, generator: Tensor.Generator) = {
    this(Tensor.generatorToMatrix(height, width, generator))
  }

  val width: Int = contents(0).length

  val height: Int = contents.length

  def entry(down: Int, across: Int): Complex = contents(down)(across)

  def compose(that: Tensor): Tensor = {
    require(this.width == that.height)

    def comp(i: Int, j: Int) = {
      (0 until this.width).
        map(k => this (i, k) * that(k, j)).
        foldLeft(Complex.zero)((a, b) => a + b)
    }

    new Tensor(this.height, that.width, comp)
  }

  def o(that: Tensor): Tensor = {
    this.compose(that)
  }

  def multiply(that: Tensor): Tensor = {
    val m = this.width

    val n = this.height

    val p = that.width

    val q = that.height


    def a_sub1(i: Int, j: Int) = i / q
    def a_sub2(i: Int, j: Int) = j / p

    def b_sub1(i: Int, j: Int) = i % q
    def b_sub2(i: Int, j: Int) = j % p


    def kronecker(i: Int, j: Int): Complex = {
      // println(List(a_sub1(i, j), a_sub2(i, j), b_sub1(i, j), b_sub2(i, j)).mkString(" "))
      this (a_sub1(i, j), a_sub2(i, j)) * that(b_sub1(i, j), b_sub2(i, j))
    }

    new Tensor(n * q, m * p, kronecker)
  }

  def x(that: Tensor): Tensor = {
    this.multiply(that)
  }

  override def toString: String =
    this.contents.map(s => s.mkString(" ")).mkString("\n")

  def apply(down: Int, across: Int): Complex = {
    this.entry(down, across)
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Tensor =>
        (that canEqual this) &&
          this.contents.deep == that.contents.deep
      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Tensor]

  override def hashCode(): Int = this.contents.deep.hashCode()
}

object Tensor {

  type Generator = (Int, Int) => Complex


  def generatorToMatrix(height: Int, width: Int, generator: Tensor.Generator): Array[Array[Complex]] = {
    require(width > 0)
    require(height > 0)
    val e1 = emptyArray(height, width)
    for (j <- 0 until height) {
      for (i <- 0 until width) {
        e1(j)(i) = generator(j, i)
      }
    }
    e1
  }


  def emptyArray(height: Int, width: Int): Array[Array[Complex]] = {
    (for (j <- 0 until height) yield
      (for (i <- 0 until width) yield Complex.zero).toArray).toArray
  }


  def id(n: Int): Tensor = {
    new Tensor(n, n, (a: Int, b: Int) => if (a == b) new Complex(1) else new Complex(0))
  }

  def idPow(log2n: Int): Tensor = {
    id(math.pow(2, log2n).toInt)
  }

  def apply(c: Array[Array[Complex]]) = new Tensor(c)

  def apply(height: Int, width: Int, generator: Tensor.Generator) = {
    Tensor.generatorToMatrix(height, width, generator)
  }
}
