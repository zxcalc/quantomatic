package quanto.cosy

import quanto.cosy._

import scala.runtime.RichInt

/**
  * A tensor-valued interpretation of a graph
  *
  * For now we stick to the ZX-calculus
  *
  * This means that we will deal with AdjMat, not general graphs
  */


class Tensor(c: Array[Array[Complex]]) {
  type contentType = Array[Array[Complex]]
  val contents: contentType = c

  def this(height: Int, width: Int, generator: Tensor.Generator) = {
    this(Tensor.generatorToMatrix(height, width, generator))
  }

  val width: Int = contents(0).length

  val height: Int = contents.length

  val isDiagramShape: Boolean = {
    def log2(x: Int): Double = Math.log(x) / math.log(2)

    val rightWidth = log2(width) == log2(width).floor
    val rightHeight = log2(height) == log2(height).floor
    rightWidth && rightHeight
  }

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

  def transpose: Tensor = {
    new Tensor(this.width, this.height, (i, j) => this.c(j)(i))
  }

  def t: Tensor = this.transpose

  def plug(that: Tensor, plugThatOutputsToThisInputs: Int => Int): Tensor = {
    require(this.isDiagramShape)
    require(that.isDiagramShape)
    val sizeNeeded = math.max(this.width, that.height)
    val stretchedThis = this.widen(sizeNeeded)
    val stretchedThat = that.heighten(sizeNeeded)
    val sigma = Tensor.swap((math.log(sizeNeeded) / math.log(2)).toInt, plugThatOutputsToThisInputs)
    stretchedThis o sigma o stretchedThat
  }

  def widen(n: Int): Tensor = {
    if (this.width < n) (this x Tensor.id(2)).widen(n) else this
  }

  def heighten(n: Int): Tensor = {
    if (this.height < n) (this x Tensor.id(2)).heighten(n) else this
  }

  override def toString: String = {
    val minWidth = 1
    val longestLength = this.contents.flatten.map(s => s.toString.length).foldLeft(minWidth)((a, b) => Math.max(a, b))

    def pad(s: String): String = if (s.length < longestLength) pad(s + " ") else s

    this.contents.map(line => line.map(s => pad(s.toString)).mkString(" ")).mkString("\n")
  }

  def toStringSparse: String = {
    val minWidth = 1
    val longestLength = this.contents.flatten.map(s => s.toString.length).foldLeft(minWidth)((a, b) => Math.max(a, b))

    def pad(s: String): String = if (s.length < longestLength) pad(s + " ") else s

    def sparse(s: String): String = if (s == "0") "." else s

    this.contents.map(line => line.map(s => pad(sparse(s.toString))).mkString(" ")).mkString("\n")
  }

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
  type Matrix = Array[Array[Complex]]

  type Generator = (Int, Int) => Complex


  def generatorToMatrix(height: Int, width: Int, generator: Tensor.Generator): Matrix = {
    require(width > 0)
    require(height > 0)
    val e1 = emptyMatrix(height, width)
    for (j <- 0 until height) {
      for (i <- 0 until width) {
        e1(j)(i) = generator(j, i)
      }
    }
    e1
  }


  def emptyMatrix(height: Int, width: Int): Matrix = {
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

  def apply(cInt: Array[Array[Int]]): Tensor = {
    new Tensor(cInt.length, cInt(0).length, (i, j) => Complex.doubleToComplex(cInt(i)(j)))
  }

  def apply(height: Int, width: Int, generator: Tensor.Generator): Tensor = {
    new Tensor(generatorToMatrix(height, width, generator))
  }

  def permutationMatrix(size: Int, gen: Int => Int): Matrix = {
    val base = emptyMatrix(size, size)
    for (i <- 0 until size) {
      base(gen(i))(i) = Complex.one
    }
    base
  }

  def permutation(size: Int, gen: Int => Int): Tensor = {
    new Tensor(permutationMatrix(size, gen))
  }

  def permutation(asList: List[Int]): Tensor = {
    val gen = (x: Int) => asList(x)
    new Tensor(permutationMatrix(asList.length, gen))
  }

  def permutation(asArray: Array[Int]): Tensor = {
    val gen = (x: Int) => asArray(x)
    new Tensor(permutationMatrix(asArray.length, gen))
  }

  def swap(size: Int, gen: Int => Int): Tensor = {
    def padLeft(s: String, n: Int): String = if (s.length < n) padLeft("0" + s, n) else s

    def permGen(i: Int): Int = {
      val binaryStringIn = padLeft((i: RichInt).toBinaryString, size)
      val permedString = (for (j <- 0 until size) yield binaryStringIn(gen(j))).mkString("")
      Integer.parseInt(permedString, 2)
    }

    permutation(math.pow(2, size).toInt, permGen)
  }

  def swap(asList: List[Int]): Tensor = {
    val gen = (x: Int) => asList(x)
    swap(asList.length, gen)
  }
}
