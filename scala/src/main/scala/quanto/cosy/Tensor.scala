package quanto.cosy

import quanto.util.json.{JsonArray, JsonObject}

import scala.runtime.RichInt

/**
  * A tensor-valued interpretation of a graph
  *
  * For now we stick to the ZX-calculus
  *
  * This means that we will deal with AdjMat, not general graphs
  */


class Tensor(c: Array[Array[Complex]]) {
  // A tensor as a matrix in the computational basis
  type contentType = Array[Array[Complex]]
  lazy val normalised: Tensor = this.normalise()
  override lazy val hashCode: Int = this.contents.flatten.count(c => c.re > 0)
  val contents: contentType = c
  val width: Int = contents(0).length
  val height: Int = contents.length
  val isDiagramShape: Boolean = {
    def log2(x: Int): Double = Math.log(x) / math.log(2)

    val rightWidth = log2(width) == log2(width).floor
    val rightHeight = log2(height) == log2(height).floor
    rightWidth && rightHeight
  }

  def compose(that: Tensor): Tensor = {
    // this o that
    require(this.width == that.height)

    def comp(i: Int, j: Int) = {
      (0 until this.width).
        map(k => this (i, k) * that(k, j)).
        foldLeft(Complex.zero)((a, b) => a + b)
    }

    Tensor(this.height, that.width, comp)
  }

  def o(that: Tensor): Tensor = {
    // composition
    this.compose(that)
  }

  def multiply(that: Tensor): Tensor = {
    // tensor product
    val m = this.width

    val n = this.height

    val p = that.width

    val q = that.height


    def a_sub1(i: Int, j: Int) = i / q

    def a_sub2(i: Int, j: Int) = j / p

    def b_sub1(i: Int, j: Int) = i % q

    def b_sub2(i: Int, j: Int) = j % p


    def kronecker(i: Int, j: Int): Complex = {
      this (a_sub1(i, j), a_sub2(i, j)) * that(b_sub1(i, j), b_sub2(i, j))
    }

    Tensor(n * q, m * p, kronecker)
  }

  def x(that: Tensor): Tensor = {
    //tensor product
    this.multiply(that)
  }

  def t: Tensor = this.transpose

  def transpose: Tensor = {
    // transpose
    Tensor(this.width, this.height, (i, j) => this.c(j)(i))
  }

  def dagger: Tensor = {
    // conjugate transpose
    this.conjugate.transpose
  }

  def conjugate: Tensor = {
    Tensor(this.height, this.width, (i, j) => this.c(i)(j).conjugate)
  }

  def plugAbove(that: Tensor, plugThatOutputsToThisInputs: Int => Int): Tensor = {
    // this o pluggins o that
    require(this.isDiagramShape)
    require(that.isDiagramShape)
    val sizeNeeded = math.max(this.width, that.height)
    val stretchedThis = this.widen(sizeNeeded)
    val stretchedThat = that.heighten(sizeNeeded)
    val sigma = Tensor.swap((math.log(sizeNeeded) / math.log(2)).toInt, plugThatOutputsToThisInputs)
    stretchedThis o sigma o stretchedThat
  }

  def plugBeneath(that: Tensor, plugThisOutputsToThatInputs: Int => Int): Tensor = {
    // that o pluggins o this
    require(this.isDiagramShape)
    require(that.isDiagramShape)
    val sizeNeeded = math.max(this.height, that.width)
    val stretchedThis = this.heighten(sizeNeeded)
    val stretchedThat = that.widen(sizeNeeded)
    val sigma = Tensor.swap((math.log(sizeNeeded) / math.log(2)).toInt, plugThisOutputsToThatInputs)
    stretchedThat o sigma o stretchedThis
  }

  def widen(n: Int): Tensor = {
    // multiply by the identity to reach width n
    if (this.width < n) this x Tensor.id(n / this.width) else this
  }

  def heighten(n: Int): Tensor = {
    // multiply by the identity to reach height n
    if (this.height < n) this x Tensor.id(n / this.height) else this
  }

  override def toString: String = {
    val minWidth = 1
    val longestLength = this.contents.flatten.map(s => s.toString.length).foldLeft(minWidth)((a, b) => Math.max(a, b))

    def pad(s: String): String = if (s.length < longestLength) pad(" " + s) else s

    this.contents.map(line => line.map(s => pad(s.toString)).mkString(" ")).mkString("\n")
  }

  def toJson: JsonObject = {
    JsonObject(
      "contents" -> JsonArray(c.map(x => JsonArray(
        x.map(comp => comp.toJson)
      )))
    )
  }

  def toStringSparse: String = {
    // Convert to string but with "." instead of "0" to make non-zero entries stand out
    val minWidth = 1
    val longestLength = this.contents.flatten.map(s => s.toString.length).foldLeft(minWidth)((a, b) => Math.max(a, b))

    def pad(s: String): String = if (s.length < longestLength) pad(s + " ") else s

    def sparse(s: String): String = if (s == "0") "." else s

    this.contents.map(line => line.map(s => pad(sparse(s.toString))).mkString(" ")).mkString("\n")
  }

  def apply(down: Int, across: Int): Complex = {
    // get the (i,j)th entry
    this.entry(down, across)
  }

  def entry(down: Int, across: Int): Complex = contents(down)(across)

  def isRoughlyUpToScalar(that: Tensor, distance: Double = Tensor.defaultDistance): Boolean = {
    //
    this.distanceAfterScaling(that) < distance
  }

  def distanceAfterScaling(that: Tensor): Double = {
    if (this.isSameShapeAs(that)) {
      var maxEntry = (0, 0)
      var maxEntryValue = Complex.zero
      for (i <- this.c.indices; j <- this.c.head.indices) {
        if (this.c(i)(j).abs > maxEntryValue.abs) {
          maxEntry = (i, j)
          maxEntryValue = this.c(i)(j)
        }
      }
      if (maxEntryValue == Complex.zero) {
        this.distance(that)
      } else {
        val sameEntryInThat = that.contents(maxEntry._1)(maxEntry._2)
        if (sameEntryInThat.abs == 0) {
          this.distance(that)
        } else {
          this.distance(that.scaled(maxEntryValue / sameEntryInThat))
        }
      }
    } else {
      1
    }
  }

  def approximates(maxDist: Double): Tensor => Boolean = {
    // uncurried form of isRoughly
    isRoughly(_, maxDist)
  }

  override def equals(other: Any): Boolean =
  // Compares matrix-entry by matrix-entry
    other match {
      case that: Tensor =>
        (that canEqual this) &&
          this.contents.deep == that.contents.deep
      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Tensor]

  def sum(that: Tensor): Tensor = {
    def gen(i: Int, j: Int): Complex = {
      if (i < this.height && j < this.width) {
        this.contents(i)(j)
      } else if (i >= this.height && j >= this.width) {
        that.contents(i - this.height)(j - this.width)
      } else {
        0
      }
    }

    Tensor(this.height + that.height, this.width + that.width, gen)
  }

  def power(index: Int): Tensor = {
    require(index >= 0)
    index match {
      case 0 => Tensor.id(1)
      case 1 => new Tensor(this.c)
      case n => this x this.power(n - 1)
    }
  }

  def sumPower(index: Int): Tensor = {
    require(index >= 0)
    index match {
      case 0 => Tensor.id(1)
      case 1 => new Tensor(this.c)
      case n => this sum this.sumPower(n - 1)
    }
  }

  private def normalise(): Tensor = {
    // scale so the largest entry is 1, unless the tensor is roughly 0
    val sameSizeZero = Tensor.zero(this.height, this.width)
    if (this.isRoughly(sameSizeZero)) sameSizeZero else {
      val maxAbsEntry = this.contents.flatten.foldLeft(Complex.zero)((a, b) => if (b.abs > a.abs) b else a)
      this.scaled(maxAbsEntry.inverse())
    }
  }

  def isRoughly(that: Tensor, maxDistance: Double = 1e-14): Boolean =
  // Compare two tensors up to a given distance
  if(this.isSameShapeAs(that)) {
    this.distance(that) < maxDistance
  }else{
    false
  }

  /** Returns max abs distance */
  def distance(that: Tensor): Double = {
    require(this.isSameShapeAs(that))
    (this - that).contents.flatten.foldLeft(0.0) { (a: Double, b: Complex) => math.max(a, b.abs) }
  }

  def isSameShapeAs(that: Tensor): Boolean = {
    this.width == that.width && this.height == that.height
  }

  def -(that: Tensor): Tensor = {
    // this - that
    this + that.scaled(Complex(-1, 0))
  }

  def +(that: Tensor): Tensor = {
    // this + that
    require(this.width == that.width)
    require(this.height == that.height)
    Tensor(this.height, this.width, (i, j) => this.c(i)(j) + that.contents(i)(j))
  }

  def scaled(factor: Complex): Tensor = {
    // scalar multiplication
    Tensor(this.height, this.width, (i, j) => this.c(i)(j) * factor)
  }
}

object Tensor {
  type Matrix = Array[Array[Complex]]

  type Generator = (Int, Int) => Complex

  val hadamard: Tensor = {
    // Hadamard with 1 input and 1 output
    Tensor(Array(Array(1, 1), Array(1, -1))).scaled(math.pow(2, -0.5))
  }
  val defaultDistance = 1e-14

  def idWires(n: Int): Tensor = {
    // Identity on n wires, i.e. 2^n * 2^n matrix
    id(math.pow(2, n).toInt)
  }

  def id(n: Int): Tensor = {
    // Identity as n*n matrix
    Tensor(n, n, (a: Int, b: Int) => if (a == b) new Complex(1) else new Complex(0))
  }

  def apply(c: Array[Array[Complex]]) = new Tensor(c)

  def apply(cInt: Array[Array[Int]]): Tensor = {
    // Convert integer matrix to complex matrix
    Tensor(cInt.length, cInt(0).length, (i, j) => Complex.doubleToComplex(cInt(i)(j)))
  }

  def apply(height: Int, width: Int, generator: Tensor.Generator): Tensor = {
    // create tensor based on size and generating function
    new Tensor(generatorToMatrix(height, width, generator))
  }

  def generatorToMatrix(height: Int, width: Int, generator: Tensor.Generator): Matrix = {
    // Create matrix of given size using the generator function
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
    // Fill matrix with zeroes
    (for (_ <- 0 until height) yield
      (for (_ <- 0 until width) yield Complex.zero).toArray).toArray
  }

  def permutation(asList: List[Int]): Tensor = {
    // Produce the matrix that sends i -> asList(i)
    val gen = (x: Int) => asList(x)
    new Tensor(permutationMatrix(asList.length, gen))
  }

  private var permutationCache : Map[List[Int], Matrix] = Map()

  def permutationMatrix(size: Int, gen: Int => Int): Matrix = {
    val genAsList : List[Int] = (0 until size).map(gen(_)).toList
    if(permutationCache.contains(genAsList)){
      permutationCache(genAsList)
    }else{
      val base = emptyMatrix(size, size)
      for (i <- 0 until size) {
        base(genAsList(i))(i) = Complex.one
      }
      permutationCache += genAsList -> base
      base
    }
  }

  def permutation(asArray: Array[Int]): Tensor = {
    // Produce the matrix that sends i -> asArray(i)
    val gen = (x: Int) => asArray(x)
    new Tensor(permutationMatrix(asArray.length, gen))
  }

  def swap(asList: List[Int]): Tensor = {
    // Produce the matrix that sends WIRE i to WIRE asList(i)
    val gen = (x: Int) => asList(x)
    swap(asList.length, gen)
  }

  private var swapCache : Map[List[Int], Tensor] = Map()

  def swap(size: Int, gen: Int => Int): Tensor = {
    // Produce the matrix that sends WIRE i to WIRE gen(i)
    val genAsList : List[Int] = (0 until size).map(gen).toList

    if(swapCache.contains(genAsList)){
      swapCache(genAsList)
    }else{

      def padLeft(s: String, n: Int): String = if (s.length < n) padLeft("0" + s, n) else s

      def permGen(i: Int): Int = {
        val binaryStringIn = padLeft((i: RichInt).toBinaryString, size)
        val permedString = (for (j <- 0 until size) yield binaryStringIn(gen(j))).mkString("")
        permedString match {
          case "" => 0
          case s => Integer.parseInt(s, 2)
        }

      }
      val answer = permutation(math.pow(2, size).toInt, permGen)
      swapCache += genAsList -> answer
      answer
    }

  }

  def permutation(size: Int, gen: Int => Int): Tensor = {
    // Produce the matrix that sends i -> gen(i)
    new Tensor(permutationMatrix(size, gen))
  }

  def diagonal(entries: Array[Complex]): Tensor = {
    val size = entries.length
    val generator: Generator = (i: Int, j: Int) => {
      if (i == j) {
        entries(i)
      } else 0
    }
    Tensor(height = size, width = size, generator)
  }

  def zero(height: Int, width: Int): Tensor =
  // Create the zero tensor
    Tensor(height, width, (_, _) => Complex.zero)

  // for comparing entries

  implicit def fromJson(json: JsonObject): Tensor = {
    val contents: Array[Array[Complex]] = (json / "contents").asArray.map(
      x => x.asArray.map(i => Complex.fromJson(i.asObject)).toArray
    ).toArray
    new Tensor(contents)
  }
}
