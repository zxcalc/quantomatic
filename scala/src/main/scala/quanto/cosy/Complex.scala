package quanto.cosy

import quanto.util.json.JsonObject

/* Complex numbers for use in Tensor
 *
 */

class Complex(
               val re: Double,
               val im: Double) {
  // Stores complex numbers as pairs of Doubles
  override def toString: String =
    toStringConcise

  def toStringConcise: String = {
    // Convert to concise expression, e.g. -2i not 0-2i
    def simpleNum(x: Double): String = {
      if (x == 0) ""
      else if (x == x.floor) x.toInt.toString
      else x.toString
    }


    val realPart: String = simpleNum(re)


    val imPart: String = {
      simpleNum(im) match {
        case "" => ""
        case "1" => "i"
        case "-1" => "-i"
        case s => s + "i"
      }
    }

    if (realPart != "" && imPart != "") {
      realPart + (if (im >= 0) "+" else "") + imPart
    }
    else {
      if (realPart != "") realPart else if (imPart != "") imPart else "0"
    }
  }


  def this(re: Double) = this(re, 0)

  def /(that: Complex): Complex = this * that.inverse

  def inverse(): Complex = (Complex(re, 0) - Complex(0, im)) * (1 / (re * re + im * im))

  def -(that: Complex): Complex =
    this + that.negate

  def +(that: Complex): Complex =
    new Complex(re + that.re, im + that.im)

  def negate(): Complex = new Complex(-this.re, -this.im)

  def *(that: Complex) =
    new Complex(re * that.re - im * that.im, re * that.im + im * that.re)

  def conjugate =
    new Complex(re, -im)

  def abs: Double = math.pow(re * re + im * im, 0.5)

  override def equals(other: Any): Boolean =
    other match {
      case that: Complex =>
        (that canEqual this) &&
          this.re == that.re &&
          this.im == that.im
      case that: Double =>
        this.re == that
      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Complex]

  override def hashCode(): Int = ((41 * this.im) + this.re).toInt

  def toJson : JsonObject = {
    JsonObject(
      "real" -> re,
      "imaginary" -> im
    )
  }
}

object Complex {
  val one = new Complex(1, 0)
  val zero = new Complex(0, 0)
  val i = new Complex(0, 1)

  def apply(r: Double, i: Double) = new Complex(r, i)

  implicit def intToCOmplex(x: Int): Complex = new Complex(x)

  implicit def doubleToComplex(x: Double): Complex = new Complex(x)

  def apply(r: Int, i: Int) = new Complex(r, i)

  def fromJson(json : JsonObject) : Complex = {
    new Complex((json / "real").doubleValue,
      (json / "imaginary").doubleValue)
  }
}
