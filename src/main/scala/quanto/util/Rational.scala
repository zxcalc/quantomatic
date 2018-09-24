package quanto.util

class RationalDivideByZeroException(r: Rational)
  extends Exception("Attempted to divide by 0 in (" + r.n + "/" + r.d + ")")

class Rational(numerator: Int, denominator: Int) extends Ordered[Rational] {

  private val r = Rational.gcd(numerator, denominator)
  private val dsn = if (denominator < 0) -1 else 1

  if (denominator == 0) throw new RationalDivideByZeroException(this)
  val n: Int = dsn * numerator / r
  val d: Int = dsn * denominator / r

  def +(r: Rational) = Rational(n * r.d + r.n * d, d * r.d)

  def -(r: Rational) = Rational(n * r.d - r.n * d, d * r.d)

  def *(r: Rational) = Rational(n * r.n, d * r.d)

  def *(i: Int) = Rational(n * i, d)

  def /(r: Rational) = Rational(n * r.d, d * r.n)

  def mod(i: Int): Rational =
    if (n < 0) Rational((n % (d * i)) + (d * i), d)
    else Rational(n % (d * i), d)

  def inv = Rational(d, n)

  override def equals(r: Any): Boolean = r match {
    case r1: Rational => n == r1.n && d == r1.d
    case _ => false
  }

  override def compare(r: Rational): Int = {
    n * r.d - r.n * d
  }

  def isZero: Boolean = n == 0

  def isOne: Boolean = n == 1 && d == 1

  override def toString: String = if (d == 1) n.toString else "(" + n + "/" + d + ")"

}

object Rational {
  def apply(numerator: Int, denominator: Int) = new Rational(numerator, denominator)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) Math.abs(a) else gcd(b, a % b)
  }

  def apply(numerator: Int) = new Rational(numerator, 1)



  implicit def intToRational(i: Int): Rational = Rational(i)

  implicit def rationalToDouble(r: Rational): Double = r.n.toFloat / r.d.toFloat
}
