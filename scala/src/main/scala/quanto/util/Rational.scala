package quanto.util

class RationalDivideByZeroException(r: Rational)
  extends Exception("Attempted to divide by 0 in (" + r.n + "/" + r.d + ")")

class Rational(numerator : Int, denominator : Int) extends Ordered[Rational] {
  if (denominator == 0) throw new RationalDivideByZeroException(this)
  private val r = Rational.gcd(numerator,denominator)
  private val dsn = if (denominator < 0) -1 else 1

  val n = dsn * numerator/r
  val d = dsn * denominator/r

  def +(r : Rational) = Rational(n * r.d + r.n * d, d * r.d)
  def -(r : Rational) = Rational(n * r.d - r.n * d, d * r.d)
  def *(r : Rational) = Rational(n * r.n, d * r.d)
  def *(i : Int) = Rational(n * i, d)
  def /(r : Rational) = Rational(n * r.d, d * r.n)
  def mod(i : Int) = Rational(n % (d * i), d)
  def inv = Rational(d,n)
  override def equals(r : Any) = r match {
    case r1 : Rational => n == r1.n && d == r1.d
    case _ => false
  }
  override def compare(r : Rational) = { n * r.d - r.n * d }
  def isZero = n == 0

  override def toString = if (d == 1) n.toString else "(" + n + "/" + d + ")"

}

object Rational {
  def apply(numerator: Int, denominator: Int) = new Rational(numerator, denominator)
  def apply(numerator: Int) = new Rational(numerator, 1)

  private def gcd(a: Int,b: Int): Int = {
    if(b == 0) Math.abs(a) else gcd(b, a%b)
  }

  implicit def intToRational(i : Int) : Rational = Rational(i)
}
