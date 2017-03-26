package quanto.data

class Rational(numerator : Int, denominator : Int) extends Ordered[Rational] {
  def gcd(a: Int,b: Int): Int = {
    if(b == 0) Math.abs(a) else gcd(b, a%b)
  }
  private val r = gcd(numerator,denominator)
  private val dsn = if (denominator < 0) -1 else 1

  val n = dsn * numerator/r
  val d = dsn * denominator/r

  def +(r : Rational) = Rational(n * r.d + r.n * d, d * r.d)
  def -(r : Rational) = Rational(n * r.d - r.n * d, d * r.d)
  def *(r : Rational) = Rational(n * r.n, d * r.d)
  def *(i : Int) = Rational(n * i, d)
  def equals(r : Rational) = { n == r.n && d == r.d }
  def compare(r : Rational) = { n * r.d - r.n * d }
  def isZero = n == 0

  override def toString = if (d == 1) n.toString else "(" + n + "/" + d + ")"

}

object Rational {
  def apply(numerator: Int, denominator: Int) = new Rational(numerator, denominator)
  def apply(numerator: Int) = new Rational(numerator, 1)
}
