package quanto.data

// ported from linrat_angle_expr.ML

import quanto.util.Rational

import scala.util.parsing.combinator._

class BooleanParseException(message: String)
  extends Exception(message)

class BooleanEvaluationException(message: String)
  extends Exception(message)

class BooleanExpression(val const : Rational, val coeffs : Map[String,Rational]) {
  lazy val vars = coeffs.keySet

  def *(r : Rational): BooleanExpression =
    BooleanExpression(const * r, coeffs.mapValues(x => x * r))

  def *(i : Int): BooleanExpression = this * Rational(i)

  def +(e : BooleanExpression) = BooleanExpression(const + e.const, e.coeffs.foldLeft(coeffs) {
    case (m, (k,v)) => m + (k -> (v + m.getOrElse(k, Rational(0))))
  })

  def -(e: BooleanExpression) : BooleanExpression = this + (e * -1)

  def subst(v : String, e : BooleanExpression) : BooleanExpression = {
    val c = coeffs.getOrElse(v,Rational(0))
    this - BooleanExpression(Rational(0), Map(v -> c)) + (e * c)
  }

  def subst(mp : Map[String, BooleanExpression]): BooleanExpression =
    mp.foldLeft(this) { case (e, (v,e1)) => e.subst(v,e1) }

  def evaluate(mp: Map[String, Double]) : Double = {
    try {
      const + coeffs.foldLeft(0.0) { (a, b) => a + (mp(b._1) * Rational.rationalToDouble(b._2)) }
    } catch {
      case e : Exception => new BooleanEvaluationException("Given arguments do not match those in the coefficient list")
        0
    }
  }

  override def equals(that: Any): Boolean = that match {
    case e : BooleanExpression =>
      const == e.const && coeffs == e.coeffs
    case _ => false
  }

  override def toString: String = {
    var fst = true
    var s = ""
    if (!const.isZero) {

      fst = false
      if (const.isOne) s += "\\pi"
      else {
        val (n,sgn) =
          if (const.n > const.d && const.n < 2 * const.d) (2 * const.d - const.n, "-")
          else (const.n, "")
        if (n == 1) s += sgn + "\\pi/" + const.d
        else s += sgn + n.toString + "\\pi/" + const.d
      }
    }

    coeffs.foreach { case (x,c) =>
      if (fst) {
        fst = false
        s = s + (if (c == Rational(1)) "" else c.toString + " ") + x
      } else {
        if (c < Rational(0)) {
          s = s + " - " + (if (c == Rational(-1)) "" else (c * -1).toString + " ") + x
        } else {
          s = s + " + " + (if (c == Rational(1)) "" else c.toString + " ") + x
        }
      }
    }

    s
  }


}

object BooleanExpression {
  def apply(const : Rational = Rational(0),
            coeffs : Map[String,Rational] = Map()) =
    new BooleanExpression(const mod 2, coeffs.filter { case (_,c) => !c.isZero })

  val ZERO = BooleanExpression(Rational(0))
  val ONE_PI = BooleanExpression(Rational(1))

  def parse(s : String) = BooleanExpressionParser.p(s)

  private object BooleanExpressionParser extends RegexParsers {
    override def skipWhitespace = true
    def INT: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
    def INT_OPT : Parser[Int] = INT.? ^^ { _.getOrElse(1) }
    def IDENT : Parser[String] = """[\\a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { _.toString }
    def PI : Parser[Unit] = """\\?[pP][iI]""".r ^^ { _ => Unit }


    def coeff : Parser[Rational] =
      INT ~ "/" ~ INT ^^ { case n ~ _ ~ d => Rational(n,d) } |
      "(" ~ coeff ~ ")" ^^ { case _ ~ c ~ _ => c } |
      INT ^^ { n => Rational(n) }


    def frac : Parser[BooleanExpression] =
      INT_OPT ~ "*".? ~ PI ~ "/" ~ INT ^^ { case n ~ _ ~ _ ~ _ ~ d => BooleanExpression(Rational(n,d)) } |
      INT_OPT ~ "*".? ~ IDENT ~ "/" ~ INT ^^ {
        case n ~ _ ~ x ~ _ ~ d => BooleanExpression(Rational(0), Map(x -> Rational(n,d)))
      }

    def term : Parser[BooleanExpression] =
      frac |
      "-" ~ term ^^ { case _ ~ t => t * -1 } |
      coeff ~ "*".? ~ PI ^^ { case c ~ _ ~ _ => BooleanExpression(c) } |
      PI ^^ { _ => ONE_PI } |
      coeff ~ "*".? ~ IDENT ^^ { case c ~ _ ~ x => BooleanExpression(Rational(0), Map(x -> c)) } |
      IDENT ^^ { case x => BooleanExpression(Rational(0), Map(x -> Rational(1))) } |
      coeff ^^ { BooleanExpression(_) } |
      "(" ~ expr ~ ")" ^^ { case _ ~ t ~ _ => t }

    def term1 : Parser[BooleanExpression] =
      "+" ~ term ^^ { case _ ~ t => t } |
      "-" ~ term ^^ { case _ ~ t => t * -1 }

    def terms : Parser[BooleanExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
      term1

    def expr : Parser[BooleanExpression] =
      term ~ terms ^^ { case s ~ t => s + t } |
      term |
      "" ^^ { _ => ZERO }

    def p(s : String) = parseAll(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw new BooleanParseException(msg)
      case Error(msg, _) => throw new BooleanParseException(msg)
    }
  }
}
