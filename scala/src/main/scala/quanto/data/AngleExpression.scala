package quanto.data

// ported from linrat_angle_expr.ML

import scala.util.parsing.combinator._

class AngleParseException(message: String)
  extends Exception(message)

class AngleExpression(val const : Rational, val coeffs : Map[String,Rational]) {

  def *(r : Rational): AngleExpression =
    AngleExpression(const * r, coeffs.mapValues(x => x * r))

  def +(e : AngleExpression) = AngleExpression(const + e.const, e.coeffs.foldLeft(coeffs) {
    case (m, (k,v)) => m + (k -> (v + m.getOrElse(k, Rational(0))))
  })

  override def toString = {
    var fst = true
    var s = ""
    if (const != Rational(0)) {
      fst = false
      s += const.toString + " \\pi"
    }

    coeffs.foreach { case (x,c) =>
      if (fst) {
        fst = false
        s = s + c.toString + " " + x
      } else {
        if (c < Rational(0)) {
          s = s + " - " + (c * -1).toString + " " + x
        } else {
          s = s + " + " + c.toString + " " + x
        }
      }
    }

    s
  }


}

object AngleExpression {
  def apply(const : Rational, coeffs : Map[String,Rational] = Map()) =
    new AngleExpression(const, coeffs.filter { case (_,c) => !c.isZero })

  val ONE_PI = AngleExpression(Rational(1))

  def parse(s : String) = AngleExpressionParser.p(s)

  private object AngleExpressionParser extends RegexParsers {
    override def skipWhitespace = true
    def INT: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
    def INT_OPT : Parser[Int] = INT.? ^^ { _.getOrElse(1) }
    def IDENT : Parser[String] = """[\\a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { _.toString }
    def PI : Parser[Unit] = """\\?[pP][iI]""".r ^^ { _ => Unit }
    def SYM(s : String) : Parser[Unit] = s ^^ { _ => Unit }

    def coeff : Parser[Rational] =
      INT ~ SYM("/") ~ INT ^^ { case n ~ _ ~ d => Rational(n,d) } |
        SYM("(") ~ coeff ~ SYM(")") ^^ { case _ ~ c ~ _ => c } |
        INT ^^ { n => Rational(n) }


    def frac : Parser[AngleExpression] =
      INT_OPT ~ SYM("*").? ~ PI ~ SYM("/") ~ INT ^^ { case n ~ _ ~ _ ~ _ ~ d => AngleExpression(Rational(n,d)) } |
        INT_OPT ~ SYM("*").? ~ IDENT ~ SYM("/") ~ INT ^^ {
          case n ~ _ ~ x ~ _ ~ d => AngleExpression(Rational(0), Map(x -> Rational(n,d)))
        }

    def term : Parser[AngleExpression] =
      frac |
        SYM("-") ~ term ^^ { case _ ~ t => t * Rational(-1) } |
        coeff ~ SYM("*").? ~ PI ^^ { case c ~ _ ~ _ => AngleExpression(c) } |
        PI ^^ { _ => AngleExpression.ONE_PI } |
        coeff ~ SYM("*").? ~ IDENT ^^ { case c ~ _ ~ x => AngleExpression(Rational(0), Map(x -> c)) } |
        IDENT ^^ { case x => AngleExpression(Rational(0), Map(x -> Rational(1))) } |
        SYM("(") ~ term ~ SYM(")") ^^ { case _ ~ t ~ _ => t }

    def term1 : Parser[AngleExpression] =
      SYM("+") ~ term ^^ { case _ ~ t => t } |
      SYM("-") ~ term ^^ { case _ ~ t => t * Rational(-1) }

    def terms : Parser[AngleExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
      term1

    def expr : Parser[AngleExpression] = term ~ terms ^^ { case s ~ t => s + t } | term

    def p(s : String) = parse(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw new AngleParseException(msg)
      case Error(msg, _) => throw new AngleParseException(msg)
    }
  }
}
