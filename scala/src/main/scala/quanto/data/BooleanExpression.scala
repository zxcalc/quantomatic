package quanto.data

// ported from linrat_angle_expr.ML

import quanto.util.Rational

import scala.util.parsing.combinator._

class BooleanParseException(message: String)
  extends Exception(message)

class BooleanEvaluationException(message: String)
  extends Exception(message)

class BooleanExpression(val const: Int, val symbols: Set[String]) {
  lazy val vars = symbols

  def subst(mp: Map[String, BooleanExpression]): BooleanExpression =
    mp.foldLeft(this) { case (e, (v, e1)) => e.subst(v, e1) }

  def subst(v: String, e: BooleanExpression): BooleanExpression = {
    val c: Int = if (symbols.contains(v)) 1 else 0
    this - BooleanExpression(0, Set(v)) + (e * c)
  }

  def -(e: BooleanExpression): BooleanExpression = this + e

  // Disjoint union of variables
  def +(e: BooleanExpression) =
    BooleanExpression(const + e.const, e.symbols.diff(symbols).union(symbols.diff(e.symbols)))

  def *(i: Int): BooleanExpression = if (i == 0) BooleanExpression(0) else this

  def evaluate(mp: Map[String, Int]): Int = {
    try {
      (const + symbols.foldLeft(0) { (a, b) => a + mp(b) }) % 2
    } catch {
      case e: Exception => new BooleanEvaluationException("Given arguments do not match those in the coefficient list")
        0
    }
  }

  override def equals(that: Any): Boolean = that match {
    case e: BooleanExpression =>
      const == e.const && symbols == e.symbols
    case _ => false
  }

  override def toString: String = {
    val t = "\\True"
    val f = "\\False"
    if (symbols.nonEmpty){
      symbols.mkString(if(const == 1){s"$t + "}else{""}," + ","")
    } else {
      if(const==1) t else f
    }
  }


}

object BooleanExpression {
  val BOOL_TRUE = BooleanExpression(1)
  val BOOL_FALSE = BooleanExpression(0)

  def apply(const: Int = 0,
            coeffs: Set[String] = Set()) =
    new BooleanExpression(const % 2, coeffs)

  def parse(s: String) = BooleanExpressionParser.p(s)

  private object BooleanExpressionParser extends RegexParsers {
    override def skipWhitespace = true

    def INT_OPT: Parser[Int] = INT.? ^^ {
      _.getOrElse(0)
    } // If you can't understand it, it's false

    def INT: Parser[Int] =
      """[0-9]+""".r ^^ {
        _.toInt
      }

    def SYMBOL: Parser[String] =
      """[\\a-zA-Z_][a-zA-Z0-9_]*""".r ^^ {
        _.toString
      }

    def TRUE: Parser[Int] = """\\?[Tt](rue|RUE)?""".r ^^ { _ => 1 }

    def FALSE: Parser[Int] = """\\?[Ff](alse|ALSE)?""".r ^^ { _ => 0 }


    def coeff: Parser[Int] =
      TRUE | FALSE | INT


    def term: Parser[BooleanExpression] =
      "-" ~ term ^^ { case _ ~ t => t } |
        TRUE ^^ { _ => BOOL_TRUE } |
        FALSE ^^ { _ => BOOL_FALSE } |
        coeff ~ "*" ~ coeff ^^ { case c ~ _ ~ x => BooleanExpression(x * c) } |
        coeff ~ "*".? ~ SYMBOL ^^ { case c ~ _ ~ x => BooleanExpression(0, Set(x)) * c } |
        SYMBOL ^^ { x => BooleanExpression(0, Set(x)) } |
        coeff ^^ { x => BooleanExpression(x) } |
        "(" ~ expr ~ ")" ^^ { case _ ~ t ~ _ => t }

    def term1: Parser[BooleanExpression] =
      "+" ~ term ^^ { case _ ~ t => t } |
        "-" ~ term ^^ { case _ ~ t => t * -1 }

    def terms: Parser[BooleanExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
        term1

    def expr: Parser[BooleanExpression] =
      term ~ terms ^^ { case s ~ t => s + t } |
        term |
        terms |
        "" ^^ { _ => BOOL_FALSE }

    def p(s: String) = parseAll(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw new BooleanParseException(msg)
      case Error(msg, _) => throw new BooleanParseException(msg)
    }
  }

}
