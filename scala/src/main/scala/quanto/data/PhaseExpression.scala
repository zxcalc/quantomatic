package quanto.data

import quanto.data.Theory.ValueType
import quanto.util.Rational

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

// Phases are just groups with added symbolic variables,
// Except we also need to be able to do Gaussian elimination on them
// So need to be able to interpret integers inside the phase group
// (i.e. needs to know what '1' is)
// We use the language of Abelian groups
// For things like Strings you just need to fudge it to allow people to put notes into nodes


case class PhaseParseException(msg: String, valueType: ValueType)
  extends Exception(s"Attempting parse $ValueType threw message $msg")

case class PhaseEvaluationException(msg: String) extends Exception(msg)


abstract class RationalWithSymbols[Group <: RationalWithSymbols[Group]](val coefficients: Map[String, Rational], val constant: Rational) {

  val description: ValueType
  // Any symbolic variables should be in a set,
  // However since we need these things to give consistent string outputs
  // we sort the list here, and convert back into a set if needed
  // to avoid "x + y" != "y + x"
  val vars: List[String] = coefficients.keySet.toList.sorted

  // The companion object should hold the zero element, but link to it from the class
  def zero: Group

  // Needs a 1, also held on the companion object
  def one: Group

  // Addition
  def +(g: Group): Group

  // Subtraction
  def -(g: Group): Group

  // Multiplication - only needs to be able to do multiplication by possible variable coefficients
  def *(r: Rational): Group

  // Substitution of single variable
  def subst(s: String, e: Group): Group

  // Substitution of map of variables
  def subst(mp: Map[String, Group]): Group

  def evaluate(mp: Map[String, Double]): Double = {
    try {
      constant + coefficients.foldLeft(0.0) { (a, b) => a + (mp(b._1) * Rational.rationalToDouble(b._2)) }
    } catch {
      case _: Exception => PhaseEvaluationException("Given arguments do not match those in the coefficient list")
        0
    }
  }

}

class PhaseExpression(const: Rational,
                      coeff: Map[String, Rational],
                      val modulus: Option[Int],
                      val finiteField: Boolean,
                      override val description: ValueType)
  extends RationalWithSymbols[PhaseExpression](coeff, const) {

  // Apply the modulus at creation
  override val constant: Rational = mod(const)
  // filter out any variables with zero as their coefficient
  // If restricting to integers then we also apply the modulo to the coefficients
  // e.g. 2*alpha = false in Bool (yes, that's a horrible mix, but it's the sort of thing we expect to see parsed)
  override val coefficients: Map[String, Rational] = coeff.map(
    sr => sr._1 -> (if (finiteField) mod(sr._2) else sr._2)
  ).filterNot(sr => sr._2.isZero)

  def mod(r: Rational): Rational = if (modulus.nonEmpty) {
    val reduced = Rational(r.n % (modulus.get * r.d), r.d)
    // Recall that -1 % 2 is still -1, for reasons.
    if (reduced < 0) {
      reduced + modulus.get
    } else {
      reduced
    }
  } else {
    r
  }

  override def equals(that: Any): Boolean = that match {
    case e: PhaseExpression =>
      description == e.description && constant == e.constant && coefficients == e.coefficients
    case _ => false
  }

  def zero: PhaseExpression = PhaseExpression.zero(description)

  def one: PhaseExpression = PhaseExpression.one(description)

  def subst(mp: Map[String, PhaseExpression]): PhaseExpression =
    mp.foldLeft(this) { case (e, (v, e1)) => e.subst(v, e1) }

  def subst(v: String, e: PhaseExpression): PhaseExpression = {
    val c = coefficients.getOrElse(v, Rational(0))
    this - PhaseExpression(Rational(0), Map(v -> c), description) + (e * c)
  }

  def -(e: PhaseExpression): PhaseExpression = this + (e * -1)

  def *(i: Int): PhaseExpression = this * Rational(i)

  def +(e: PhaseExpression) = PhaseExpression(mod(constant + e.constant),
    e.coefficients.foldLeft(coefficients) {
      case (m, (k, v)) => m + (k -> (v + m.getOrElse(k, Rational(0))))
    }, description)

  def *(r: Rational): PhaseExpression =
    PhaseExpression(mod(constant * r), coefficients.mapValues(x => x * r), description)

  override def toString: String = PhaseExpression.toString(description, this)

  override def evaluate(mp: Map[String, Double]): Double = mod(super.evaluate(mp))

  def mod(d: Double): Double = if (modulus.nonEmpty) {
    d % modulus.get
  } else {
    d
  }

  def as(valueType: ValueType): PhaseExpression = PhaseExpression(constant, coefficients, valueType)

  def convertTo(valueType: ValueType): PhaseExpression = PhaseExpression(this.constant, this.coefficients, valueType)
}

case class FieldData(modulus: Option[Int], finiteField: Boolean)


object PhaseExpression {

  def apply(r: Rational, valueType: ValueType): PhaseExpression = {
    PhaseExpression(r, Map(), valueType)
  }

  def parse(s: String, valueType: ValueType): PhaseExpression = {
    valueType match {
      case ValueType.AngleExpr => AngleExpressionParser.p(s)
      case ValueType.Boolean => BooleanExpressionParser.p(s)
      case ValueType.Rational => RationalExpressionParser.p(s)
      case ValueType.Empty => PhaseExpression(0, Map(), ValueType.Empty)
      case ValueType.String => StringExpressionParser.p(s)
      case v => throw PhaseParseException(s"Asked to parse unexpected '$s' for type $v", v)
    }
  }

  def one(valueType: ValueType): PhaseExpression = PhaseExpression(1, Map(), valueType)

  def apply(r: Rational, m: Map[String, Rational], valueType: ValueType): PhaseExpression = {
    val fData = fieldData(valueType)
    new PhaseExpression(r, m, fData.modulus, fData.finiteField, valueType)
  }

  def fieldData(valueType: ValueType): FieldData = valueType match {
    case ValueType.AngleExpr => FieldData(Some(2), finiteField = false)
    case ValueType.Boolean => FieldData(Some(2), finiteField = true)
    case _ => FieldData(None, finiteField = false)
  }

  def toString(valueType: ValueType, phaseExpression: PhaseExpression): String = {
    valueType match {
      case ValueType.AngleExpr =>
        writeAsAngle(phaseExpression)
      case ValueType.Boolean =>
        writeAsBoolean(phaseExpression)
      case ValueType.String =>
        writeAsString(phaseExpression)
      case ValueType.Empty =>
        ""
      case ValueType.Rational =>
        writeAsRational(phaseExpression)
    }
  }

  private def writeAsRational(phaseExpression: PhaseExpression): String = {
    val constant = phaseExpression.constant
    val coefficients = phaseExpression.coefficients
    var fst = true
    var s = ""
    if (!constant.isZero) {

      fst = false
      val (n, sgn) =
        if (constant.n > constant.d && constant.n < 2 * constant.d) (2 * constant.d - constant.n, "-")
        else (constant.n, "")
      if (n == 1) s += sgn + "1/" + constant.d
      else s += sgn + n.toString + "/" + constant.d

    }

    def rStr(c: Rational) : String = writeAsRational(PhaseExpression(c, Map(), ValueType.Rational))

    coefficients.keys.toList.sorted.foreach { variableName =>
      val c = coefficients(variableName)
      if (!c.isZero) {
        if (fst) {
          fst = false
          s = s + (if (c == Rational(1)) "" else rStr(c) + " ") + variableName
        } else {
          if (c < Rational(0)) {
            s = s + " - " + (if (c == Rational(-1)) "" else rStr(c * -1) + " ") + variableName
          } else {
            s = s + " + " + (if (c == Rational(1)) "" else rStr(c) + " ") + variableName
          }
        }
      }
    }
    if (phaseExpression == PhaseExpression.zero(ValueType.Rational)) s = "0"

    s
  }

  private def writeAsString(phaseExpression: PhaseExpression): String = {
    val constant = phaseExpression.constant
    val vars = phaseExpression.vars
    if (vars.nonEmpty) {
      vars.mkString("", ", ", "")
    } else {
      ""
    }
  }

  private def writeAsBoolean(phaseExpression: PhaseExpression): String = {
    val constant = phaseExpression.constant
    val vars = phaseExpression.vars
    val t = "\\True"
    val f = "\\False"
    if (vars.nonEmpty) {
      vars.mkString(if (constant == Rational(1)) {
        s"$t + "
      } else {
        ""
      }, " + ", "")
    } else {
      if (constant == Rational(1)) t else f
    }
  }

  private def writeAsAngle(phaseExpression: PhaseExpression): String = {
    val constant = phaseExpression.constant
    val coefficients = phaseExpression.coefficients
    var fst = true
    var s = ""
    if (!constant.isZero) {

      fst = false
      if (constant.isOne) s += "\\pi"
      else {
        val (n, sgn) =
          if (constant.n > constant.d && constant.n < 2 * constant.d) (2 * constant.d - constant.n, "-")
          else (constant.n, "")
        if (n == 1) s += sgn + "\\pi/" + constant.d
        else s += sgn + n.toString + "\\pi/" + constant.d
      }
    }

    coefficients.keys.toList.sorted.foreach { variableName =>
      val c = coefficients(variableName)
      if (!c.isZero) {
        if (fst) {
          fst = false
          s = s + (if (c == Rational(1)) "" else c.toString + " ") + variableName
        } else {
          if (c < Rational(0)) {
            s = s + " - " + (if (c == Rational(-1)) "" else (c * -1).toString + " ") + variableName
          } else {
            s = s + " + " + (if (c == Rational(1)) "" else c.toString + " ") + variableName
          }
        }
      }
    }
    if (phaseExpression == PhaseExpression.zero(ValueType.AngleExpr)) s = "0"

    s
  }

  def zero(valueType: ValueType): PhaseExpression = PhaseExpression(0, Map(), valueType)

  private object AngleExpressionParser extends CommonParser(ValueType.AngleExpr) {

    def angleExpression(r: Rational): PhaseExpression = PhaseExpression(r, ValueType.AngleExpr)

    def angleExpression(r: Rational, m: Map[String, Rational]): PhaseExpression = PhaseExpression(r, m, ValueType.AngleExpr)

    def INT_OPT: Parser[Int] = INT.? ^^ {
      _.getOrElse(1)
    }

    def PI: Parser[Unit] = """\\?[pP][iI]""".r ^^ { _ => Unit }


    def coeff: Parser[Rational] =
      INT ~ "/" ~ INT ^^ { case n ~ _ ~ d => Rational(n, d) } |
        "(" ~ coeff ~ ")" ^^ { case _ ~ c ~ _ => c } |
        INT ^^ { n => Rational(n) }


    def frac: Parser[PhaseExpression] =
      INT_OPT ~ "*".? ~ PI ~ "/" ~ INT ^^ { case n ~ _ ~ _ ~ _ ~ d => angleExpression(Rational(n, d)) } |
        INT_OPT ~ "*".? ~ SYMBOL ~ "/" ~ INT ^^ {
          case n ~ _ ~ x ~ _ ~ d => angleExpression(Rational(0), Map(x -> Rational(n, d)))
        }

    def term: Parser[PhaseExpression] =
      frac |
        "-" ~ term ^^ { case _ ~ t => t * -1 } |
        coeff ~ "*".? ~ PI ^^ { case c ~ _ ~ _ => angleExpression(c) } |
        PI ^^ { _ => one } |
        coeff ~ "*".? ~ SYMBOL ^^ { case c ~ _ ~ x => angleExpression(Rational(0), Map(x -> c)) } |
        SYMBOL ~ "*" ~ coeff ^^ { case x ~ _ ~ c => angleExpression(Rational(0), Map(x -> c)) } |
        SYMBOL ^^ { x => angleExpression(Rational(0), Map(x -> Rational(1))) } |
        coeff ^^ angleExpression |
        "(" ~ expr ~ ")" ^^ { case _ ~ t ~ _ => t }

    def term1: Parser[PhaseExpression] =
      "+" ~ term ^^ { case _ ~ t => t } |
        "-" ~ term ^^ { case _ ~ t => t * -1 }

    def terms: Parser[PhaseExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
        term1

    def expr: Parser[PhaseExpression] =
      term ~ terms ^^ { case s ~ t => s + t } |
        term |
        "" ^^ { _ => zero }

  }


  private object RationalExpressionParser extends CommonParser(ValueType.AngleExpr) {
    def rationalExpression(r: Rational): PhaseExpression = PhaseExpression(r, ValueType.Rational)

    def rationalExpression(r: Rational, m: Map[String, Rational]): PhaseExpression =
      PhaseExpression(r, m, ValueType.Rational)

    def INT_OPT: Parser[Int] = INT.? ^^ {
      _.getOrElse(1)
    }

    def coeff: Parser[Rational] =
      INT ~ "/" ~ INT ^^ { case n ~ _ ~ d => Rational(n, d) } |
        "(" ~ coeff ~ ")" ^^ { case _ ~ c ~ _ => c } |
        INT ^^ { n => Rational(n) }

    def frac: Parser[PhaseExpression] =
        INT_OPT ~ "*".? ~ SYMBOL ~ "/" ~ INT ^^ {
          case n ~ _ ~ x ~ _ ~ d => rationalExpression(Rational(0), Map(x -> Rational(n, d)))
        }

    def term: Parser[PhaseExpression] =
      frac |
        "-" ~ term ^^ { case _ ~ t => t * -1 } |
        coeff ~ "*".? ~ SYMBOL ^^ { case c ~ _ ~ x => rationalExpression(Rational(0), Map(x -> c)) } |
        SYMBOL ^^ { x => rationalExpression(Rational(0), Map(x -> Rational(1))) } |
        SYMBOL ~ "*" ~ coeff ^^ { case x ~ _ ~ c => rationalExpression(Rational(0), Map(x -> c)) } |
        coeff ^^ rationalExpression |
        "(" ~ expr ~ ")" ^^ { case _ ~ t ~ _ => t }

    def term1: Parser[PhaseExpression] =
      "+" ~ term ^^ { case _ ~ t => t } |
        "-" ~ term ^^ { case _ ~ t => t * -1 }

    def terms: Parser[PhaseExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
        term1

    def expr: Parser[PhaseExpression] =
      term ~ terms ^^ { case s ~ t => s + t } |
        term |
        "" ^^ { _ => zero }

  }


  private object StringExpressionParser {
    def zero: PhaseExpression = PhaseExpression.zero(ValueType.String)

    def p(s: String): PhaseExpression = s match {
      case "" => zero
      case t => PhaseExpression(0, Map(t -> Rational(1,1)), ValueType.String)
    }
  }

  private abstract class CommonParser(T: ValueType) extends RegexParsers{

    val zero: PhaseExpression = PhaseExpression.zero(T)
    val one: PhaseExpression = PhaseExpression.one(ValueType.AngleExpr)

    def INT: Parser[Int] =
      """[0-9]+""".r ^^ {
        _.toInt
      }

    def SYMBOL: Parser[String] =
      """[\\a-zA-Z_][a-zA-Z0-9_']*""".r ^^ {
        _.toString
      }

    final override def skipWhitespace = true

    def expr: Parser[PhaseExpression]

    def p(s: String): PhaseExpression = parseAll(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw PhaseParseException(msg, T)
      case Error(msg, _) => throw PhaseParseException(msg, T)
    }
  }

  private object BooleanExpressionParser extends CommonParser(ValueType.Boolean) {

    def BooleanExpression(i: Int): PhaseExpression = PhaseExpression(Rational(i), ValueType.Boolean)

    def BooleanExpression(i: Int, m: Map[String, Rational]): PhaseExpression =
      PhaseExpression(Rational(i), m, ValueType.Boolean)

    def TRUE: Parser[Int] = """\\?[Tt](rue|RUE)?""".r ^^ { _ => 1 }

    def FALSE: Parser[Int] = """\\?[Ff](alse|ALSE)?""".r ^^ { _ => 0 }

    def coeff: Parser[Int] =
      TRUE | FALSE | INT

    def INT_OPT: Parser[Int] = INT.? ^^ {
      _.getOrElse(0)
    }


    def term: Parser[PhaseExpression] =
      "-" ~ term ^^ { case _ ~ t => t } |
        TRUE ^^ { _ => one } |
        FALSE ^^ { _ => zero } |
        coeff ~ "*" ~ coeff ^^ { case c ~ _ ~ x => BooleanExpression(x * c) } |
        coeff ~ "*".? ~ SYMBOL ^^ { case c ~ _ ~ x => BooleanExpression(0, Map(x -> 1)) * c } |
        SYMBOL ^^ { x => BooleanExpression(0, Map(x -> 1)) } |
        coeff ^^ { x => BooleanExpression(x) } |
        "(" ~ expr ~ ")" ^^ { case _ ~ t ~ _ => t }

    def term1: Parser[PhaseExpression] =
      "+" ~ term ^^ { case _ ~ t => t } |
        "-" ~ term ^^ { case _ ~ t => t * -1 }

    def terms: Parser[PhaseExpression] =
      term1 ~ terms ^^ { case s ~ t => s + t } |
        term1

    def expr: Parser[PhaseExpression] =
      term ~ terms ^^ { case s ~ t => s + t } |
        term |
        terms |
        "" ^^ { _ => zero }

  }


}
