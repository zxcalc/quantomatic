package quanto.data

import quanto.data.Theory.ValueType
import quanto.util.Rational

import scala.util.parsing.combinator.RegexParsers


case class MismatchedPhaseData(data1: Vector[ValueType], data2: Vector[ValueType])
  extends Exception(data1.toString + " != " + data2.toString)

case class GenericParseException(message: String) extends Exception(message)

case class TypeNotFoundException(message: String) extends Exception(message)


// The types are stored as a vector of enums
// The data is stored as a vector of PhaseExpressions, which are then cast according to the type data
case class CompositeExpression(valueTypes: Vector[ValueType], values: Vector[PhaseExpression]) {

  lazy val varsWithType: Set[(ValueType, String)] =
    values.zipWithIndex.flatMap(valueWithIndex =>
      valueWithIndex._1.vars.map(variableName =>
        (valueTypes(valueWithIndex._2), variableName))
    ).toSet
  val vars: Set[String] = values.flatMap(_.vars).toSet
  val description: ValueType = ValueType.Empty

  // Addition
  def +(e: CompositeExpression): CompositeExpression = {

    if (valueTypes != e.valueTypes) {
      throw MismatchedPhaseData(valueTypes, e.valueTypes)
    }


    val summedValues: Vector[PhaseExpression] =
      valueTypes.zipWithIndex.map(vi => values(vi._2) + e.values(vi._2))
    CompositeExpression(valueTypes, summedValues)
  }

  // Subtraction
  def -(e: CompositeExpression): CompositeExpression = {


    val negatedValues: Vector[PhaseExpression] =
      valueTypes.zipWithIndex.map(vi => values(vi._2) - e.values(vi._2))
    CompositeExpression(valueTypes, negatedValues)
  }

  // Combine strings of each subvalue
  override def toString: String = {

    if (values.forall(e => e == PhaseExpression.zero(e.description))) {
      ""
    } else {
      val stringValues = values.zipWithIndex.map(pi => (pi._1.description, pi._2) match {
        case (ValueType.String, _) =>
          pi._1.toString // Always render strings directly
        case (_, 0) => // Always render the first entry directly
          pi._1.toString
        case (_, _) => // Put a space before anything else to aid legibiliy
          " " + pi._1.toString
      })
      stringValues.mkString(",")
    }
  }

  def *(r: Rational): CompositeExpression = {

    val scaledValues: Vector[PhaseExpression] = values.map(v => v * r)
    CompositeExpression(valueTypes, scaledValues)
  }

  def firstOrError[T <: PhaseExpression](valueType: ValueType): T = {
    val typeIndex = valueTypes.zipWithIndex.find(x => x._1 == valueType)
    if (typeIndex.nonEmpty) {
      values(typeIndex.get._2).asInstanceOf[T]
    } else {
      throw TypeNotFoundException(valueType.toString + " was not present in " + valueTypes.mkString(","))
    }
  }

  def first[T <: PhaseExpression](valueType: ValueType): Option[T] = {
    try {
      Some(this.firstOrError(valueType))
    }
    catch {
      case _: Throwable => None
    }
  }

  def substSubValues(mp: Map[String, PhaseExpression]): CompositeExpression =
    mp.foldLeft(this) { case (e, (v, e1)) => e.substSubValue(v, e1) }

  def substSubVariables(mp: Map[(ValueType, String), String]): CompositeExpression = {
    substSubValues(mp.map(vss => vss._1._2 -> PhaseExpression.parse(vss._2, vss._1._1) ))
  }

  def substSubValue(variableName: String, phase: PhaseExpression): CompositeExpression = {
    // Apply substitution to subvalues with the correct valueType

    val newValues: Vector[PhaseExpression] =
      valueTypes.zipWithIndex.map(vi => {
        val current = values(vi._2)
        if (phase.description == current.description) {
          current.subst(variableName, phase)
        }
        else {
          current
        }
      })
    CompositeExpression(valueTypes, newValues)
  }

}

object CompositeExpression {

  implicit val modulus: Option[Int] = None

  def empty: CompositeExpression = {
    CompositeExpression(Vector(), Vector())
  }

  def zero(valueTypes: Vector[ValueType]): CompositeExpression = {
    val zeroValues: Vector[PhaseExpression] = valueTypes.map(t => PhaseExpression.zero(t))
    CompositeExpression(valueTypes, zeroValues)
  }

  def one(valueTypes: Vector[ValueType]): CompositeExpression = {
    val oneValues: Vector[PhaseExpression] = valueTypes.map(t => PhaseExpression.one(t))
    CompositeExpression(valueTypes, oneValues)
  }

  def parse(types: String, values: String): CompositeExpression = {
    val typeVector = parseTypes(types)
    CompositeExpression(typeVector, parseKnowingTypes(values, typeVector))
  }

  def parseKnowingTypes(s: String, v: Vector[ValueType]): Vector[PhaseExpression] = {
    // Will fill with empties if more types requested than string elements given
    val split: Array[String] = s.split(",")
    v.zipWithIndex.map(si => parseSingle(split.lift(si._2).getOrElse(""), si._1))
  }

  def parseSingle(s: String, v: ValueType): PhaseExpression = PhaseExpression.parse(s, v)

  def parseTypes(s: String): Vector[ValueType] = TypeExpressionParser.p(s).toVector

  def wrap[T <: PhaseExpression](expression: T): CompositeExpression = {
    CompositeExpression(Vector(expression.description), Vector(expression))
  }

  private object TypeExpressionParser extends RegexParsers {
    override def skipWhitespace = true

    // Partial matches will confuse things!
    // Make sure a supermatch comes before a submatch
    def ANGLE: Parser[ValueType] =
      """(angle_expr|(LinRat|)[Aa]ngle)""".r ^^ { _ => ValueType.AngleExpr }

    def BOOL: Parser[ValueType] = """[bB]ool(ean|)""".r ^^ { _ => ValueType.Boolean }

    def RATIONAL: Parser[ValueType] = """[Rr]ational""".r ^^ { _ => ValueType.Rational }

    def INTEGER: Parser[ValueType] = """[Ii]nt(eger|)""".r ^^ { _ => ValueType.Integer }

    def STRING: Parser[ValueType] = """[Ss]tring""".r ^^ { _ => ValueType.String }

    def LONG: Parser[ValueType] = """long(_string|)""".r ^^ { _ => ValueType.Long }

    def ENUM: Parser[ValueType] = """enum""".r ^^ { _ => ValueType.Enum }

    def EMPTY: Parser[ValueType] = """[Ee]mpty""".r ^^ { _ => ValueType.Empty }


    def term: Parser[ValueType] =
      ANGLE |
        BOOL |
        RATIONAL |
        INTEGER |
        STRING |
        LONG |
        ENUM |
        EMPTY


    def terms: Parser[List[ValueType]] =
      "(" ~ terms ~ ")" ^^ { case _ ~ t ~ _ => t } |
        term ~ "," ~ terms ^^ { case s ~ _ ~ t => s :: t } |
        term ^^ { t => List(t) }

    def expr: Parser[List[ValueType]] =
      terms |
        term ^^ { t => List(t) } |
        "" ^^ { _ => List(ValueType.Empty) }

    def p(s: String): List[ValueType] = parseAll(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw GenericParseException(msg)
      case Error(msg, _) => throw GenericParseException(msg)
    }
  }

}

