package quanto.data.test

import org.scalatest._
import quanto.data.Theory.ValueType
import quanto.data.{PhaseParseException, PhaseExpression}
import quanto.util.Rational

class BooleanExpressionSpec extends FlatSpec {
  behavior of "A boolean expression"

  def BooleanExpression(constant: Rational) = PhaseExpression(constant, Map(), ValueType.Boolean)

  def BooleanExpression(constant: Int, coefficients: Map[String, Rational]) =
    PhaseExpression(constant, coefficients, ValueType.Boolean)

  def BOOL_FALSE = PhaseExpression.zero(ValueType.Boolean)

  def BOOL_TRUE = PhaseExpression.one(ValueType.Boolean)

  def testReparse(e: PhaseExpression) {
    assert(e === parse(e.toString))
  }

  def parse(s: String): PhaseExpression = PhaseExpression.parse(s, ValueType.Boolean)


  val a = BooleanExpression(0,Map("a"-> 1))
  val b = BooleanExpression(0,Map("b"-> 1))

  it should "output to string" in {
    assert(BOOL_FALSE.toString == "\\False")
    assert(BOOL_TRUE.toString == "\\True")
    assert(BooleanExpression(0,Map("a"-> 1)).toString == "a")
    assert(BooleanExpression(1,Map("a"-> 1)).toString == "\\True + a")
    assert(BooleanExpression(1,Map("a"-> 1, "b"-> 1)).toString == "\\True + a + b")
    assert(BooleanExpression(0,Map("a"-> 1, "b"-> 1)).toString == "a + b")
  }

  it should "compare expressions" in {
    assert(BOOL_FALSE === BooleanExpression(0))
    assert(BOOL_TRUE === BooleanExpression(1))
    assert(a + b === b + a)
    assert(BOOL_FALSE !== BOOL_TRUE)
    assert(BOOL_TRUE + (a * 1) + (b * 4) === (a * 1) + BOOL_TRUE + (b * 4))
    assert((a * 1) + (a * 1) === (a * 0))
  }

  it should "parse '0'" in {
    assert(BOOL_FALSE.toString == "\\False")
    testReparse(BOOL_FALSE)
    assert(parse("") === BOOL_FALSE)
    assert(parse("0") === BOOL_FALSE)
  }

  it should "parse 't'" in {
    testReparse(BOOL_TRUE)
    assert(parse("\\t") === BOOL_TRUE)
    assert(parse("1*\\t") === BOOL_TRUE)

    assert(parse("t") === BOOL_TRUE)
    assert(parse("1*t") === BOOL_TRUE)

    assert(parse("True") === BOOL_TRUE)
    assert(parse("1*True") === BOOL_TRUE)
  }

  it should "parse 'a'" in {
    testReparse(a)
    assert(parse("a") === a)
    assert(parse("1*a") === a)
  }

  it should "parse '-T'" in {
    val minusTrue = BOOL_TRUE * -1
    assert(minusTrue === BOOL_TRUE)
    testReparse(minusTrue)
    assert(parse("-t") === minusTrue)
    assert(parse("-1*t") === minusTrue)
  }

  it should "parse '-a'" in {
    val minusA =  a * -1
    testReparse(minusA)
    assert(parse("-a") === minusA)
    assert(parse("-1*a") === minusA)
  }

  it should "parse '+-1'" in {
    val t = BooleanExpression(1)
    testReparse(t)
    assert(parse("-1") === t)
    assert(parse("-1*true") === t)
    assert(parse("+-1") === t)
  }

  it should "parse addition and subtraction correctly" in {
    testReparse(a + b)
    testReparse(a - b)
    testReparse((a * -1) - b)
    assert(parse("a + b") === a + b)
    assert(parse("a - b") === a - b)
    assert(parse("-a + b") === b - a)
    assert(parse("- a - b") === (a * -1) - b)
    assert(parse("-(a + b)") === (a * -1) - b)
    assert(parse("-(a - b)") === b - a)
  }


  it should "do substitutions correctly" in {
    val e1 = parse("x - 2*y")
    val e2 = parse("a + b - c")
    assert(e1.subst("x", e2) === parse("a + b - c - 2y"))
    assert(e1.subst("y", e2) === parse("x - 2a - 2b + 2c"))
  }

  it should "evaluate a sum" in {
    val e1 = parse("x + 1")
    assert(e1.evaluate(Map("x" -> 1)) == 0)
    val e2 = parse("x + y + z")
    assert(e2.evaluate(Map("x" -> 1, "y" ->1, "z" ->0)) == 0)
  }
}
