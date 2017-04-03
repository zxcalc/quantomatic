package quanto.data.test

import org.scalatest._
import quanto.data._
import AngleExpression._
import quanto.util.Rational

class AngleExpressionSpec extends FlatSpec {
  behavior of "A rational number"

  it should "add correctly" in {
    assert(Rational(1,2) + Rational(2,3) === Rational(7,6))
  }

  behavior of "An angle expression"

  def testReparse(e : AngleExpression) {
    assert(e === parse(e.toString))
  }

  it should "compare expressions" in {
    val a = AngleExpression(Rational(0), Map("a" -> Rational(1)))
    val b = AngleExpression(Rational(0), Map("b" -> Rational(1)))
    assert(ZERO === AngleExpression(Rational(0)))
    assert(ONE_PI === AngleExpression(Rational(1)))
    assert(a + b === b + a)
    assert(ZERO !== ONE_PI)
    assert(ONE_PI + (a * Rational(1,2)) + (b * 4) === (a * Rational(1,2)) + ONE_PI + (b * 4))
    assert((a * Rational(1,2)) + (a * Rational(2,3)) === (a * Rational(7,6)))
  }

  it should "parse '0'" in {
    testReparse(ZERO)
    assert(parse("") === ZERO)
    assert(parse("0") === ZERO)
  }

  it should "parse 'PI'" in {
    testReparse(ONE_PI)
    assert(parse("\\pi") === ONE_PI)
    assert(parse("1\\pi") === ONE_PI)
    assert(parse("1*\\pi") === ONE_PI)
    assert(parse("1/1\\pi") === ONE_PI)
    assert(parse("1/1*\\pi") === ONE_PI)
    assert(parse("1\\pi/1") === ONE_PI)
    assert(parse("1*\\pi/1") === ONE_PI)
    assert(parse("\\pi/1") === ONE_PI)

    assert(parse("pi") === ONE_PI)
    assert(parse("1pi") === ONE_PI)
    assert(parse("1*pi") === ONE_PI)
    assert(parse("1/1pi") === ONE_PI)
    assert(parse("1/1*pi") === ONE_PI)
    assert(parse("1pi/1") === ONE_PI)
    assert(parse("1*pi/1") === ONE_PI)
    assert(parse("pi/1") === ONE_PI)

    assert(parse("PI") === ONE_PI)
    assert(parse("1PI") === ONE_PI)
    assert(parse("1*PI") === ONE_PI)
    assert(parse("1/1PI") === ONE_PI)
    assert(parse("1/1*PI") === ONE_PI)
    assert(parse("1PI/1") === ONE_PI)
    assert(parse("1*PI/1") === ONE_PI)
    assert(parse("PI/1") === ONE_PI)
  }

  it should "parse 'a'" in {
    val a = AngleExpression(Rational(0), Map("a" -> Rational(1)))
    testReparse(a)
    assert(parse("a") === a)
    assert(parse("1a") === a)
    assert(parse("1*a") === a)
    assert(parse("1/1a") === a)
    assert(parse("1/1*a") === a)
    assert(parse("1a/1") === a)
    assert(parse("1*a/1") === a)
    assert(parse("a/1") === a)
  }

  it should "parse '-PI'" in {
    val minusPI = ONE_PI * -1
    testReparse(minusPI)
    assert(parse("-pi") === minusPI)
    assert(parse("-1pi") === minusPI)
    assert(parse("-1*pi") === minusPI)
    assert(parse("-1/1pi") === minusPI)
    assert(parse("-1/1*pi") === minusPI)
    assert(parse("-1pi/1") === minusPI)
    assert(parse("-1*pi/1") === minusPI)
    assert(parse("-pi/1") === minusPI)
  }

  it should "parse '-a'" in {
    val minusA = AngleExpression(Rational(0), Map("a" -> Rational(-1)))
    testReparse(minusA)
    assert(parse("-a") === minusA)
    assert(parse("-1a") === minusA)
    assert(parse("-1*a") === minusA)
    assert(parse("-1/1a") === minusA)
    assert(parse("-1/1*a") === minusA)
    assert(parse("-1a/1") === minusA)
    assert(parse("-1*a/1") === minusA)
    assert(parse("-a/1") === minusA)
  }

  it should "parse '+-3/4 PI'" in {
    val tfPI = AngleExpression(Rational(3,4))
    testReparse(tfPI)
    assert(parse("3/4") === tfPI)
    assert(parse("3/4pi") === tfPI)
    assert(parse("3/4*pi") === tfPI)
    assert(parse("3pi/4") === tfPI)
    assert(parse("3*pi/4") === tfPI)

    val mtfPI = AngleExpression(Rational(-3,4))
    testReparse(mtfPI)
    assert(parse("-3/4") === mtfPI)
    assert(parse("-3/4pi") === mtfPI)
    assert(parse("-3/4*pi") === mtfPI)
    assert(parse("-3pi/4") === mtfPI)
    assert(parse("-3*pi/4") === mtfPI)
  }

  it should "parse '+-1/4 PI' and '+-1/4 a'" in {
    val fPI = AngleExpression(Rational(1,4))
    val mfPI = fPI * -1
    val fA = AngleExpression(Rational(0), Map("a" -> Rational(1,4)))
    val mfA = fA * -1
    assert(parse("pi/4") === fPI)
    assert(parse("-pi/4") === mfPI)
    assert(parse("a/4") === fA)
    assert(parse("-a/4") === mfA)
  }

  it should "parse addition and subtraction correctly" in {
    val a = AngleExpression(Rational(0), Map("a" -> Rational(1)))
    val b = AngleExpression(Rational(0), Map("b" -> Rational(1)))
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
    val e1 = parse("x - 2 y")
    val e2 = parse("a + b - c")
    assert(e1.subst("x", e2) === parse("a + b - c - 2y"))
    assert(e1.subst("y", e2) === parse("x - 2a - 2b + 2c"))
  }
}
