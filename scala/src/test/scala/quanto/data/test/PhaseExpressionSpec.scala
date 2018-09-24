package quanto.data.test

import org.scalatest._
import quanto.data.Theory.ValueType
import quanto.data._
import quanto.util.Rational

class PhaseExpressionSpec extends FlatSpec {
  behavior of "A rational number"

  it should "add correctly" in {
    assert(Rational(1, 2) + Rational(2, 3) === Rational(7, 6))
  }

  behavior of "An angle expression"


  def AngleExpression(constant: Rational) = PhaseExpression(constant, Map(), ValueType.AngleExpr)

  def AngleExpression(constant: Rational, coefficients: Map[String, Rational]) =
    PhaseExpression(constant, coefficients, ValueType.AngleExpr)

  def zero = PhaseExpression.zero(ValueType.AngleExpr)

  def one = PhaseExpression.one(ValueType.AngleExpr)

  def testReparse(e: PhaseExpression) {
    assert(e === parse(e.toString))
  }

  def parse(s: String): PhaseExpression = PhaseExpression.parse(s, ValueType.AngleExpr)

  it should "create expressions" in {
    val a1 = AngleExpression(Rational(1,2), Map("a" -> Rational(2,1)))
    assert(a1.constant == Rational(1,2))
    assert(a1.coefficients.size == 1)
    testReparse(a1)
  }


  it should "compare expressions" in {
    val a = AngleExpression(Rational(0), Map("a" -> Rational(1)))
    val b = AngleExpression(Rational(0), Map("b" -> Rational(1)))
    assert(zero === AngleExpression(Rational(0)))
    assert(one === AngleExpression(Rational(1)))
    assert(a + b === b + a)
    assert(zero !== one)
    assert(one + (a * Rational(1, 2)) + (b * 4) === (a * Rational(1, 2)) + one + (b * 4))
    assert((a * Rational(1, 2)) + (a * Rational(2, 3)) === (a * Rational(7, 6)))
  }

  it should "parse '0'" in {
    testReparse(zero)
    assert(parse("") === zero)
    assert(parse("0") === zero)
  }

  it should "parse 'PI'" in {
    testReparse(one)
    assert(parse("\\pi") === one)
    assert(parse("1\\pi") === one)
    assert(parse("1*\\pi") === one)
    assert(parse("1/1\\pi") === one)
    assert(parse("1/1*\\pi") === one)
    assert(parse("1\\pi/1") === one)
    assert(parse("1*\\pi/1") === one)
    assert(parse("\\pi/1") === one)

    assert(parse("pi") === one)
    assert(parse("1pi") === one)
    assert(parse("1*pi") === one)
    assert(parse("1/1pi") === one)
    assert(parse("1/1*pi") === one)
    assert(parse("1pi/1") === one)
    assert(parse("1*pi/1") === one)
    assert(parse("pi/1") === one)

    assert(parse("PI") === one)
    assert(parse("1PI") === one)
    assert(parse("1*PI") === one)
    assert(parse("1/1PI") === one)
    assert(parse("1/1*PI") === one)
    assert(parse("1PI/1") === one)
    assert(parse("1*PI/1") === one)
    assert(parse("PI/1") === one)
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
    val minusPI = one * -1
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

  it should "parse \\pi/2" in {
    val ohPI = AngleExpression(Rational(1, 2))
    testReparse(ohPI)
    assert(parse("1/2") === ohPI)
    assert(parse("\\pi/2") === ohPI)
  }

  it should "parse '+-3/4 PI'" in {
    val tfPI = AngleExpression(Rational(3, 4))
    testReparse(tfPI)
    assert(parse("3/4") === tfPI)
    assert(parse("3/4pi") === tfPI)
    assert(parse("3/4*pi") === tfPI)
    assert(parse("3pi/4") === tfPI)
    assert(parse("3*pi/4") === tfPI)

    val mtfPI = AngleExpression(Rational(-3, 4))
    testReparse(mtfPI)
    assert(parse("-3/4") === mtfPI)
    assert(parse("-3/4pi") === mtfPI)
    assert(parse("-3/4*pi") === mtfPI)
    assert(parse("-3pi/4") === mtfPI)
    assert(parse("-3*pi/4") === mtfPI)
  }

  it should "parse '+-1/4 PI' and '+-1/4 a'" in {
    val fPI = AngleExpression(Rational(1, 4))
    val mfPI = fPI * -1
    val fA = AngleExpression(Rational(0), Map("a" -> Rational(1, 4)))
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

  it should "evaluate a polynomial" in {
    val e1 = parse("2x + 3/4")
    assert(Math.abs(e1.evaluate(Map("x" -> 1.0/8.0)) - 1) < 1e-15)
  }

  it should "evaluate a string" in {
    val s1 = PhaseExpression.parse("Hi", ValueType.String)
    assert(s1.toString == "Hi")
  }

  it should "evaluate an empty" in {
    val e1 = PhaseExpression.parse("Hi", ValueType.Empty)
    assert(e1.toString == "")
  }

  it should "print a rational" in {
    val r1 = PhaseExpression.parse("3/27", ValueType.Rational)
    assert(r1.toString == "1/9")
    val r2 = PhaseExpression.parse("3/27 + a/4", ValueType.Rational)
    assert(r2.toString == "1/9 + 1/4 a")
  }

  it should "parse alpha'" in {
    val e = parse("alpha' + alpha")
    assert(e.coefficients.keys.size == 2)
  }

  it should "substitute beta" in {
    val e = parse("-beta")
    val f = e.subst("beta", parse("0"))
    assert(f == zero)
  }

  it should "substitute composite beta" in {
    val e = parse("-beta")
    val ee = CompositeExpression.wrap(e)
    val f = ee.substSubVariables(Map((ValueType.AngleExpr, "beta") -> "0"))
    assert(f.values.head == zero)
  }
}
