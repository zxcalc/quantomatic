package quanto.rewrite.test
import org.scalatest._
import quanto.data.Theory.ValueType
import quanto.rewrite._
import quanto.data._
import quanto.util.Rational


class AngleExpressionMatcherSpec extends FlatSpec {

  def AngleExpressionMatcher(pVars: Vector[String], tVars: Vector[String]) = PhaseExpressionMatcher(pVars, tVars, Some(2))

  def AngleExpression(constant: Rational) = PhaseExpression(constant, Map(), ValueType.AngleExpr)

  def AngleExpression(constant: Rational, coefficients: Map[String, Rational]) =
    PhaseExpression(constant, coefficients, ValueType.AngleExpr)

  def zero = PhaseExpression.zero(ValueType.AngleExpr)

  def one = PhaseExpression.one(ValueType.AngleExpr)

  def testReparse(e: PhaseExpression) {
    assert(e === parse(e.toString))
  }

  def parse(s: String): PhaseExpression = PhaseExpression.parse(s, ValueType.AngleExpr)

  behavior of "An angle expression matcher"

  it should "handle single-variable matches" in {
    var m = AngleExpressionMatcher(Vector("a", "b", "c"), Vector("x", "y", "z"))
    m = m.addMatch(parse("a"), parse("x + 2 y")).get
    m = m.addMatch(parse("b"), parse("z + pi")).get
    val mp = m.toMap

    // check we got correct map
    assert(m.toMap.mapValues(_.as(ValueType.AngleExpr)) === Map("a" -> parse("x + 2 y"), "b" -> parse("z + pi")))

    // check substitutions into pattern yield target
    assert(parse("a").subst(mp) === parse("x + 2 y"))
    assert(parse("b").subst(mp) === parse("z + pi"))
  }

  it should "handle expression matches" in {
    var m = AngleExpressionMatcher(Vector("a", "b", "c"), Vector("x", "y", "z"))
    m = m.addMatch(parse("a + 2 b"), parse("x + 2 y")).get
    m = m.addMatch(parse("b + c"), parse("z + pi")).get
    m = m.addMatch(parse("a - c"), parse("4 x")).get
    val mp = m.toMap(ValueType.AngleExpr)

    // check we got correct map
    assert(mp === Map(
      "a" -> parse("7 x - 2 y + 2 z"),
      "b" -> parse("-pi - 3 x + 2 y - z"),
      "c" -> parse("3 x - 2 y + 2 z")
    ))

    // check substitutions into pattern yield target
    assert(parse("a + 2 b").subst(mp) === parse("x + 2 y"))
    assert(parse("b + c").subst(mp) === parse("z + pi"))
    assert(parse("a - c").subst(mp) === parse("4 x"))
  }

  it should "fail for impossible matches" in {
    var m = AngleExpressionMatcher(Vector("a", "b", "c"), Vector("x", "y", "z"))
    m = m.addMatch(parse("a"), parse("x")).get
    m = m.addMatch(parse("b"), parse("y")).get

    val m1 = m.addMatch(parse("a + b"), parse("z"))
    assert(m1 === None)
  }
}
