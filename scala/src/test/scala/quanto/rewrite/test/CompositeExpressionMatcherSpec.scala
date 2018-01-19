package quanto.rewrite.test

import org.scalatest._
import quanto.data.Theory.ValueType
import quanto.data._
import quanto.rewrite._
import quanto.util.Rational


class CompositeExpressionMatcherSpec extends FlatSpec {

  def AngleExpression(constant: Rational) = PhaseExpression(constant, Map(), ValueType.AngleExpr)

  def AngleExpression(constant: Rational, coefficients: Map[String, Rational]) =
    PhaseExpression(constant, coefficients, ValueType.AngleExpr)

  def parse(types: String, values: String) = CompositeExpression.parse(types, values)

  def composite(p: PhaseExpression): CompositeExpression = CompositeExpression.wrap(p)

  def zero = PhaseExpression.zero(ValueType.AngleExpr)

  def one = PhaseExpression.one(ValueType.AngleExpr)

  def testReparse(e: PhaseExpression) {
    assert(e === parse(e.toString))
  }

  def parse(s: String): PhaseExpression = PhaseExpression.parse(s, ValueType.AngleExpr)

  behavior of "An angle expression matcher"

  it should "handle single-variable matches" in {

    var m = CompositeExpressionMatcher()
    m = m.addMatch(parse("angle", "a"), parse("angle", "x + 2 y")).get
    m = m.addMatch(parse("angle", "b"), parse("angle", "z + pi")).get
    val mp = m.toMap

    // check we got correct map
    assert(m.toMap.values.flatten.toMap ===
      Map("a" -> parse("angle", "x + 2 y").firstOrError(ValueType.AngleExpr), "b" -> parse("angle", "z + pi").firstOrError(ValueType.AngleExpr)))

    // check substitutions into pattern yield target
    assert(parse("angle", "a").substSubValues(mp.values.flatten.toMap) === parse("angle", "x + 2 y"))
    assert(parse("angle", "b").substSubValues(mp.values.flatten.toMap) === parse("angle", "z + pi"))
  }

  it should "handle expression matches" in {
    var m = CompositeExpressionMatcher()
    m = m.addMatch(parse("angle, boolean", "a + 2 b, d"), parse("angle, boolean", "x + 2 y, true")).get
    m = m.addMatch(parse("angle, boolean", "b + c, false"), parse("angle, boolean", "z + pi, false")).get
    m = m.addMatch(parse("angle, boolean", "a - c,false"), parse("angle, boolean", "4 x, false")).get
    val mp = m.toMap

    // check we got correct map
    assert(mp(ValueType.AngleExpr) === Map(
      "a" -> parse("angle", "7 x - 2 y + 2 z").firstOrError(ValueType.AngleExpr),
      "b" -> parse("angle", "-pi - 3 x + 2 y - z").firstOrError(ValueType.AngleExpr),
      "c" -> parse("angle", "3 x - 2 y + 2 z").firstOrError(ValueType.AngleExpr)
    ))
    assert(mp(ValueType.Boolean) === Map(
      "d" -> parse("boolean", "true").firstOrError(ValueType.Boolean)
    ))

    // check substitutions into pattern yield target
    assert(parse("angle, boolean",  "a + 2 b, d").substSubValues(mp.values.flatten.toMap) ===
      parse("angle,boolean","x + 2 y, true"))
    assert(parse("angle, boolean", "b + c").substSubValues(mp.values.flatten.toMap) ===
      parse("angle, boolean","z + pi"))
    assert(parse("angle, boolean", "a - c").substSubValues(mp.values.flatten.toMap) ===
      parse("angle, boolean","4 x"))
  }
}
