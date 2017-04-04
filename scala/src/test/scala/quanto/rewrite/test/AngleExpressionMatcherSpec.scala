package quanto.rewrite.test
import org.scalatest._
import quanto.rewrite._
import quanto.data._


class AngleExpressionMatcherSpec extends FlatSpec {
  behavior of "An angle expression matcher"

  it should "handle one-to-many matches" in {
    var m = AngleExpressionMatcher(Vector("a", "b", "c"), Vector("x", "y", "z"))
    m = m.addMatch(AngleExpression.parse("a"), AngleExpression.parse("x + 2 y")).get
    m = m.addMatch(AngleExpression.parse("b"), AngleExpression.parse("z + pi")).get

    println(m.toMap)
  }
}
