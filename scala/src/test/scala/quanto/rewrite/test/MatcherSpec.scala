package quanto.rewrite.test

import quanto.rewrite._
import quanto.data._
import org.scalatest._

class MatcherSpec extends FlatSpec {
  behavior of "The matcher"

  it should "initialise a match search" in {
    val matches = Matcher.findMatches(Graph(), Graph())
  }
}
