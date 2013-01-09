package quanto.data.test

import quanto.data._
import org.scalatest._

class TheorySpec extends FlatSpec {
  behavior of "A theory"

  var thy: Theory = _

  it should "save to JSON" in {
    pending
  }

  it should "load from JSON" in {
    pending
    thy = Theory.fromJson("")
  }
}
