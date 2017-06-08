package quanto.cosy.test

import org.scalatest._
import quanto.cosy._

/**
  * Created by hector on 02/06/17.
  */
class EquivalenceClassesSpec extends FlatSpec {
  var results = EquivClassRunResults(normalised = false, 4)
  var resultsNormalised = EquivClassRunResults(normalised = true, 4)

  behavior of "Equivalence classes"

  it should "generate some diagrams" in {
    var diagramStream = ColbournReadEnum.enumerate(1, 1, 1, 1)
    assert(diagramStream.nonEmpty)
  }

  it should "create lots of diagrams" in {
    var diagramStream = ColbournReadEnum.enumerate(2,2,2,2)
    results.findEquivalenceClasses(diagramStream)
    resultsNormalised.findEquivalenceClasses(diagramStream)
    assert(results.equivalenceClasses.map(x => x.members.length).sum ==
      resultsNormalised.equivalenceClasses.map(x => x.members.length).sum)
  }
}
