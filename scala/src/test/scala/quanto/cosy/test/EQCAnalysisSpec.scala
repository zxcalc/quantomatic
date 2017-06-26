package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy._
import quanto.data.{Rule, Theory}

/**
  * Test files for the EQCAnalysis class
  * It should take equivalence classes and return results about that class as a whole
  */
class EQCAnalysisSpec extends FlatSpec {
  behavior of "Connected Components"

  val rg: Theory = Theory.fromFile("red_green")
  var emptyRuleList: List[Rule] = List()


  it should "find a 1-colour adjmat" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true))
    assert(EQCAnalysis.AdjMatConnectedComponents(amat) == 1)
  }

  it should "find a 2-colour adjmat" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false))
    assert(EQCAnalysis.AdjMatConnectedComponents(amat) == 2)
  }

  it should "accept an equivalence class" in {
    var results = EquivClassRunAdjMat(numAngles = 2,
      tolerance = EquivClassRunAdjMat.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    var eqcConCom = results.equivalenceClassesNormalised.map(e => EQCAnalysis.AdjMatConnectedComponents(e))
    println(eqcConCom)
  }
}
