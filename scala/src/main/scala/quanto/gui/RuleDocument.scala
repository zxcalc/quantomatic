package quanto.gui

import quanto.gui.graphview.GraphView
import java.io.File
import quanto.data.{Theory, Rule, Graph}
import quanto.util.json.Json
import scala.swing.Component

class RuleDocument(val parent: Component, theory: Theory) extends Document {
  val description = "Rule"
  val fileExtension = "qrule"
  var lhsGraph = Graph(theory)
  var rhsGraph = Graph(theory)

  private var storedRule: Rule = Rule(Graph(theory), Graph(theory))
  def unsavedChanges =
    storedRule.lhs != lhsGraph ||
    storedRule.rhs != rhsGraph

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    val r = Rule.fromJson(json, theory)
    lhsGraph = r.lhs
    rhsGraph = r.rhs
    storedRule = r
  }

  protected def saveDocument(f: File)  {
    val r = Rule(lhsGraph, rhsGraph)
    val json = Rule.toJson(r, theory)
    json.writeTo(f)
    storedRule = r
  }

  protected def clearDocument() = {
    lhsGraph = Graph(theory)
    rhsGraph = Graph(theory)
    storedRule = Rule(lhsGraph, rhsGraph)
  }
}
