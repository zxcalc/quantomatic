package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import scala.swing.Component

class RuleDocument(val parent: Component, theory: Theory) extends Document {
  val description = "Rule"
  val fileExtension = "qrule"

  object lhsRef extends HasGraph {
    protected var gr = Graph(theory)
  }

  object rhsRef extends HasGraph {
    protected var gr = Graph(theory)
  }

  private var storedRule: Rule = Rule(Graph(theory), Graph(theory))
  def unsavedChanges =
    storedRule.lhs != lhsRef.graph ||
    storedRule.rhs != rhsRef.graph

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    val r = Rule.fromJson(json, theory)
    lhsRef.graph = r.lhs
    rhsRef.graph = r.rhs
    lhsRef.publish(GraphReplaced(lhsRef, clearSelection = true))
    rhsRef.publish(GraphReplaced(rhsRef, clearSelection = true))
    storedRule = r
  }

  protected def saveDocument(f: File)  {
    val r = Rule(lhsRef.graph, rhsRef.graph)
    val json = Rule.toJson(r, theory)
    json.writeTo(f)
    storedRule = r
  }

  protected def clearDocument() = {
    lhsRef.graph = Graph(theory)
    rhsRef.graph = Graph(theory)
    storedRule = Rule(lhsRef.graph, rhsRef.graph)

    lhsRef.publish(GraphReplaced(lhsRef, clearSelection = true))
    rhsRef.publish(GraphReplaced(rhsRef, clearSelection = true))
  }
}
