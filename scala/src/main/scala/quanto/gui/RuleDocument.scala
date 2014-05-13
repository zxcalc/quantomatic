package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import scala.swing.Component
import quanto.util.FileHelper.printToFile

class RuleDocument(val parent: Component, theory: Theory) extends Document {
  val description = "Rule"
  val fileExtension = "qrule"
  var derivation: Option[String] = None

  object lhsRef extends HasGraph {
    protected var gr = Graph(theory)
  }

  object rhsRef extends HasGraph {
    protected var gr = Graph(theory)
  }

  def rule = Rule(lhsRef.graph, rhsRef.graph, derivation)
  def rule_=(r: Rule) {
    lhsRef.graph = r.lhs
    rhsRef.graph = r.rhs
    derivation = r.derivation
    lhsRef.publish(GraphReplaced(lhsRef, clearSelection = true))
    rhsRef.publish(GraphReplaced(rhsRef, clearSelection = true))
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
    derivation = r.derivation
    lhsRef.publish(GraphReplaced(lhsRef, clearSelection = true))
    rhsRef.publish(GraphReplaced(rhsRef, clearSelection = true))
    storedRule = r
  }

  protected def saveDocument(f: File)  {
    val r = Rule(lhsRef.graph, rhsRef.graph, derivation)
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

  override protected def exportDocument(f: File) = {
    val (lhsView, rhsView) = parent match {
      case component : RuleEditPanel => (component.lhsView, component.rhsView)
      case _ => throw new Exception(
        "Exporting from this component is not supported. Please report bug"
      )
    }

    lhsView.exportView(f, false)

    printToFile(f, true)( p => {
      p.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      p.println("\\quad = \\quad")
      p.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    })

    rhsView.exportView(f, true)
  }
}
