package quanto.gui

import quanto.gui.graphview.GraphView
import java.io.File
import quanto.data.{Rule, Graph}
import quanto.util.json.Json

class RuleDocument(lhsView: GraphView, rhsView: GraphView) extends Document {
  protected def parent = lhsView
  private var storedRule: Rule = Rule(Graph(lhsView.theory), Graph(rhsView.theory))
  def unsavedChanges =
    storedRule.lhs != lhsView.graph ||
    storedRule.rhs != rhsView.graph

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    val r = Rule.fromJson(json, lhsView.theory)
    storedRule = r

    lhsView.graph = r.lhs
    lhsView.invalidateGraph()
    lhsView.repaint()

    rhsView.graph = r.rhs
    rhsView.invalidateGraph()
    rhsView.repaint()
  }

  protected def saveDocument(f: File)  {
    val r = Rule(lhsView.graph, rhsView.graph)
    val json = Rule.toJson(r, lhsView.theory)
    json.writeTo(f)
    storedRule = r
  }

  protected def clearDocument() = {
    val r = Rule(Graph(lhsView.theory), Graph(rhsView.theory))
    lhsView.graph = r.lhs
    lhsView.invalidateGraph()
    lhsView.repaint()

    rhsView.graph = r.rhs
    rhsView.invalidateGraph()
    rhsView.repaint()
  }
}
