package quanto.gui

import quanto.gui.graphview.GraphView
import java.io.File

class RuleDocument(lhsView: GraphView, rhsView: GraphView) extends Document {
  protected def parent = ???

  protected def loadDocument(f: File) = ???

  protected def saveDocument(f: File) = ???

  protected def clearDocument() = ???

  def unsavedChanges = ???
}
