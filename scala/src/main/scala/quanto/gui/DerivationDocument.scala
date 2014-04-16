package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import quanto.layout._

class DerivationDocument(panel: DerivationPanel) extends Document {
  val description = "Derivation"
  val fileExtension = "qderive"

  protected def parent = panel
  private var storedDerivation: Derivation = Derivation(panel.theory, Graph(panel.theory))
  private var _derivation: Derivation = storedDerivation
  def derivation = _derivation
  def derivation_=(d: Derivation) = {
    _derivation = d
    publish(DocumentChanged(this))
  }

  def unsavedChanges = _derivation != storedDerivation

  object rootRef extends HasGraph {
    protected def gr = _derivation.root
    protected def gr_=(g: Graph) {
      _derivation = _derivation.copy(root = g)
    }
  }

  class StepRef(s: DSName) extends HasGraph {
    protected def gr = _derivation.steps(s).graph
    protected def gr_=(g: Graph) {
      _derivation = _derivation.updateGraphInStep(s, g)
    }
  }

  def stepRef(s: DSName) = new StepRef(s)

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    _derivation = Derivation.fromJson(json, panel.theory)
    storedDerivation = _derivation
  }

  protected def saveDocument(f: File)  {
    val json = Derivation.toJson(_derivation, panel.theory)
    json.writeTo(f)
    storedDerivation = _derivation
  }

  protected def clearDocument() = {
    _derivation = Derivation(panel.theory, root = Graph(panel.theory))
  }

  def root_=(g: Graph) {
    rootRef.graph = g
    // if the root is set manually, move to root (which causes a state refresh of the panel)
    panel.controller.state = HeadState(None)
    publish(DocumentChanged(this))
  }

  def root = rootRef.graph
}
