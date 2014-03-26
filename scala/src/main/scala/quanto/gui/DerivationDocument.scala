package quanto.gui

import java.io.File
import quanto.data.{Graph, Derivation}
import quanto.util.json.Json

class DerivationDocument(panel: DerivationPanel) extends Document {
  val description = "Derivation"
  val fileExtension = "qrule"

  protected def parent = panel
  private var storedDerivation: Derivation = Derivation(panel.theory, Graph(panel.theory))
  private var _derivation: Derivation = storedDerivation
  def unsavedChanges = _derivation != storedDerivation

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
    _derivation = Derivation(panel.theory, Graph(panel.theory))
  }
}
