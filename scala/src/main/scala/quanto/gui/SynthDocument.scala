package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import scala.swing.Component


class SynthDocument(val parent: Component, theory: Theory) extends Document {
  val description = "Synthesis"
  val fileExtension = "qsynth"

  def unsavedChanges = false

  //  protected def gr = _graph
  //  protected def gr_=(g: Graph) {
  //    _graph = g
  //
  //    // clears any stored filename and the undo stack
  //    resetDocumentInfo()
  //  }

  protected def loadDocument(f: File) {
    val json = Json.parse(f)

  }

  protected def saveDocument(f: File) {

  }

  protected def clearDocument() {

  }
}
