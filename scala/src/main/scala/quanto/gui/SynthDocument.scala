package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.{JsonAccessException, Json}
import scala.swing.Component



class SynthDocument(val parent: Component, theory: Theory) extends Document {
  val description = "Synthesis"
  val fileExtension = "qsynth"
  var synth : Synth = _
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
    synth = Synth.fromJson(json, theory)
    publish(SynthReady())
  }

  protected def saveDocument(f: File) {
    // ....
  }

  protected def clearDocument() {
    synth = Synth()
  }
}
