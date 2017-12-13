package quanto.gui

import java.io.File

import quanto.data.{Project, Theory}
import quanto.util.UserAlerts
import quanto.util.json.Json

import scala.swing.{Component, Publisher}

// this is necessarily boring - used to interface correctly with the tabbed GUI
class BatchDerivationCreationDocument(val parent: Component) extends Document with Publisher {
  val description = "Batch Derivation"
  val fileExtension = ""


  protected def clearDocument() {
  }

  protected def saveDocument(f: File) {
  }

  override def loadDocument(f: File) {
  }

  override def unsavedChanges: Boolean = false

  override protected def exportDocument(f: File) {
  }

}
