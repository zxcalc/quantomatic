package quanto.gui

import java.io.File

import quanto.data.{Project, Theory}
import quanto.util.UserAlerts
import quanto.util.json.Json

import scala.swing.{Component, Publisher}

//
// This file is intentionally bare because this panel will spawn jobs, rather than display files
//
class BatchDerivationCreationDocument(val parent: Component) extends Document with Publisher {
  val description = "Batch Derivation"
  val fileExtension = ""


  protected def clearDocument() {
  }

  protected def saveDocument(f: File) {
  }

  override def loadDocument(f: File) {
  }

  override def titleDescription: String = "Batch Derivation"

  override def unsavedChanges: Boolean = false

  override protected def exportDocument(f: File) {
  }

}
