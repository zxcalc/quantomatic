package quanto.gui

import scala.swing.{Component, FileChooser, Dialog, Publisher}
import java.io.{FileNotFoundException, IOException, File}
import scala.swing.event.Event
import quanto.data.GraphLoadException
import quanto.util.json.JsonParseException

abstract class DocumentEvent extends Event
case class DocumentChanged(sender: Document) extends DocumentEvent
case class DocumentSaved(sender: Document) extends DocumentEvent

/**
 * For an object connected to a single file. Provides an undo stack, tracks changes, and gives
 * convenience functions for loading, saving, etc.
 */

abstract class Document extends Publisher {
  var file: Option[File] = None
  val undoStack = new UndoStack
  def unsavedChanges : Boolean

  protected def clearDocument()
  protected def saveDocument(f: File)
  protected def loadDocument(f: File)
  protected def parent : Component

  protected def resetDocumentInfo() {
    undoStack.clear()
    file = None
    publish(DocumentChanged(this))
  }

  def clear() {
    clearDocument()
    resetDocumentInfo()
  }

  def save(fopt: Option[File] = None) {
    fopt.orElse(file).map { f =>
      try {
        saveDocument(f)
        file = Some(f)
        publish(DocumentSaved(this))
      } catch {
        case _: IOException => errorDialog("save", "file unwriteable")
        case _ => errorDialog("save", "unexpected error")
      }
    }
  }

  def load(f : File) {
    try {
      file = Some(f)
      loadDocument(f)
    } catch {
      case _: JsonParseException => errorDialog("load", "mal-formed JSON")
      case _: GraphLoadException => errorDialog("load", "invalid graph")
      case _: FileNotFoundException => errorDialog("load", "not found")
      case _: IOException => errorDialog("load", "file unreadable")
    }
  }

  def titleDescription =
    file.map(f => f.getName).getOrElse("untitled") + (if (unsavedChanges) "*" else "")

  def promptUnsaved() = {
    if (unsavedChanges) {
      Dialog.showConfirmation(
        title = "Unsaved changes",
        message = "There are unsaved changes, do you wish to continue?") == Dialog.Result.Yes
    } else true
  }

  def promptExists(f: File) = {
    if (f.exists()) {
      Dialog.showConfirmation(
        title = "File exists",
        message = "File exists, do you wish to overwrite?") == Dialog.Result.Yes
    } else true
  }

  def errorDialog(action: String, reason: String) {
    Dialog.showMessage(
      title = "Error",
      message = "Cannot " + action + " file (" + reason + ")",
      messageType = Dialog.Message.Error)
  }

  def showSaveAsDialog() {
    val chooser = new FileChooser()
    chooser.showSaveDialog(parent) match {
      case FileChooser.Result.Approve =>
        if (promptExists(chooser.selectedFile)) save(Some(chooser.selectedFile))
      case _ =>
    }
  }

  def showOpenDialog() {
    if (promptUnsaved()) {
      val chooser = new FileChooser()
      chooser.showOpenDialog(parent) match {
        case FileChooser.Result.Approve =>
          load(chooser.selectedFile)
        case _ =>
      }
    }
  }

  // any time the graph state changes in a meaningful way, an undo is registered
  listenTo(undoStack)
  reactions += {
    case UndoRegistered(_) =>
      publish(DocumentChanged(this))
  }
}
