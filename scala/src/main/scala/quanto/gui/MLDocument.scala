package quanto.gui

import java.io.{FileWriter, BufferedWriter, File}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import scala.io.Source
import scala.swing.Component
import org.gjt.sp.jedit.buffer.{BufferAdapter, JEditBuffer}

class JEditBufferUndoStack(textArea: StandaloneTextArea) extends UndoStack {
  val buffer = textArea.getBuffer
  override def canRedo = buffer.canRedo
  override def canUndo = buffer.canUndo

  override def redoActionName = if (canRedo) Some("Edit") else None
  override def undoActionName = if (canUndo) Some("Edit") else None

  override def redo() = buffer.redo(textArea)
  override def undo() = buffer.undo(textArea)

  // none of these do anything
  override def register(aName: String)(f: => Any) {}
  override def cancel() {}
  override def commit() {}
  override def +=(f: => Any) {}
  override def start(aName: String) {}
  override def clear() {}
}


class MLDocument(val parent: Component, textArea: StandaloneTextArea) extends Document { doc =>
  val description = "ML Code"
  val fileExtension = "ML"
  private val _jeditUndoStack = new JEditBufferUndoStack(textArea)
  override def undoStack = _jeditUndoStack

  // the ML, as it was last saved or loaded
  private var storedCode: String = ""
  def unsavedChanges = storedCode != textArea.getBuffer.getText

  textArea.getBuffer.addBufferListener(new BufferAdapter {
    override def contentInserted(buffer: JEditBuffer, startLine: Int, offset: Int, numLines: Int, length: Int) {
      publish(DocumentChanged(doc))
    }
    override def contentRemoved(buffer: JEditBuffer, startLine: Int, offset: Int, numLines: Int, length: Int) {
      publish(DocumentChanged(doc))
    }
  })

  def code = textArea.getBuffer.getText
  def code_=(c: String) {
    textArea.setText(c)

    // clears any stored filename and the undo stack
    resetDocumentInfo()
  }

  protected def loadDocument(f: File) {
    val text = Source.fromFile(f).mkString
    storedCode = text
    textArea.setText(text)
  }

  protected def saveDocument(f: File) {
    storedCode = textArea.getText
    val fw = new BufferedWriter(new FileWriter(f))
    fw.write(storedCode)
    fw.write("\n")
    fw.close()
  }

  protected def clearDocument() {
    code = ""
  }
}
