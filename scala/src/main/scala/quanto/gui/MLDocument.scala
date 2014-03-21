package quanto.gui

import java.io.{FileWriter, BufferedWriter, File}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import scala.io.Source
import scala.swing.Component
import org.gjt.sp.jedit.buffer.{BufferAdapter, JEditBuffer}
import javax.swing.text.Segment

class JEditBufferUndoStack(textArea: StandaloneTextArea) extends UndoStack {
  val buffer = textArea.getBuffer
  override def canRedo = buffer.canRedo
  override def canUndo = buffer.canUndo

  override def redoActionName = if (canRedo) Some("Edit") else None
  override def undoActionName = if (canUndo) Some("Edit") else None

  override def redo() = {
    buffer.redo(textArea)
    publish(RedoPerformed("Edit"))
  }
  override def undo() = {
    buffer.undo(textArea)
    publish(UndoPerformed("Edit"))
  }

  // none of these do anything
  override def register(aName: String)(f: => Any) {}
  override def cancel() {}
  override def commit() {}
  override def +=(f: => Any) {}
  override def start(aName: String) {}
  override def clear() {
    buffer.setDirty(false)
    buffer match {
      case b1 : JEditBuffer1 =>
        b1.clearUndoStack()
      case _ =>
    }
  }
}

class JEditBuffer1 extends JEditBuffer {
  def clearUndoStack() { undoMgr.clear() }
}


class MLDocument(val parent: Component, textArea: StandaloneTextArea) extends Document { doc =>
  val description = "ML Code"
  val fileExtension = "ML"
  private val _jeditUndoStack = new JEditBufferUndoStack(textArea)
  override def undoStack = _jeditUndoStack

  listenTo(_jeditUndoStack)

  reactions += {
    case UndoPerformed(_) => publish(DocumentChanged(this))
    case RedoPerformed(_) => publish(DocumentChanged(this))
  }

  // the ML, as it was last saved or loaded
//  private var storedCode: String = ""
  def unsavedChanges = textArea.getBuffer.isDirty

  object BufferChanges extends BufferAdapter {
    override def contentInserted(buffer: JEditBuffer, startLine: Int, offset: Int, numLines: Int, length: Int) {
      publish(DocumentChanged(doc))
    }
    override def contentRemoved(buffer: JEditBuffer, startLine: Int, offset: Int, numLines: Int, length: Int) {
      publish(DocumentChanged(doc))
    }
  }

  textArea.getBuffer.addBufferListener(BufferChanges)


  def code = textArea.getBuffer.getText
  def code_=(c: String) {
    textArea.setText(c)

    // clears any stored filename and the undo stack
    resetDocumentInfo()
  }

  protected def loadDocument(f: File) {
    val text = Source.fromFile(f).mkString
    textArea.setText(text)

    undoStack.clear()
    textArea.getBuffer.setDirty(false)
//    textArea.setText("foo")
//    println("can undo: " + textArea.getBuffer.canUndo)
  }

  protected def saveDocument(f: File) {
    //storedCode = textArea.getText
    val fw = new BufferedWriter(new FileWriter(f))
    fw.write(textArea.getText)
    fw.write("\n")
    fw.close()
    textArea.getBuffer.setDirty(false)
  }

  protected def clearDocument() {
    code = ""
  }
}
