package quanto.gui

import quanto.data.Graph
import java.io.{FileWriter, BufferedWriter, File}
import quanto.util.json.Json
import quanto.gui.graphview.GraphView
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import scala.io.Source
import scala.swing.Component


class MLDocument(val parent: Component, textArea: StandaloneTextArea) extends Document {
  val description = "ML Code"
  val fileExtension = "ML"

  // the ML, as it was last saved or loaded
  private var storedCode: String = ""
  def unsavedChanges = storedCode != textArea.getBuffer.getText

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
