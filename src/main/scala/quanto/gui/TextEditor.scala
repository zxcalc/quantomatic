package quanto.gui

import java.awt.BorderLayout
import java.awt.event.{KeyAdapter, KeyEvent}
import java.io.{File, PrintWriter}

import org.gjt.sp.jedit.buffer.JEditBuffer
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import org.gjt.sp.jedit.{Mode, Registers}
import quanto.util.UserOptions

import scala.io.Source
import scala.swing.{BorderPanel, Label}

class TextEditor(val mode: Mode) extends BorderPanel {
  lazy val Component: BorderPanel = new BorderPanel {
    peer.add(TextArea, BorderLayout.CENTER)
  }

  import org.gjt.sp.jedit.IPropertyManager
  import org.gjt.sp.jedit.textarea.StandaloneTextArea

  // TODO: check font availability and/or allow user to select one
  val props = new java.util.Properties()
  val propFile = this.getClass.getResourceAsStream("jedit.props")
  val keyFile = this.getClass.getResourceAsStream("jEdit_keys.props")
  props.load(propFile)
  props.load(keyFile)
  //props.setProperty("view.font", "Arial")
  props.setProperty("view.fontsize", UserOptions.fontSize.toString)
  //props.setProperty("view.fontstyle", "0")
  //props.putAll(loadProperties("/keymaps/jEdit_keys.props"))
  //props.putAll(loadProperties("/org/gjt/sp/jedit/jedit.props"))
  val TextArea = new StandaloneTextArea(new IPropertyManager() {
    override def getProperty(name: String): String = props.getProperty(name)
  })
  //textArea.getBuffer.setProperty("folding", "explicit")
  //val TextArea: StandaloneTextArea = StandaloneTextArea.createTextArea()

  private val buf = new JEditBuffer1
  private val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  def registerBuffer() {
    TextArea.setFont(UserOptions.font)
    buf.setMode(mode)
    TextArea.setBuffer(buf)
    TextArea.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent) {
        if (e.getModifiers == CommandMask) e.getKeyChar match {
          case 'x' => Registers.cut(TextArea, '$')
          case 'c' => Registers.copy(TextArea, '$')
          case 'v' => Registers.paste(TextArea, '$')
          case _ =>
        }
      }
    })
  }

  def getText: String = {
    TextArea.getBuffer.getText
  }

  registerBuffer()
}

object TextEditor {
  // JEdit can't access scala resources directly, so need to make a dummy file to hold the xml
  def makeDummyXMLFile(name: String): String = {
    val f = File.createTempFile(name, "xml")
    f.deleteOnExit()
    val pr = new PrintWriter(f)
    Source.fromInputStream(getClass.getResourceAsStream(s"$name.xml")).foreach(pr.print)
    pr.close()
    f.getCanonicalPath
  }

  object Modes {
    def python: Mode = {
      val mode = new Mode("Python")
      val modeFile = makeDummyXMLFile("python")
      mode.setProperty("file", modeFile)
      mode
    }

    def markdown: Mode = {
      val mode = new Mode("Markdown")
      val modeFile = makeDummyXMLFile("markdown")
      mode.setProperty("file", modeFile)
      mode
    }

    def blank : Mode = {
      val mode = new Mode("Blank")
      val modeFile = makeDummyXMLFile("blank")
      mode.setProperty("file", modeFile)
      mode
    }

    def fromFile(path: String, identifier: String): Mode = {
      val mode = new Mode(identifier)
      val modeFile = path
      mode.setProperty("file", modeFile)
      mode
    }
  }

}