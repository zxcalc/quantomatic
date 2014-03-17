package quanto.gui

import scala.swing._
import java.awt.{BorderLayout, Dimension}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import org.gjt.sp.jedit.Mode
import org.gjt.sp.jedit.syntax.ModeProvider

//import org.gjt.sp.jedit.textarea.StandaloneTextArea
//import org.gjt.sp.jedit.jEdit




object MLEditor extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "ML Editor - "

    val sml = new Mode("StandardML")
    sml.setProperty("file", getClass.getResource("ml.xml").getPath)
    println(sml.getProperty("file"))
    val text = StandaloneTextArea.createTextArea()
    text.getBuffer.setMode(sml)

    contents = new BorderPanel {
      peer.add(text, BorderLayout.CENTER)
    }

    size = new Dimension(1000,800)




    menuBar = new MenuBar {
    }
  }
}
