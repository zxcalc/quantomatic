package quanto.gui

import scala.swing._
import org.gjt.sp.jedit.Mode
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.BorderLayout

class MLEditPanel extends BorderPanel with HasDocument {
  val sml = new Mode("StandardML")
  sml.setProperty("file", getClass.getResource("ml.xml").getPath)
  println(sml.getProperty("file"))
  val mlCode = StandaloneTextArea.createTextArea()
  mlCode.setBuffer(new JEditBuffer1())
  mlCode.getBuffer.setMode(sml)
  val document = new MLDocument(this, mlCode)
  peer.add(mlCode, BorderLayout.CENTER)
}
