package quanto.gui

import scala.swing._
import org.gjt.sp.jedit.{Registers, Mode}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.BorderLayout
import java.awt.event.{KeyEvent, KeyAdapter}

class MLEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val sml = new Mode("StandardML")
  sml.setProperty("file", getClass.getResource("ml.xml").getPath)
  println(sml.getProperty("file"))
  val mlCode = StandaloneTextArea.createTextArea()
  mlCode.setBuffer(new JEditBuffer1())
  mlCode.getBuffer.setMode(sml)

  mlCode.addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent) {
      if (e.getModifiers == CommandMask) e.getKeyChar match {
        case 'x' => Registers.cut(mlCode, '$')
        case 'c' => Registers.copy(mlCode, '$')
        case 'v' => Registers.paste(mlCode, '$')
        case _ =>
      }
    }
  })

  val document = new MLDocument(this, mlCode)
  peer.add(mlCode, BorderLayout.CENTER)
}
