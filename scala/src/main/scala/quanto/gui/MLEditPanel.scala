package quanto.gui

import scala.swing._
import org.gjt.sp.jedit.{Registers, Mode}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.BorderLayout
import java.awt.event.{KeyEvent, KeyAdapter}
import javax.swing.ImageIcon
import quanto.util.swing.ToolBar

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

  val textPanel = new BorderPanel {
    peer.add(mlCode, BorderLayout.CENTER)
  }

  val RunButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("start.png"), "Run buffer in Poly/ML")
  }

  val InterruptButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("stop.png"), "Interrupt execution")
  }

  val MLToolbar = new ToolBar {
    contents += (RunButton, InterruptButton)
  }

  val outputTextArea = new TextArea()
  outputTextArea.enabled = false

  val polyOutput = new TextAreaOutputStream(outputTextArea)

  add(MLToolbar, BorderPanel.Position.North)

  object Split extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(textPanel, new ScrollPane(outputTextArea))
  }

  add(Split, BorderPanel.Position.Center)
}
