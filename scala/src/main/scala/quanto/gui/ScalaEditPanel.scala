package quanto.gui

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.swing._
import org.gjt.sp.jedit.{Registers, Mode}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.{Color, BorderLayout}
import java.awt.event.{KeyEvent, KeyAdapter}
import javax.swing.ImageIcon
import quanto.util.swing.ToolBar
import scala.swing.event.ButtonClicked
import quanto.util._
import java.io.{File, PrintStream}

class ScalaEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val sml = new Mode("StandardML")

  val scalaModeXml = if (Globals.isBundle) new File("scala.xml").getAbsolutePath
  else getClass.getResource("scala.xml").getPath
  sml.setProperty("file", scalaModeXml)
  //println(sml.getProperty("file"))
  val scalaCode = StandaloneTextArea.createTextArea()
  //mlCode.setFont(new Font("Menlo", Font.PLAIN, 14))

  val buf = new JEditBuffer1
  buf.setMode(sml)

  var scalaThread : Thread = null

  scalaCode.setBuffer(buf)

  //  mlCode.addKeyListener(new KeyAdapter {
  //    override def keyPressed(e: KeyEvent) {
  //      if (e.getModifiers == CommandMask) e.getKeyChar match {
  //        case 'x' => Registers.cut(mlCode, '$')
  //        case 'c' => Registers.copy(mlCode, '$')
  //        case 'v' => Registers.paste(mlCode, '$')
  //        case _ =>
  //      }
  //    }
  //  })

  val document = new CodeDocument("Scala Code", "scala", this, scalaCode)

  val toolbox = runtimeMirror(getClass.getClassLoader).mkToolBox() //currentMirror.mkToolBox()


  val textPanel = new BorderPanel {
    peer.add(scalaCode, BorderLayout.CENTER)
  }

  val RunButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("start.png"), "Run scala code")
    tooltip = "Run Scala"
  }

  val InterruptButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("stop.png"), "Interrupt execution")
    tooltip = "Interrupt execution"
  }

  val ScalaToolbar = new ToolBar {
    contents += (RunButton, InterruptButton)
  }

  val outputTextArea = new TextArea()
  outputTextArea.editable = false
  val textOut = new TextAreaOutputStream(outputTextArea)

  val scalaOutput = new PrintStream(new TextAreaOutputStream(outputTextArea))

  add(ScalaToolbar, BorderPanel.Position.North)

  object Split extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(textPanel, new ScrollPane(outputTextArea))
  }

  add(Split, BorderPanel.Position.Center)

  listenTo(RunButton, InterruptButton)

  reactions += {
    case ButtonClicked(RunButton) =>
      if (scalaThread == null) {
        QuantoDerive.CoreStatus.text = "Running scala code"
        QuantoDerive.CoreStatus.foreground = Color.BLUE

        scalaThread = new Thread(new Runnable {
          def run() {
            try {
              val tree = toolbox.parse(scalaCode.getBuffer.getText())
              toolbox.eval(tree)
              QuantoDerive.CoreStatus.text = "Scala compiled sucessfully"
              QuantoDerive.CoreStatus.foreground = new Color(0, 150, 0)
            } catch {
              case e =>
                QuantoDerive.CoreStatus.text = "Error in scala code"
                QuantoDerive.CoreStatus.foreground = Color.RED
                Swing.onEDT { e.printStackTrace(scalaOutput) }
            } finally {
              scalaThread = null
            }
          }
        })
        scalaThread.start()

      } else {
        QuantoDerive.CoreStatus.text = "Scala already running"
        QuantoDerive.CoreStatus.foreground = Color.RED
      }

    case ButtonClicked(InterruptButton) =>
      if (scalaThread != null) {
        scalaThread.interrupt()
        scalaThread = null
      }
  }
}
