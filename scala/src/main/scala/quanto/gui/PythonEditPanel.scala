package quanto.gui

import org.python.util.PythonInterpreter

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

class PythonEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val pyMode = new Mode("Python")

  val modeXml = if (Globals.isBundle) new File("python.xml").getAbsolutePath
  else getClass.getResource("python.xml").getPath
  pyMode.setProperty("file", modeXml)
  //println(sml.getProperty("file"))
  val code = StandaloneTextArea.createTextArea()

  //mlCode.setFont(new Font("Menlo", Font.PLAIN, 14))

  val buf = new JEditBuffer1
  buf.setMode(pyMode)

  var execThread : Thread = null

  code.setBuffer(buf)

  code.addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent) {
      if (e.getModifiers == CommandMask) e.getKeyChar match {
        case 'x' => Registers.cut(code, '$')
        case 'c' => Registers.copy(code, '$')
        case 'v' => Registers.paste(code, '$')
        case _ =>
      }
    }
  })

  val document = new CodeDocument("Python Script", "py", this, code)


  val textPanel = new BorderPanel {
    peer.add(code, BorderLayout.CENTER)
  }

  val RunButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("start.png"), "Run scala code")
    tooltip = "Run Scala"
  }

  val InterruptButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("stop.png"), "Interrupt execution")
    tooltip = "Interrupt execution"
  }

  val Toolbar = new ToolBar {
    contents += (RunButton, InterruptButton)
  }

  val outputTextArea = new TextArea()
  outputTextArea.editable = false
  val textOut = new TextAreaOutputStream(outputTextArea)

  val output = new PrintStream(new TextAreaOutputStream(outputTextArea))

  add(Toolbar, BorderPanel.Position.North)

  object Split extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(textPanel, new ScrollPane(outputTextArea))
  }

  add(Split, BorderPanel.Position.Center)

  listenTo(RunButton, InterruptButton)

  reactions += {
    case ButtonClicked(RunButton) =>
      if (execThread == null) {
        QuantoDerive.CoreStatus.text = "Running python code"
        QuantoDerive.CoreStatus.foreground = Color.BLUE

        execThread = new Thread(new Runnable {
          def run() {
            try {
              val python = new PythonInterpreter
              python.set("output", output)
              python.exec(code.getBuffer.getText)
              QuantoDerive.CoreStatus.text = "Python ran sucessfully"
              QuantoDerive.CoreStatus.foreground = new Color(0, 150, 0)
            } catch {
              case e =>
                QuantoDerive.CoreStatus.text = "Error in python code"
                QuantoDerive.CoreStatus.foreground = Color.RED
                Swing.onEDT { e.printStackTrace(output) }
            } finally {
              execThread = null
            }
          }
        })
        execThread.start()

      } else {
        QuantoDerive.CoreStatus.text = "Python already running"
        QuantoDerive.CoreStatus.foreground = Color.RED
      }

    case ButtonClicked(InterruptButton) =>
      if (execThread != null) {
        execThread.interrupt()
        execThread = null
      }
  }
}
