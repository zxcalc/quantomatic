package quanto.gui

import org.python.core.{PyDictionary, PySystemState}
import org.python.util.PythonInterpreter

import scala.swing._
import org.gjt.sp.jedit.{Mode, Registers}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.{BorderLayout, Color, Font}
import java.awt.event.{KeyAdapter, KeyEvent}
import javax.swing.ImageIcon

import quanto.util.swing.ToolBar
import quanto.util.UserAlerts.{Elevation, SelfAlertingProcess, alert}

import scala.swing.event.ButtonClicked
import quanto.util._
import java.io.{File, PrintStream}

import quanto.rewrite.Simproc

class PythonEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val pyMode = new Mode("Python")

  //val modeXml =
  //  if (Globals.isBundle) new File("python.xml").getAbsolutePath
  //  else getClass.getResource("python.xml").getPath
  pyMode.setProperty("file", QuantoDerive.pythonModeFile)
  //println(sml.getProperty("file"))

  val code: StandaloneTextArea = StandaloneTextArea.createTextArea()

  // Inject python to expose some relevant variables
  def documentName: String = document.file.map(
    // Assume the user has a project loaded, otherwise shouldn't be able to access GUI
    f => QuantoDerive.CurrentProject.get.relativePath(f)
  ).getOrElse("Unsaved File")

  // Now run the python along with the header
  def codeWithHeader : String = PythonManipulation.addHeader(code.getBuffer.getText, documentName)

  code.setFont(UserOptions.font)

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

  def allSimprocs : Map[String, Simproc] = QuantoDerive.CurrentProject.map(p => p.simprocs).getOrElse(Map())

  reactions += {
    case ButtonClicked(RunButton) =>
      if (execThread == null) {
        val processReporting = new SelfAlertingProcess(s"Python $documentName")

        execThread = new Thread(new Runnable {
          def run() {
            try {
              val python = new PythonInterpreter


              def simprocsFromThisFile = allSimprocs.filter(kv => kv._2.sourceFile == documentName).keys
              // unregister any simprocs previously linked to this file
              simprocsFromThisFile.foreach(simprocName => QuantoDerive.CurrentProject.foreach(
                p => p.simprocs -= simprocName
              ))

              QuantoDerive.CurrentProject.foreach(pr => python.getSystemState.path.add(pr.rootFolder))
              python.set("output", output)
              python.exec(codeWithHeader)

              // Tell the user which simprocs are linked to this file
              alert(s"Simprocs registered to $documentName: " +
                simprocsFromThisFile.mkString(", ")
              )
              // Link this python to those simprocs
              simprocsFromThisFile.foreach(simprocName => QuantoDerive.CurrentProject.foreach(
                p => p.simprocs(simprocName).sourceCode = codeWithHeader
              ))

              processReporting.finish()
            } catch {
              case e : Throwable =>
                processReporting.fail()
                Swing.onEDT { e.printStackTrace(output) }
            } finally {
              execThread = null
            }
          }
        })
        execThread.start()

      } else {
        alert("Python already running, please wait until complete", Elevation.WARNING)
      }

    case ButtonClicked(InterruptButton) =>
      if (execThread != null) {
        execThread.interrupt()
        execThread = null
      }
  }
}
