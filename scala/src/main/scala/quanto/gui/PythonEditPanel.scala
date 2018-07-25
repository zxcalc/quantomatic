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

import scala.swing.event.{ButtonClicked, Event}
import quanto.util._
import java.io.{File, PrintStream}

import quanto.rewrite.Simproc

case class SimprocsUpdated() extends Event

class PythonEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val CodeArea : TextEditor = new TextEditor(TextEditor.Modes.python)

  // Inject python to expose some relevant variables
  def documentName: String = document.file.map(
    // Assume the user has a project loaded, otherwise shouldn't be able to access GUI
    f => QuantoDerive.CurrentProject.get.relativePath(f)
  ).getOrElse("Unsaved File")

  // Now run the python along with the header
  def code : String = CodeArea.getText

  val document = new CodeDocument("Python Script", "py", this, CodeArea.TextArea)
  listenTo(document)

  var execThread : Thread = null
  val textPanel = CodeArea.Component

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
    case DocumentRequestingNaturalFocus(_) =>
      CodeArea.TextArea.requestFocus()
    case ButtonClicked(RunButton) =>
      if (execThread == null) {
        val processReporting = new SelfAlertingProcess(s"Python $documentName")

        execThread = new Thread(new Runnable {
          def run() {
            try {
              val python = new PythonInterpreter

              QuantoDerive.CurrentProject.foreach(project => project.lastRunPythonFilePath = Some(documentName))
              def simprocsFromThisFile = allSimprocs.filter(kv => kv._2.sourceFile == documentName).keys
              // unregister any simprocs previously linked to this file
              simprocsFromThisFile.foreach(simprocName => QuantoDerive.CurrentProject.foreach(
                p => p.simprocs -= simprocName
              ))

              QuantoDerive.CurrentProject.foreach(pr => python.getSystemState.path.add(pr.rootFolder))
              outputTextArea.text = ""
              python.set("output", output)
              python.exec(code)

              // Tell the user which simprocs are linked to this file
              alert(s"Simprocs registered to $documentName: " +
                simprocsFromThisFile.mkString(", ")
              )
              // Link this python to those simprocs
              simprocsFromThisFile.foreach(simprocName => QuantoDerive.CurrentProject.foreach(
                p => p.simprocs(simprocName).sourceCode = code
              ))
              PythonEditPanel.publishUpdate()
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

object PythonEditPanel extends Publisher {
  def publishUpdate() : Unit = {
    publish(SimprocsUpdated())
  }
}