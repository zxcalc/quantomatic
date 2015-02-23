package quanto.gui

import scala.swing._
import org.gjt.sp.jedit.{Registers, Mode}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import java.awt.{Color, BorderLayout}
import java.awt.event.{KeyEvent, KeyAdapter}
import javax.swing.ImageIcon
import quanto.util.swing.ToolBar
import quanto.core._
import scala.swing.event.ButtonClicked
import quanto.util._
import java.io.{File, PrintStream}
import java.awt.Font

class MLEditPanel extends BorderPanel with HasDocument {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val sml = new Mode("StandardML")

  val mlModeXml = if (Globals.isBundle) new File("ml.xml").getAbsolutePath
                  else getClass.getResource("ml.xml").getPath
  sml.setProperty("file", mlModeXml)
  println(sml.getProperty("file"))
  val mlCode = StandaloneTextArea.createTextArea()
  //mlCode.setFont(new Font("Menlo", Font.PLAIN, 14))

  val buf = new JEditBuffer1
  buf.setMode(sml)

  mlCode.setBuffer(buf)

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

  val document = new MLDocument(this, mlCode)

  val textPanel = new BorderPanel {
    peer.add(mlCode, BorderLayout.CENTER)
  }

  val RunButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("start.png"), "Run buffer in Poly/ML")
    tooltip = "Run buffer in Poly/ML"
  }

  val InterruptButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("stop.png"), "Interrupt execution")
    tooltip = "Interrupt execution"
  }

  val MLToolbar = new ToolBar {
    contents += (RunButton, InterruptButton)
  }

  val outputTextArea = new TextArea()
  outputTextArea.editable = false
  val textOut = new TextAreaOutputStream(outputTextArea)

  QuantoDerive.core ! AddConsoleOutput(textOut)

  val polyOutput = new PrintStream(new TextAreaOutputStream(outputTextArea))

  add(MLToolbar, BorderPanel.Position.North)

  object Split extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(textPanel, new ScrollPane(outputTextArea))
  }

  add(Split, BorderPanel.Position.Center)

  listenTo(RunButton, InterruptButton)

  reactions += {
    case ButtonClicked(RunButton) =>
      QuantoDerive.ConsoleProgress.indeterminate = true
      QuantoDerive.CoreStatus.text = "Compiling ML"
      QuantoDerive.CoreStatus.foreground = Color.BLUE

      val compileMessage = CompileML(document.file.map(_.getName), mlCode.getBuffer.getText) { msg =>
        Swing.onEDT(msg.stripCodes match {
          case (_ :: _ :: StringPart("S") :: _) =>
            QuantoDerive.CoreStatus.text = "ML compiled sucessfully"
            QuantoDerive.CoreStatus.foreground = new Color(0, 150, 0)
            QuantoDerive.ConsoleProgress.indeterminate = false
//          case (_ :: _ :: StringPart("X") :: _ :: _ :: StringPart("Interrupt\n") :: _) =>
//            QuantoDerive.CoreStatus.text = "ML execution interrupted"
//            QuantoDerive.CoreStatus.foreground = new Color(200, 100, 0)
//            QuantoDerive.ConsoleProgress.indeterminate = false
//          case (_ :: _ :: StringPart("C") :: _) =>
//            QuantoDerive.CoreStatus.text = "ML compilation interrupted"
//            QuantoDerive.CoreStatus.foreground = new Color(200, 100, 0)
//            QuantoDerive.ConsoleProgress.indeterminate = false
          case (_ :: _ :: StringPart("X") :: _) =>
            QuantoDerive.CoreStatus.text = "Exception in ML"
            QuantoDerive.CoreStatus.foreground = Color.RED
            QuantoDerive.ConsoleProgress.indeterminate = false
          case msg =>
            println(msg)
            QuantoDerive.CoreStatus.text = "Unrecognised response from Poly/ML"
            QuantoDerive.CoreStatus.foreground = Color.BLUE
            QuantoDerive.ConsoleProgress.indeterminate = false
        })
      }

      QuantoDerive.core ! compileMessage
    case ButtonClicked(InterruptButton) =>
      QuantoDerive.core ! InterruptML
  }
}
