package quanto.gui

import scala.swing._
import java.awt.{BorderLayout, Dimension}
import org.gjt.sp.jedit.textarea.StandaloneTextArea
import org.gjt.sp.jedit.{Registers, Mode}
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.datatransfer._
import java.io.PrintStream
import quanto.util.{StreamRedirector, SignallingStreamRedirector}
import scala.swing.event.Key
import javax.swing.KeyStroke


object MLEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  def top = new MainFrame {
    title = "ML Editor"

    val pb = new ProcessBuilder("poly")
    pb.redirectErrorStream(true)
    val poly = pb.start()

    val polyCode = new PrintStream(poly.getOutputStream)

    val sml = new Mode("StandardML")
    sml.setProperty("file", getClass.getResource("ml.xml").getPath)
    println(sml.getProperty("file"))
    val mlCode = StandaloneTextArea.createTextArea()
    mlCode.getBuffer.setMode(sml)

    val textPanel = new BorderPanel {
      peer.add(mlCode, BorderLayout.CENTER)
    }

    textPanel.preferredSize = new Dimension(1000, 500)

    val outputTextArea = new TextArea()
    outputTextArea.enabled = false

    val textOut = new TextAreaOutputStream(outputTextArea)

    val polySignals = new SignallingStreamRedirector(poly.getInputStream, textOut)
    polySignals.start()

    polyCode.println("TextIO.print \"hello\\n\";" + ";")
    polyCode.flush()

    val FileMenu = new Menu("File") { menu =>
      mnemonic = Key.F

      val NewAction = new Action("New") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
        menu.contents += new MenuItem(this) { mnemonic = Key.G }
        def apply() { }
      }

      val BuildAction = new Action("Build") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_B, CommandMask))
        menu.contents += new MenuItem(this) { mnemonic = Key.G }
        def apply() {
          polyCode.println(mlCode.getBuffer.getText)
          polyCode.println(";")
          polyCode.flush()
        }
      }
    }



    val emptyOwner = new ClipboardOwner {
      def lostOwnership(clipboard: Clipboard, contents: Transferable) = {}
    }

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

//    val scroll = new ScrollPane
//    scroll.peer.add(text)
    object Split extends SplitPane {
      orientation = Orientation.Horizontal
      contents_=(textPanel, new ScrollPane(outputTextArea))
    }

    contents = Split

    size = new Dimension(1000,800)


    menuBar = new MenuBar {
      contents += FileMenu
    }
  }
}
