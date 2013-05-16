package quanto.gui

import swing._
import event.{TableUpdated, Key, KeyPressed}
import java.awt.{Font => AWTFont, Color}
import javax.swing.border.LineBorder
import apple.laf.JRSUIConstants.AlignmentVertical

sealed abstract class CoreOutputItem
case class Waiting(rid: Int) extends CoreOutputItem {
  override def toString = "waiting for request " + rid + "..."
}

case class ErrorMessage(rid: Int, output: String) extends CoreOutputItem {
  override def toString = output
}
case class Output(rid: Int, output: String) extends CoreOutputItem {
  override def toString = output
}

object JsonConsole extends SimpleSwingApplication {

//  object OutputModel {
//
//    case class Cell(row: Int, column: Int) {
//      override def toString = "foo"
//    }
//    val cells = Array.ofDim[Cell](3,1)
//    for (i <- 0 until 3) cells(i)(0) = Cell(i,0)
//  }

  val JsonOutput = new ListView[CoreOutputItem] {
    renderer = new ListView.Renderer[CoreOutputItem] {
      val label = new Label
      label.horizontalAlignment = Alignment.Left
      val panel = new BorderPanel {
        add(label, BorderPanel.Position.Center)
        border = new LineBorder(Color.WHITE, 1)
      }

      def componentFor(list: ListView[_], isSelected: Boolean,
                       focused: Boolean, a: CoreOutputItem, index: Int): Component =
      {

        a match {
          case Waiting(_) =>
            panel.background = new Color(0.9f,0.9f,1.0f)
            label.border = new LineBorder(new Color(0.2f,0.2f,0.4f))
          case ErrorMessage(_,_) =>
            panel.background = new Color(1.0f,0.5f,0.5f)
            label.border = new LineBorder(new Color(0.4f,0.2f,0.2f))
          case Output(_,_) =>
            panel.background = new Color(0.9f,1.0f,0.9f)
            label.border = new LineBorder(new Color(0.2f,0.6f,0.2f))
        }

        label.text = "<html><body style='width:100%;padding:1px 4px;text-align:left;font-family:monospace'><pre>" + a.toString

        panel
      }
    }
  }

  var rid = 0

  val JsonInput = new TextArea { input =>
    preferredSize = new Dimension(400,250)
    font = new Font("Monospaced", AWTFont.PLAIN, 12)
    reactions += {
      case KeyPressed(_, Key.Enter, mods, _) =>
        if ((mods & Key.Modifier.Shift) == Key.Modifier.Shift) {
          JsonOutput.listData = Waiting(rid) +: JsonOutput.listData
          JsonOutput.repaint()
          rid += 1
        }
    }
    listenTo(keys)
  }

  val json =
    """
      |{
      |  "request_id": 1,
      |  "test": "one",
      |  "foo": "two"
      |}""".stripMargin

  JsonOutput.listData = List(Waiting(2), ErrorMessage(1, json), Output(0, json))

//  val JsonOutput = new GridBagPanel {
//    val bp = new BorderPanel {
//      preferredSize = new Dimension(300, 60)
//      add(new Label(json), BorderPanel.Position.Center)
//    }
//    layout(bp) = (0,0)
//  }

  def top = new MainFrame {
    title = "Quanto Console"
    contents = new GridPanel(2,1) {
      vGap = 3
      hGap = 3
      contents += new ScrollPane(JsonInput)
      contents += new ScrollPane(JsonOutput)
    }
  }
}
