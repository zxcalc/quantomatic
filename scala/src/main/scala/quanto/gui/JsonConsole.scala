package quanto.gui

import swing._
import event.{Key, KeyPressed}
import java.awt.{Font => AWTFont, Color}
import javax.swing.border.LineBorder
import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import duration._
import quanto.core._
import quanto.util.json._
import akka.util.Timeout
import ExecutionContext.Implicits.global

sealed abstract class CoreOutputItem { def rid: Int }
case class Waiting(rid: Int) extends CoreOutputItem {
  override def toString = "waiting for request " + rid + "..."
}

case class ErrorMessage(rid: Int, output: String) extends CoreOutputItem {
  override def toString = output
}
case class Output(rid: Int, output: String) extends CoreOutputItem {
  override def toString = output
}

/*
EXAMPLES:

{
"controller": "!!",
"module": "system",
"function": "version"
}

{
"controller": "!!",
"module": "system",
"function": "help",
"input": {
  "controller": "red_green",
  "module": "test",
  "function": "echo"
}
}

{
"controller": "red_green",
"module": "test",
"function": "echo",
"input": {
  "foo": "bar",
  "baz": 12
}
}

{
"controller": "red_green",
"module": "test",
"function": "diverge"
}

{
"controller": "!!",
"module": "system",
"function": "kill",
"input": { "job": 8 }
}

 */

object JsonConsole extends SimpleSwingApplication {
  val sys = ActorSystem("QuantoConsole")
  val core = sys.actorOf(Props { new Core }, "core")
  implicit val timeout = Timeout(1.day)

  def appendResult(out: CoreOutputItem) {
    JsonOutput.listData = JsonOutput.listData.map {
      case Waiting(requestId) => if (requestId == out.rid) out
                                 else Waiting(requestId)
      case x => x
    }
    JsonOutput.repaint()
  }

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

  var outputSlot = 0

  val JsonInput = new TextArea { input =>
    preferredSize = new Dimension(400,250)
    font = new Font("Monospaced", AWTFont.PLAIN, 12)
    reactions += {
      case KeyPressed(_, Key.Enter, mods, _) =>
        if ((mods & Key.Modifier.Shift) == Key.Modifier.Shift) {
          val i = outputSlot
          JsonOutput.listData = Waiting(i) +: JsonOutput.listData

          try {
            val json = Json.parse(input.text)
            val future = core ? JsonRequest(json)
            future.map { x =>
              val out = x match {
                case Success(resp) => Output(i, resp.toString)
                case Error(code, message) => ErrorMessage(i, "Error " + code + ": " + message)
              }

              Swing.onEDT { appendResult(out) }
            }
          } catch {
            case e : JsonParseException => Swing.onEDT { appendResult(ErrorMessage(i, "Parse error")) }
          }

          JsonOutput.repaint()
          outputSlot += 1
        }
    }
    listenTo(keys)
  }

  JsonOutput.listData = List()

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

