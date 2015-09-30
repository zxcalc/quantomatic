/*  Title:      Pure/GUI/system_dialog.scala
    Author:     Makarius

Dialog for system processes, with optional output window.
*/

package isabelle


import java.awt.{GraphicsEnvironment, Point, Font}
import javax.swing.WindowConstants
import java.io.{File => JFile, BufferedReader, InputStreamReader}

import scala.swing.{ScrollPane, Button, CheckBox, FlowPanel,
  BorderPanel, Frame, TextArea, Component, Label}
import scala.swing.event.ButtonClicked


class System_Dialog extends Build.Progress
{
  /* component state -- owned by GUI thread */

  private var _title = "Isabelle"
  private var _window: Option[Window] = None
  private var _return_code: Option[Int] = None

  private def check_window(): Window =
  {
    GUI_Thread.require {}

    _window match {
      case Some(window) => window
      case None =>
        val window = new Window
        _window = Some(window)

        window.pack()
        val point = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint()
        window.location =
          new Point(point.x - window.size.width / 2, point.y - window.size.height / 2)
        window.visible = true

        window
      }
  }

  private val result = Future.promise[Int]

  private def conclude()
  {
    GUI_Thread.require {}
    require(_return_code.isDefined)

    _window match {
      case None =>
      case Some(window) =>
        window.visible = false
        window.dispose
        _window = None
    }

    try { result.fulfill(_return_code.get) }
    catch { case ERROR(_) => }
  }

  def join(): Int = result.join
  def join_exit(): Nothing = sys.exit(join)


  /* window */

  private class Window extends Frame
  {
    title = _title
    peer.setIconImages(GUI.isabelle_images())


    /* text */

    val text = new TextArea {
      editable = false
      columns = 65
      rows = 24
    }

    val scroll_text = new ScrollPane(text)


    /* layout panel with dynamic actions */

    val action_panel = new FlowPanel(FlowPanel.Alignment.Center)()
    val layout_panel = new BorderPanel
    layout_panel.layout(scroll_text) = BorderPanel.Position.Center
    layout_panel.layout(action_panel) = BorderPanel.Position.South

    contents = layout_panel

    def set_actions(cs: Component*)
    {
      action_panel.contents.clear
      action_panel.contents ++= cs
      layout_panel.revalidate
      layout_panel.repaint
    }


    /* close */

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation {
      if (_return_code.isDefined) conclude()
      else stopping()
    }

    def stopping()
    {
      is_stopped = true
      set_actions(new Label("Stopping ..."))
    }

    val stop_button = new Button("Stop") {
      reactions += { case ButtonClicked(_) => stopping() }
    }

    var do_auto_close = true
    def can_auto_close: Boolean = do_auto_close && _return_code == Some(0)

    val auto_close = new CheckBox("Auto close") {
      reactions += {
        case ButtonClicked(_) => do_auto_close = this.selected
        if (can_auto_close) conclude()
      }
    }
    auto_close.selected = do_auto_close
    auto_close.tooltip = "Automatically close dialog when finished"

    set_actions(stop_button, auto_close)


    /* exit */

    val delay_exit =
      GUI_Thread.delay_first(Time.seconds(1.0))
      {
        if (can_auto_close) conclude()
        else {
          val button =
            new Button(if (_return_code == Some(0)) "OK" else "Exit") {
              reactions += { case ButtonClicked(_) => conclude() }
            }
          set_actions(button)
          button.peer.getRootPane.setDefaultButton(button.peer)
        }
      }
  }


  /* progress operations */

  def title(txt: String): Unit =
    GUI_Thread.later {
      _title = txt
      _window.foreach(window => window.title = txt)
    }

  def return_code(rc: Int): Unit =
    GUI_Thread.later {
      _return_code = Some(rc)
      _window match {
        case None => conclude()
        case Some(window) => window.delay_exit.invoke
      }
    }

  override def echo(txt: String): Unit =
    GUI_Thread.later {
      val window = check_window()
      window.text.append(txt + "\n")
      val vertical = window.scroll_text.peer.getVerticalScrollBar
      vertical.setValue(vertical.getMaximum)
    }

  override def theory(session: String, theory: String): Unit =
    echo(session + ": theory " + theory)

  @volatile private var is_stopped = false
  override def stopped: Boolean = is_stopped


  /* system operations */

  def execute(cwd: JFile, env: Map[String, String], args: String*): Int =
  {
    val proc = Isabelle_System.raw_execute(cwd, env, true, args: _*)
    proc.getOutputStream.close

    val stdout = new BufferedReader(new InputStreamReader(proc.getInputStream, UTF8.charset))
    try {
      var line = stdout.readLine
      while (line != null) {
        echo(line)
        line = stdout.readLine
      }
    }
    finally { stdout.close }

    proc.waitFor
  }
}

