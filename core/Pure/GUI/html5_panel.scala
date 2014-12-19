/*  Title:      Pure/GUI/html5_panel.scala
    Module:     PIDE-GUI
    Author:     Makarius

HTML5 panel based on Java FX WebView.
*/

package isabelle


import javafx.scene.Scene
import javafx.scene.web.{WebView, WebEngine}
import javafx.scene.input.KeyEvent
import javafx.scene.text.FontSmoothingType
import javafx.scene.layout.{HBox, VBox, Priority}
import javafx.geometry.{HPos, VPos, Insets}
import javafx.event.EventHandler


// see http://netbeans.org/bugzilla/show_bug.cgi?id=210414
// and http://hg.netbeans.org/jet-main/rev/a88434cec458
private class Web_View_Workaround extends javafx.scene.layout.Pane
{
  VBox.setVgrow(this, Priority.ALWAYS)
  HBox.setHgrow(this, Priority.ALWAYS)

  setMaxWidth(java.lang.Double.MAX_VALUE)
  setMaxHeight(java.lang.Double.MAX_VALUE)

  val web_view = new WebView
  web_view.setMinSize(500, 400)
  web_view.setPrefSize(500, 400)

  getChildren().add(web_view)

  override protected def layoutChildren()
  {
    val managed = getManagedChildren()
    val width = getWidth()
    val height = getHeight()
    val top = getInsets().getTop()
    val right = getInsets().getRight()
    val left = getInsets().getLeft()
    val bottom = getInsets().getBottom()

    for (i <- 0 until managed.size)
      layoutInArea(managed.get(i), left, top,
        width - left - right, height - top - bottom,
        0, Insets.EMPTY, true, true, HPos.CENTER, VPos.CENTER)
  }
}


class HTML5_Panel extends javafx.embed.swing.JFXPanel
{
  private val future =
    JFX_Thread.future {
      val pane = new Web_View_Workaround

      val web_view = pane.web_view
      web_view.setFontSmoothingType(FontSmoothingType.GRAY)
      web_view.setOnKeyTyped(new EventHandler[KeyEvent] {
        def handle(e: KeyEvent) {
          if (e.isControlDown && e.getCharacter == "0")
            web_view.setFontScale(1.0)
          if (e.isControlDown && e.getCharacter == "+")
            web_view.setFontScale(web_view.getFontScale * 1.1)
          else if (e.isControlDown && e.getCharacter == "-")
            web_view.setFontScale(web_view.getFontScale / 1.1)
        }
      })

      setScene(new Scene(pane))
      pane
    }

  def web_view: WebView = future.join.web_view
  def web_engine: WebEngine = web_view.getEngine
}
