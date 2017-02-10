/*  Title:      Pure/GUI/jfx_gui.scala
    Author:     Makarius

Basic GUI tools (for Java FX).
*/

package isabelle


import java.io.{FileInputStream, BufferedInputStream}

import javafx.application.{Platform => JFX_Platform}
import javafx.scene.text.{Font => JFX_Font}


object JFX_GUI
{
  /* evaluation within the Java FX application thread */

  object Thread
  {
    def assert() = Predef.assert(JFX_Platform.isFxApplicationThread())
    def require() = Predef.require(JFX_Platform.isFxApplicationThread())

    def later(body: => Unit)
    {
      if (JFX_Platform.isFxApplicationThread()) body
      else JFX_Platform.runLater(new Runnable { def run = body })
    }

    def future[A](body: => A): Future[A] =
    {
      if (JFX_Platform.isFxApplicationThread()) Future.value(body)
      else {
        val promise = Future.promise[A]
        later { promise.fulfill_result(Exn.capture(body)) }
        promise
      }
    }
  }


  /* Isabelle fonts */

  def install_fonts()
  {
    for (font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS"))) {
      val stream = new BufferedInputStream(new FileInputStream(font.file))
      try { JFX_Font.loadFont(stream, 1.0) }
      finally { stream.close }
    }
  }

}
