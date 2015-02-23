/*  Title:      Pure/GUI/jfx_thread.scala
    Module:     PIDE-GUI
    Author:     Makarius

Evaluation within the Java FX application thread.
*/

package isabelle

import javafx.application.{Platform => JFX_Platform}


object JFX_Thread
{
  /* checks */

  def assert() = Predef.assert(JFX_Platform.isFxApplicationThread())
  def require() = Predef.require(JFX_Platform.isFxApplicationThread())


  /* asynchronous context switch */

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
