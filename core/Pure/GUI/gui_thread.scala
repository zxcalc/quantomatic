/*  Title:      Pure/GUI/gui_thread.scala
    Module:     PIDE-GUI
    Author:     Makarius

Evaluation within the GUI thread (for AWT/Swing).
*/

package isabelle


import javax.swing.SwingUtilities


object GUI_Thread
{
  /* context check */

  def assert[A](body: => A) =
  {
    Predef.assert(SwingUtilities.isEventDispatchThread())
    body
  }

  def require[A](body: => A) =
  {
    Predef.require(SwingUtilities.isEventDispatchThread())
    body
  }


  /* event dispatch queue */

  def now[A](body: => A): A =
  {
    if (SwingUtilities.isEventDispatchThread()) body
    else {
      lazy val result = { assert { Exn.capture(body) } }
      SwingUtilities.invokeAndWait(new Runnable { def run = result })
      Exn.release(result)
    }
  }

  def later(body: => Unit)
  {
    if (SwingUtilities.isEventDispatchThread()) body
    else SwingUtilities.invokeLater(new Runnable { def run = body })
  }


  /* delayed events */

  def delay_first(delay: => Time)(event: => Unit): Simple_Thread.Delay =
    Simple_Thread.delay_first(delay) { later { event } }

  def delay_last(delay: => Time)(event: => Unit): Simple_Thread.Delay =
    Simple_Thread.delay_last(delay) { later { event } }
}
