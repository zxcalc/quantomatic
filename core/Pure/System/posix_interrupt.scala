/*  Title:      Pure/System/posix_interrupt.scala
    Author:     Makarius

Support for POSIX interrupts (bypassed on Windows).
*/

package isabelle


import sun.misc.{Signal, SignalHandler}


object POSIX_Interrupt
{
  def handler[A](h: => Unit)(e: => A): A =
  {
    val SIGINT = new Signal("INT")
    val new_handler = new SignalHandler { def handle(s: Signal) { h } }
    val old_handler = Signal.handle(SIGINT, new_handler)
    try { e } finally { Signal.handle(SIGINT, old_handler) }
  }

  def exception[A](e: => A): A =
  {
    val thread = Thread.currentThread
    handler { thread.interrupt } { e }
  }
}

