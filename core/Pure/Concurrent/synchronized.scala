/*  Title:      Pure/Concurrent/synchronized.scala
    Module:     PIDE
    Author:     Makarius

Synchronized variables.
*/

package isabelle


import scala.annotation.tailrec


object Synchronized
{
  def apply[A](init: A): Synchronized[A] = new Synchronized(init)
}


final class Synchronized[A] private(init: A)
{
  /* state variable */

  private var state: A = init

  def value: A = synchronized { state }
  override def toString: String = value.toString


  /* synchronized access */

  def timed_access[B](time_limit: A => Option[Time], f: A => Option[(B, A)]): Option[B] =
    synchronized {
      def check(x: A): Option[B] =
        f(x) match {
          case None => None
          case Some((y, x1)) =>
            state = x1
            notifyAll()
            Some(y)
        }
      @tailrec def try_change(): Option[B] =
      {
        val x = state
        check(x) match {
          case None =>
            time_limit(x) match {
              case Some(t) =>
                val timeout = (t - Time.now()).ms
                if (timeout > 0L) {
                  wait(timeout)
                  check(state)
                }
                else None
              case None =>
                wait()
                try_change()
            }
          case some => some
        }
      }
      try_change()
    }

  def guarded_access[B](f: A => Option[(B, A)]): B =
    timed_access(_ => None, f).get


  /* unconditional change */

  def change(f: A => A): Unit = synchronized { state = f(state); notifyAll() }

  def change_result[B](f: A => (B, A)): B = synchronized {
    val (result, new_state) = f(state)
    state = new_state
    notifyAll()
    result
  }
}
