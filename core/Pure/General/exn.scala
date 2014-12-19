/*  Title:      Pure/General/exn.scala
    Module:     PIDE
    Author:     Makarius

Support for exceptions (arbitrary throwables).
*/

package isabelle


object Exn
{
  /* exceptions as values */

  sealed abstract class Result[A]
  case class Res[A](res: A) extends Result[A]
  case class Exn[A](exn: Throwable) extends Result[A]

  def capture[A](e: => A): Result[A] =
    try { Res(e) }
    catch { case exn: Throwable => Exn[A](exn) }

  def release[A](result: Result[A]): A =
    result match {
      case Res(x) => x
      case Exn(exn) => throw exn
    }


  /* interrupts */

  def is_interrupt(exn: Throwable): Boolean =
  {
    var found_interrupt = false
    var e = exn
    while (!found_interrupt && e != null) {
      found_interrupt |= e.isInstanceOf[InterruptedException]
      e = e.getCause
    }
    found_interrupt
  }

  object Interrupt
  {
    def apply(): Throwable = new InterruptedException
    def unapply(exn: Throwable): Boolean = is_interrupt(exn)

    def expose() { if (Thread.interrupted()) throw apply() }
    def impose() { Thread.currentThread.interrupt }

    def postpone[A](body: => A): Option[A] =
    {
      val interrupted = Thread.interrupted
      val result = capture { body }
      if (interrupted) impose()
      result match {
        case Res(x) => Some(x)
        case Exn(e) =>
          if (is_interrupt(e)) { impose(); None }
          else throw e
      }
    }

    val return_code = 130
  }


  /* POSIX return code */

  def return_code(exn: Throwable, rc: Int): Int =
    if (is_interrupt(exn)) Interrupt.return_code else rc


  /* message */

  private val runtime_exception = Class.forName("java.lang.RuntimeException")

  def user_message(exn: Throwable): Option[String] =
    if (exn.isInstanceOf[java.io.IOException]) {
      val msg = exn.getMessage
      Some(if (msg == null) "I/O error" else "I/O error: " + msg)
    }
    else if (exn.getClass == runtime_exception) {
      val msg = exn.getMessage
      Some(if (msg == null || msg == "") "Error" else msg)
    }
    else if (exn.isInstanceOf[RuntimeException]) Some(exn.toString)
    else None

  def message(exn: Throwable): String =
    user_message(exn) getOrElse (if (is_interrupt(exn)) "Interrupt" else exn.toString)
}

