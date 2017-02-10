/*  Title:      Pure/General/exn.scala
    Author:     Makarius

Support for exceptions (arbitrary throwables).
*/

package isabelle


object Exn
{
  /* user errors */

  class User_Error(message: String) extends RuntimeException(message)
  {
    override def equals(that: Any): Boolean =
      that match {
        case other: User_Error => message == other.getMessage
        case _ => false
      }
    override def hashCode: Int = message.hashCode

    override def toString: String = "ERROR(" + message + ")"
  }

  object ERROR
  {
    def apply(message: String): User_Error = new User_Error(message)
    def unapply(exn: Throwable): Option[String] = user_message(exn)
  }

  def error(message: String): Nothing = throw ERROR(message)

  def cat_message(msg1: String, msg2: String): String =
    if (msg1 == "") msg2
    else if (msg2 == "") msg1
    else msg1 + "\n" + msg2

  def cat_error(msg1: String, msg2: String): Nothing =
    error(cat_message(msg1, msg2))


  /* exceptions as values */

  sealed abstract class Result[A]
  {
    def user_error: Result[A] =
      this match {
        case Exn(ERROR(msg)) => Exn(ERROR(msg))
        case _ => this
      }
  }
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

  def release_first[A](results: List[Result[A]]): List[A] =
    results.find({ case Exn(exn) => !is_interrupt(exn) case _ => false }) match {
      case Some(Exn(exn)) => throw exn
      case _ => results.map(release(_))
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

    def expose() { if (Thread.interrupted) throw apply() }
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

  def user_message(exn: Throwable): Option[String] =
    if (exn.getClass == classOf[RuntimeException] ||
        exn.getClass == classOf[User_Error])
    {
      val msg = exn.getMessage
      Some(if (msg == null || msg == "") "Error" else msg)
    }
    else if (exn.isInstanceOf[java.sql.SQLException])
    {
      val msg = exn.getMessage
      Some(if (msg == null || msg == "") "SQL error" else msg)
    }
    else if (exn.isInstanceOf[java.io.IOException])
    {
      val msg = exn.getMessage
      Some(if (msg == null || msg == "") "I/O error" else "I/O error: " + msg)
    }
    else if (exn.isInstanceOf[RuntimeException]) Some(exn.toString)
    else None

  def message(exn: Throwable): String =
    user_message(exn) getOrElse (if (is_interrupt(exn)) "Interrupt" else exn.toString)
}
