/*  Title:      Pure/Concurrent/mailbox.scala
    Module:     PIDE
    Author:     Makarius

Message exchange via mailbox, with multiple senders (non-blocking,
unbounded buffering) and single receiver (bulk messages).
*/

package isabelle


object Mailbox
{
  def apply[A]: Mailbox[A] = new Mailbox[A]()
}


class Mailbox[A] private()
{
  private val mailbox = Synchronized(List.empty[A])
  override def toString: String = mailbox.value.reverse.mkString("Mailbox(", ",", ")")

  def send(msg: A): Unit = mailbox.change(msg :: _)

  def receive(timeout: Option[Time]): List[A] =
    (mailbox.timed_access(_ => timeout.map(t => Time.now() + t),
      { case Nil => None case msgs => Some((msgs, Nil)) }) getOrElse Nil).reverse

  def await_empty: Unit =
    mailbox.guarded_access({ case Nil => Some(((), Nil)) case _ => None })
}
