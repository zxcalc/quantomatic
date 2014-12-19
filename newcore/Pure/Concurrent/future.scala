/*  Title:      Pure/Concurrent/future.scala
    Module:     PIDE
    Author:     Makarius

Value-oriented parallel execution via futures and promises in Scala -- with
signatures as in Isabelle/ML.
*/

package isabelle


import scala.util.{Success, Failure}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor,
  Future => Scala_Future, Promise => Scala_Promise, Await}
import scala.concurrent.duration.Duration


object Future
{
  lazy val execution_context: ExecutionContextExecutor =
    ExecutionContext.fromExecutorService(Simple_Thread.default_pool)

  def value[A](x: A): Future[A] = new Finished_Future(x)

  def fork[A](body: => A): Future[A] =
    new Pending_Future(Scala_Future[A](body)(execution_context))

  def promise[A]: Promise[A] =
    new Promise_Future[A](Scala_Promise[A])
}

trait Future[A]
{
  def peek: Option[Exn.Result[A]]
  def is_finished: Boolean = peek.isDefined
  def get_finished: A = { require(is_finished); Exn.release(peek.get) }
  def join: A
  def map[B](f: A => B): Future[B] = Future.fork { f(join) }

  override def toString =
    peek match {
      case None => "<future>"
      case Some(Exn.Exn(_)) => "<failed>"
      case Some(Exn.Res(x)) => x.toString
    }
}

trait Promise[A] extends Future[A]
{
  def fulfill_result(res: Exn.Result[A]): Unit
  def fulfill(x: A): Unit
}


private class Finished_Future[A](x: A) extends Future[A]
{
  val peek: Option[Exn.Result[A]] = Some(Exn.Res(x))
  val join: A = x
}

private class Pending_Future[A](future: Scala_Future[A]) extends Future[A]
{
  def peek: Option[Exn.Result[A]] =
    future.value match {
      case Some(Success(x)) => Some(Exn.Res(x))
      case Some(Failure(e)) => Some(Exn.Exn(e))
      case None => None
    }
  override def is_finished: Boolean = future.isCompleted

  def join: A = Await.result(future, Duration.Inf)
  override def map[B](f: A => B): Future[B] =
    new Pending_Future[B](future.map(f)(Future.execution_context))
}

private class Promise_Future[A](promise: Scala_Promise[A])
  extends Pending_Future(promise.future) with Promise[A]
{
  override def is_finished: Boolean = promise.isCompleted

  def fulfill_result(res: Exn.Result[A]): Unit =
    res match {
      case Exn.Res(x) => promise.success(x)
      case Exn.Exn(e) => promise.failure(e)
    }
  def fulfill(x: A): Unit = promise.success(x)
}

