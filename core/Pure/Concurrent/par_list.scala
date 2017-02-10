/*  Title:      Pure/Concurrent/par_list.scala
    Author:     Makarius

Parallel list combinators.
*/


package isabelle


object Par_List
{
  def managed_results[A, B](f: A => B, xs: List[A]): List[Exn.Result[B]] =
    if (xs.isEmpty || xs.tail.isEmpty) xs.map(x => Exn.capture { f(x) })
    else {
      val state = Synchronized[(List[Future[B]], Boolean)]((Nil, false))

      def cancel_other(self: Int = -1): Unit =
        state.change { case (futures, canceled) =>
          if (!canceled) {
            for ((future, i) <- futures.iterator.zipWithIndex if i != self)
              future.cancel
          }
          (futures, true)
        }

      try {
        state.change(_ =>
          (xs.iterator.zipWithIndex.map({ case (x, self) =>
            Future.fork {
              Exn.capture { f(x) } match {
                case Exn.Exn(exn) => cancel_other(self); throw exn
                case Exn.Res(res) => res
              }
            }
          }).toList, false))

        state.value._1.map(_.join_result)
      }
      finally { cancel_other() }
    }

  def map[A, B](f: A => B, xs: List[A]): List[B] =
    Exn.release_first(managed_results(f, xs))

  def get_some[A, B](f: A => Option[B], xs: List[A]): Option[B] =
  {
    class Found(val res: B) extends Exception
    val results =
      managed_results(
        (x: A) => f(x) match { case None => () case Some(y) => throw new Found(y) }, xs)
    results.collectFirst({ case Exn.Exn(found: Found) => found.res }) match {
      case None => Exn.release_first(results); None
      case some => some
    }
  }

  def find_some[A](P: A => Boolean, xs: List[A]): Option[A] =
    get_some((x: A) => if (P(x)) Some(x) else None, xs)

  def exists[A](P: A => Boolean, xs: List[A]): Boolean = find_some(P, xs).isDefined
  def forall[A](P: A => Boolean, xs: List[A]): Boolean = !exists((x: A) => !P(x), xs)
}
