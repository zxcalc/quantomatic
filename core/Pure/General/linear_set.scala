/*  Title:      Pure/General/linear_set.scala
    Module:     PIDE
    Author:     Makarius
    Author:     Fabian Immler, TU Munich

Sets with canonical linear order, or immutable linked-lists.
*/

package isabelle


import scala.collection.SetLike
import scala.collection.generic.{SetFactory, CanBuildFrom, GenericSetTemplate, GenericCompanion}
import scala.collection.mutable.{Builder, SetBuilder}
import scala.language.higherKinds


object Linear_Set extends SetFactory[Linear_Set]
{
  private val empty_val: Linear_Set[Nothing] = new Linear_Set[Nothing](None, None, Map(), Map())
  override def empty[A] = empty_val.asInstanceOf[Linear_Set[A]]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Linear_Set[A]] = setCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Linear_Set[A]] = new SetBuilder[A, Linear_Set[A]](empty[A])

  class Duplicate[A](x: A) extends Exception
  class Undefined[A](x: A) extends Exception
  class Next_Undefined[A](x: Option[A]) extends Exception
}


final class Linear_Set[A] private(
    start: Option[A], end: Option[A], val nexts: Map[A, A], prevs: Map[A, A])
  extends scala.collection.immutable.Set[A]
  with GenericSetTemplate[A, Linear_Set]
  with SetLike[A, Linear_Set[A]]
{
  override def companion: GenericCompanion[Linear_Set] = Linear_Set


  /* relative addressing */

  def next(elem: A): Option[A] =
    if (contains(elem)) nexts.get(elem)
    else throw new Linear_Set.Undefined(elem)

  def prev(elem: A): Option[A] =
    if (contains(elem)) prevs.get(elem)
    else throw new Linear_Set.Undefined(elem)

  def get_after(hook: Option[A]): Option[A] =
    hook match {
      case None => start
      case Some(elem) => next(elem)
    }

  def insert_after(hook: Option[A], elem: A): Linear_Set[A] =
    if (contains(elem)) throw new Linear_Set.Duplicate(elem)
    else
      hook match {
        case None =>
          start match {
            case None => new Linear_Set[A](Some(elem), Some(elem), Map(), Map())
            case Some(elem1) =>
              new Linear_Set[A](Some(elem), end,
                nexts + (elem -> elem1), prevs + (elem1 -> elem))
          }
        case Some(elem1) =>
          if (!contains(elem1)) throw new Linear_Set.Undefined(elem1)
          else
            nexts.get(elem1) match {
              case None =>
                new Linear_Set[A](start, Some(elem),
                  nexts + (elem1 -> elem), prevs + (elem -> elem1))
              case Some(elem2) =>
                new Linear_Set[A](start, end,
                  nexts + (elem1 -> elem) + (elem -> elem2),
                  prevs + (elem2 -> elem) + (elem -> elem1))
            }
      }

  def append_after(hook: Option[A], elems: Seq[A]): Linear_Set[A] =  // FIXME reverse fold
    ((hook, this) /: elems) {
      case ((last, set), elem) => (Some(elem), set.insert_after(last, elem))
    }._2

  def delete_after(hook: Option[A]): Linear_Set[A] =
    hook match {
      case None =>
        start match {
          case None => throw new Linear_Set.Next_Undefined[A](None)
          case Some(elem1) =>
            nexts.get(elem1) match {
              case None => empty
              case Some(elem2) =>
                new Linear_Set[A](Some(elem2), end, nexts - elem1, prevs - elem2)
            }
        }
      case Some(elem1) =>
        if (!contains(elem1)) throw new Linear_Set.Undefined(elem1)
        else
          nexts.get(elem1) match {
            case None => throw new Linear_Set.Next_Undefined(Some(elem1))
            case Some(elem2) =>
              nexts.get(elem2) match {
                case None =>
                  new Linear_Set[A](start, Some(elem1), nexts - elem1, prevs - elem2)
                case Some(elem3) =>
                  new Linear_Set[A](start, end,
                    nexts - elem2 + (elem1 -> elem3),
                    prevs - elem2 + (elem3 -> elem1))
              }
          }
    }


  /* Set methods */

  override def stringPrefix = "Linear_Set"

  override def isEmpty: Boolean = !start.isDefined
  override def size: Int = if (isEmpty) 0 else nexts.size + 1

  def contains(elem: A): Boolean =
    !isEmpty && (end.get == elem || nexts.isDefinedAt(elem))

  private def make_iterator(from: Option[A]): Iterator[A] = new Iterator[A] {
    private var next_elem = from
    def hasNext(): Boolean = next_elem.isDefined
    def next(): A =
      next_elem match {
        case Some(elem) =>
          next_elem = nexts.get(elem)
          elem
        case None => Iterator.empty.next()
      }
  }

  override def iterator: Iterator[A] = make_iterator(start)

  def iterator(elem: A): Iterator[A] =
    if (contains(elem)) make_iterator(Some(elem))
    else throw new Linear_Set.Undefined(elem)

  def iterator(from: A, to: A): Iterator[A] =
    if (contains(to))
      nexts.get(to) match {
        case None => iterator(from)
        case Some(stop) => iterator(from).takeWhile(_ != stop)
      }
    else throw new Linear_Set.Undefined(to)

  def reverse: Linear_Set[A] = new Linear_Set(end, start, prevs, nexts)

  override def last: A = reverse.head

  def + (elem: A): Linear_Set[A] = insert_after(end, elem)

  def - (elem: A): Linear_Set[A] = delete_after(prev(elem))
}
