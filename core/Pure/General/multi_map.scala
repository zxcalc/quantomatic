/*  Title:      Pure/General/multi_map.scala
    Module:     PIDE
    Author:     Makarius

Maps with multiple entries per key.
*/

package isabelle


import scala.collection.generic.{ImmutableMapFactory, CanBuildFrom}


object Multi_Map extends ImmutableMapFactory[Multi_Map]
{
  private val empty_val: Multi_Map[Any, Nothing] = new Multi_Map[Any, Nothing](Map.empty)
  override def empty[A, B] = empty_val.asInstanceOf[Multi_Map[A, B]]

  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Multi_Map[A, B]] =
    new MapCanBuildFrom[A, B]
}


final class Multi_Map[A, +B] private(rep: Map[A, List[B]])
  extends scala.collection.immutable.Map[A, B]
  with scala.collection.immutable.MapLike[A, B, Multi_Map[A, B]]
{
  /* Multi_Map operations */

  def iterator_list: Iterator[(A, List[B])] = rep.iterator

  def get_list(a: A): List[B] = rep.getOrElse(a, Nil)

  def insert[B1 >: B](a: A, b: B1): Multi_Map[A, B1] =
  {
    val bs = get_list(a)
    if (bs.contains(b)) this
    else new Multi_Map(rep + (a -> (b :: bs)))
  }

  def remove[B1 >: B](a: A, b: B1): Multi_Map[A, B1] =
  {
    val bs = get_list(a)
    if (bs.contains(b)) {
      bs.filterNot(_ == b) match {
        case Nil => new Multi_Map(rep - a)
        case bs1 => new Multi_Map(rep + (a -> bs1))
      }
    }
    else this
  }


  /* Map operations */

  override def stringPrefix = "Multi_Map"

  override def empty = Multi_Map.empty
  override def isEmpty: Boolean = rep.isEmpty

  override def keySet: Set[A] = rep.keySet

  override def iterator: Iterator[(A, B)] =
    for ((a, bs) <- rep.iterator; b <- bs.iterator) yield (a, b)

  def get(a: A): Option[B] = get_list(a).headOption

  def + [B1 >: B](p: (A, B1)): Multi_Map[A, B1] = insert(p._1, p._2)

  def - (a: A): Multi_Map[A, B] =
    if (rep.isDefinedAt(a)) new Multi_Map(rep - a) else this
}
