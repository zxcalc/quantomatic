/** A package which contains the data objects */
package quanto.data

import scala.collection.immutable.TreeSet
import scala.collection.{IterableLike, mutable}

/**
  * A trait which contains useful methods for binary relations
  *
  * @tparam A type of the domain
  * @tparam B type of the codomain
  * @author Aleks Kissinger
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/BinRel.scala Source code]]
  */
trait BinRel[A, B] extends IterableLike[(A, B), BinRel[A, B]] {
  /** Domain function - assigns a set to each element of the domain */
  def domf: Map[A, Set[B]]

  /** Codomain function - assigns a set to each element of the codomain */
  def codf: Map[B, Set[A]]

  /**
    * Add an element to relation
    *
    * @param kv element to be added
    * @return New relation containing '''kv''' in addition to the current one
    */
  def +(kv: (A, B)): BinRel[A, B]

  /**
    * Check if an element is in a relation
    *
    * @param kv element to be checked
    * @return boolean
    */
  def contains(kv: (A, B)): Boolean

  /**
    * Remove an element from relation
    *
    * @param kv element to be removed
    * @return New relation containing the same elements as the
    *         current one except '''kv'''
    */
  def unmap(kv: (A, B)): BinRel[A, B]

  def -(kv: (A, B)): BinRel[A, B] = unmap(kv)

  /**
    * Remove all relation pairs '''(d,_)'''
    *
    * @param d Element to be removed from domain
    * @return New relation with relation pairs '''(d,_)''' removed
    */
  def unmapDom(d: A): BinRel[A, B]

  /**
    * Remove all relation pairs '''(_,c)'''
    *
    * @param c Element to be removed from codomain
    * @return New relation with relation pairs '''(_,c)''' removed
    */
  def unmapCod(c: B): BinRel[A, B]

  /** The domain set of the relation */
  def dom: Iterable[A] = domf.keys

  def domSet: Set[A] = domf.keySet

  /** The codomain set of the relation */
  def cod: Iterable[B] = codf.keys

  def codSet: Set[B] = codf.keySet

  /**
    * The codomain image of a set of domain elements under this relation
    *
    * @param set A set containing elements of type '''A'''
    * @return The set of codomain elements which are in relation with some
    *         element from '''set'''
    */
  def directImage(set: Set[A]): Set[B] = set.foldLeft(Set[B]()) { (s, x) => s union domf.getOrElse(x, Set()) }

  /**
    * The domain image of a set of codomain elements under this relation
    *
    * @param set A set containing elements of type '''B'''
    * @return The set of domain elements which are in relation with some
    *         element from '''set'''
    */
  def inverseImage(set: Set[B]): Set[A] = set.foldLeft(Set[A]()) { (s, y) => s union codf.getOrElse(y, Set()) }


  /**
    * BinRel inherits equality from '''domf'''
    *
    * @return The hashcode of '''domf'''
    */
  override def hashCode: Int = domf.hashCode()

  /** BinRel inherits equality from '''domf''' */
  override def equals(other: Any): Boolean = other match {
    case that: BinRel[_, _] => (that canEqual this) && (this.domf == that.domf)
    case _ => false
  }

  /** True iff '''other''' is of type '''BinRel[_,_]''' */
  override def canEqual(other: Any): Boolean = other match {
    case _: BinRel[_, _] => true
    case _ => false
  }

  override def toString: String = {
    getClass.getSimpleName + "(" + seq.map { case (k, v) => k.toString + " -> " + v.toString }.mkString(", ") + ")"
  }

  // TODO: get ++ implemented correctly (i.e. using builders etc) for PFun/BinRel
  def ++(r: BinRel[A, B]): BinRel[A, B] = r.foldLeft(this) { case (mp, kv) => mp + kv }
}

/**
  * A class which represents a binary relation as a pair of two functions -
  * the domain map and the codomain map
  *
  * @tparam A The type of the elements in the domain of the relation
  * @tparam B The type of the elements in the codomain of the relation
  * @constructor Create an instance of the class from two functions mapping
  *              elements to sets of elements
  * @param domMap The domain map - maps an element of type '''A''' to the
  *               set of elements of type '''B''' which are in relation with it
  * @param codMap The codomain map - maps an element of type '''B''' to the
  *               set of elements of type '''A''' which are in relation with it
  * @author Aleks Kissinger
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/BinRel.scala Source code]]
  */
class MapPairBinRel[A, B](domMap: Map[A, TreeSet[B]], codMap: Map[B, TreeSet[A]])
                         (implicit domOrd: Ordering[A], codOrd: Ordering[B])
  extends BinRel[A, B] {

  def +(kv: (A, B)): MapPairBinRel[A, B] = {
    new MapPairBinRel[A, B](
      domMap + (kv._1 -> (domMap.getOrElse(kv._1, TreeSet()(codOrd)) + kv._2)),
      codMap + (kv._2 -> (codMap.getOrElse(kv._2, TreeSet()(domOrd)) + kv._1))
    )
  }

  def contains(kv: (A, B)) : Boolean = domf.get(kv._1).contains(kv._2)

  def unmapDom(d: A): MapPairBinRel[A, B] = domf(d).foldLeft(this) { (rel, c) => rel unmap(d, c) }

  def domf : Map[A, TreeSet[B]] = domMap.withDefaultValue(TreeSet()(codOrd))

  def unmap(kv: (A, B)): MapPairBinRel[A, B] = {
    new MapPairBinRel[A, B](
      domMap.get(kv._1) match {
        case Some(xs) if xs.size == 1 && xs.contains(kv._2) => domMap - kv._1
        case Some(xs) => domMap + (kv._1 -> (xs - kv._2))
        case None => domMap
      },

      codMap.get(kv._2) match {
        case Some(xs) if xs.size == 1 && xs.contains(kv._1) => codMap - kv._2
        case Some(xs) => codMap + (kv._2 -> (xs - kv._1))
        case None => codMap
      }
    )
  }

  def unmapCod(c: B): MapPairBinRel[A, B] = codf(c).foldLeft(this) { (rel, d) => rel unmap(d, c) }

  def codf: Map[B, TreeSet[A]] = codMap.withDefaultValue(TreeSet()(domOrd))

  def seq: Seq[(A, B)] = iterator.toSeq

  /** Returns an iterator over pairs '''(a,b)''' of the relation */
  def iterator: Iterator[(A, B)] = domMap.foldLeft(Iterator[(A, B)]()) { case (iter, (domElement, codSet)) =>
    iter ++ (Iterator.continually(domElement) zip codSet.iterator)
  }

  protected[this] def newBuilder : mutable.Builder[(A, B), BinRel[A, B]] = new mutable.Builder[(A, B), BinRel[A, B]] {
    val s: mutable.Buffer[(A, B)] = collection.mutable.Buffer[(A, B)]()

    def result() = BinRel(s: _*)

    def clear(): Unit = s.clear()

    def +=(elem: (A, B)): this.type = {
      s += elem
      this
    }
  }
}

/**
  * Companion object for the BinRel trait
  *
  * @author Aleks Kissinger
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/BinRel.scala Source code]]
  */
object BinRel {
  /** Construct a binary relation from a sequence of pairs '''(a,b)''' */
  def apply[A, B](kvs: (A, B)*)(implicit domOrd: Ordering[A], codOrd: Ordering[B]): BinRel[A, B] = {
    kvs.foldLeft(new MapPairBinRel[A, B](Map(), Map())(domOrd, codOrd)) { (rel, kv) => rel + kv }
  }
}
