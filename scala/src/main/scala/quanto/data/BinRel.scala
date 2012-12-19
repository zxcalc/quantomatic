package quanto.data

import collection.immutable.TreeSet


trait BinRel[A,B] extends Iterable[(A,B)] {
  def domf : Map[A,Set[B]]
  def codf : Map[B,Set[A]]
  def +(kv: (A,B)): BinRel[A,B]
  def -(kv: (A,B)): BinRel[A,B]
  def unmapDom(d: A): BinRel[A,B]
  def unmapCod(c: B): BinRel[A,B]

  def dom = domf.keys
  def cod = codf.keys
  def directImage(set: Set[A]) = set.foldLeft(Set[B]())(_ union domf(_))
  def inverseImage(set: Set[B]) = set.foldLeft(Set[A]())(_ union codf(_))

  // BinRel inherits equality from "domf"
  override def hashCode = domf.hashCode()

  override def canEqual(other: Any) = other match {
    case _: BinRel[_,_] => true
    case _ => false
  }

  override def equals(other: Any) = other match {
    case that: BinRel[_,_] => (that canEqual this) && (this.domf == that.domf)
    case _ => false
  }
}

class MapPairBinRel[A,B](domMap: Map[A,TreeSet[B]], val codMap: Map[B,TreeSet[A]])
  (implicit domOrd: Ordering[A], codOrd: Ordering[B])
  extends BinRel[A,B] {

  def domf = domMap.withDefaultValue(TreeSet()(codOrd))
  def codf = codMap.withDefaultValue(TreeSet()(domOrd))

  def +(kv: (A,B)) = {
    new MapPairBinRel[A,B](
      domMap + (kv._1 -> (domMap.getOrElse(kv._1, TreeSet()(codOrd)) + kv._2)),
      codMap + (kv._2 -> (codMap.getOrElse(kv._2, TreeSet()(domOrd)) + kv._1))
    )
  }

  def -(kv: (A,B)) = {
    new MapPairBinRel[A,B](
      domMap.get(kv._1) match {
        case Some(xs) if xs.size == 1 => domMap - kv._1
        case Some(xs) => domMap + (kv._1 -> (xs - kv._2))
        case None => domMap
      },

      codMap.get(kv._2) match {
        case Some(xs) if xs.size == 1 => codMap - kv._2
        case Some(xs) => codMap + (kv._2 -> (xs - kv._1))
        case None => codMap
      }
    )
  }

  def unmapDom(d: A) = domf(d).foldLeft(this) { (rel,c) => rel - (d -> c) }
  def unmapCod(c: B) = codf(c).foldLeft(this) { (rel,d) => rel - (d -> c) }

  def iterator = domMap.foldLeft(Iterator[(A,B)]()) { case (iter, (domElement, codSet)) =>
    iter ++ (Iterator.continually(domElement) zip codSet.iterator)
  }
}

object BinRel {
  def apply[A,B](kvs: (A,B)*)(implicit domOrd: Ordering[A], codOrd: Ordering[B]) : BinRel[A,B] = {
    kvs.foldLeft(new MapPairBinRel[A,B](Map(),Map())(domOrd,codOrd)){ (rel, kv) => rel + kv }
  }
}