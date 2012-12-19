package quanto.data

import collection.immutable.TreeSet

// basically a map, but with cached inverse images
class PFun[A,B]
  (f : Map[A,B], finv: Map[B,TreeSet[A]])
  (implicit keyOrd: Ordering[A])
extends BinRel[A,B] {

  def domf = f.mapValues(Set(_))

  def codf = finv.withDefaultValue(Set[A]())

  def +(kv: (A,B)) : PFun[A,B] = {
    val finv1 =
      (f.get(kv._1) match {
        case Some(oldV) => if (finv(oldV).size == 1) finv - oldV
        else finv + (oldV -> (finv(oldV) - kv._1))
        case None => finv
      }) + (kv._2 -> (finv.getOrElse(kv._2, TreeSet[A]()) + kv._1))
    new PFun(f + kv,finv1)
  }

  def -(kv: (A, B)) = f.get(kv._1) match {
    case Some(v) if v == kv._2 =>
      val domSet = finv(v)
      new PFun[A,B](
        f - kv._1,
        if (domSet.size == 1) finv - kv._2 else finv + (kv._2 -> (domSet - kv._1))
      )
    case Some(_) => this
    case None => this
  }

  def unmapDom(k: A) = {
    val v = f(k)
    val domSet = finv(v)
    new PFun[A,B](
      f - k,
      if (domSet.size == 1) finv - v else finv + (v -> (domSet - k))
    )
  }

  def unmapCod(v: B) = {
    val domSet = finv(v)
    new PFun[A,B](
      domSet.foldLeft(f) { _ - _ },
      finv - v
    )
  }

  def iterator = f.iterator

  def toMap = f

  // children have the option to override this to give a default value
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

  def get(k: A) = f.get(k)
  def apply(k: A) = f.get(k) match {
    case Some(v) => v
    case None => default(k)
  }

  // PFun inherits equality from its member "f"
//  override def hashCode = f.hashCode()
//
//  override def canEqual(other: Any) = other match {
//    case that: PFun[_,_] => true
//    case _ => false
//  }
//
//  override def equals(other: Any) = other match {
//    case that: PFun[_,_] => (that canEqual this) && (this.toMap == that.toMap)
//    case _ => false
//  }
}

object PFun {
  def apply[A,B](kvs: (A,B)*)(implicit keyOrd: Ordering[A]) : PFun[A,B] = {
    kvs.foldLeft(new PFun[A,B](Map(),Map())){ (pf: PFun[A,B], kv: (A,B)) => pf + kv }
  }
}

