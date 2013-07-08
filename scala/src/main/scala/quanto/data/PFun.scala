package quanto.data

import collection.immutable.TreeSet

/**
 * Basically a map, but with cached inverse images
 *
 * @tparam A type of domain elements
 * @tparam B type of codomain elements
 * 
 * @constructor Create a new instance by specifying the partial function and 
 * the inverse image function
 * @param f The partial function 
 * @param finv The inverse image function '''f ^-1^ '''
 * @param keyOrd Order on the domain elements (keys)
 *
 * @author Aleks Kissinger
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/PFun.scala Source code]]
 */
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

  def unmap(kv: (A, B)) = f.get(kv._1) match {
    case Some(v) if v == kv._2 =>
      val domSet = finv(v)
      new PFun[A,B](
        f - kv._1,
        if (domSet.size == 1) finv - kv._2 else finv + (kv._2 -> (domSet - kv._1))
      )
    case Some(_) => this
    case None => this
  }

  def unmapDom(k: A) :PFun[A,B] = {
    f.get(k) match {
      case None => this // do nothing
      case Some(v) =>
        val domSet = finv(v)
        new PFun[A,B](
          f - k,
          if (domSet.size == 1) finv - v else finv + (v -> (domSet - k))
        )
    }
  }

  def unmapCod(v: B) :PFun[A,B] = {
    finv.get(v) match {
      case None => this // do nothing
      case Some(domSet) =>
        new PFun[A,B](
          domSet.foldLeft(f) { _ - _ },
          finv - v
        )
    }
  }

  /** Same as '''unmapDom''' */
  def -(k: A) = unmapDom(k)

  /** Creates an iterator (same as f.iterator) */
  def iterator = f.iterator

  /** Returns '''f''' */
  def toMap = f

  /** 
   * Specifies the behaviour for elements of the domain where the 
   * function is not defined. Children have the option to override 
   * this to give a default value.
   *
   * @param key Domain element
   * @return Alway throws an exception
   * @throws NoSuchElementException Exception indicates which key is not found
   */
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

  /** 
   * Similar to '''apply''', but returns an option instead
   * 
   * @param k Domain element where function should be evaluated
   * @return Optionally returns the function value at '''k'''
   */
  def get(k: A) = f.get(k)

  /**
   * Get the value of the function at the specified domain element 
   * '''( F(k) )''' 
   *
   * @param k Domain element where function should be evaluated
   * @return Function value at '''k''' if it is defined there, otherwise
   * '''default(k)'''
   */
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

/**
 * Companion object for the PFun class
 *
 * @author Aleks Kissinger
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/PFun.scala Source code]]
 */
object PFun {
  /** Create an instance of PFun from a sequence of pairs */
  def apply[A,B](kvs: (A,B)*)(implicit keyOrd: Ordering[A]) : PFun[A,B] = {
    kvs.foldLeft(new PFun[A,B](Map(),Map())){ (pf: PFun[A,B], kv: (A,B)) => pf + kv }
  }
}
