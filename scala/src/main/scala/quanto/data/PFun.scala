package quanto.data

import scala.collection.mutable._

// basically a map, but with cached inverse images
class PFun[A,B](kv: (A,B)*) extends Map[A,B] {
  val f = Map[A,B]()
  val finv = Map[B,Set[A]]()

  kv map (this += _)
  
  def -=(k: A) = {
    val v = f(k)
    if (finv(v).size == 1) finv -= v
    else finv(v) -= k
    f -= k
    this
  }
  
  def +=(kv: (A,B)) = {
    if (f contains kv._1)
      finv(f(kv._1)) -= kv._1
    f += kv
    if (finv contains kv._2) finv(kv._2) += kv._1
    else finv(kv._2) = Set(kv._1)
    this
  }
  
  def get(k: A) = f.get(k)
  
  def inv(v: B): Set[A] = {
    finv.get(v) match {
      case Some(s) => s
      case None => Set()
    }
  }
  
  def iterator = f.iterator
}

object PFun {
  def apply[A,B](kv: (A,B)*) : PFun[A,B] = new PFun(kv:_*)
}

