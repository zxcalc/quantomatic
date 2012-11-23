package quanto.data

// basically a map, but with cached inverse images
class PFun[A,B](f : Map[A,B], finv: Map[B,Set[A]]) extends Iterable[(A,B)] {
  //  def this(kvs: (A,B)*) =
  //    this(
  //      Map(kvs),
  //      kvs.foldLeft(Map())(
  //        (kv: (A,B), m: Map[B,Set[A]]) => m + (m.getOrElse(kv._2, Set()) + kv)
  //      )
  //    )

  // children have the option to override this to give a default value
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)

  def -(k: A) = {
    val v = f(k)
    val finv1 =
      if (finv(v).size == 1) finv - v
      else finv + (v -> (finv(v) - k))
    new PFun[A,B](f - k,finv1)
  }

  def +(kv: (A,B)) : PFun[A,B] = {
    val finv1 =
      (f.get(kv._1) match {
        case Some(oldV) => if (finv(oldV).size == 1) finv - oldV
                           else finv + (oldV -> (finv(oldV) - kv._1))
        case None => finv
      }) + (kv._2 -> (finv.getOrElse(kv._2, Set()) + kv._1))
    new PFun(f + kv,finv1)
  }

  def get(k: A) = f.get(k)
  def apply(k: A) = f.get(k) match {
    case Some(v) => v
    case None => default(k)
  }
  def inv(v: B): Set[A] = {
    finv.get(v) match {
      case Some(s) => s
      case None => Set()
    }
  }

  def dom = f.keys

  def iterator = f.iterator
}

object PFun {
  def apply[A,B](kvs: (A,B)*) : PFun[A,B] = {
    kvs.foldLeft(new PFun[A,B](Map(),Map())){ (pf: PFun[A,B], kv: (A,B)) => pf + kv }
  }
}

