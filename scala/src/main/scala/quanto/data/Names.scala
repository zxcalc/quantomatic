package quanto.data
import scala.collection._

class DuplicateNameException(ty: String, name: String)
extends Exception("Duplicate " + ty + " name: '" + name + "'")

trait HasName {
  def name : String
}

trait NameAndData[D] extends HasName {
  def data : D
}

trait NameUtil {
  protected def succ(s: String) = {
    val last = s.charAt(s.length - 1)
    if ('0' <= last && last < '9')
      s.substring(0, s.length - 1) + ((last + 1) toChar)
    else s + "0"
  }
}

class NameSet(val set: Set[String]) extends NameUtil {
  def fresh(default: String = "n0") : String = if (set.isEmpty) default else succ (set max)
}

class NameMap[T](val map: Map[String,T]) extends NameUtil {
  def fresh(default: String = "n0") : String = if (map.isEmpty) default else succ (map.keys max)
}

object Names {
  implicit def setToNameSet(set : Set[String]):NameSet =
    new NameSet(set)
  implicit def mapToNameMap[T](map : Map[String,T]):NameMap[T] =
    new NameMap(map)
}
