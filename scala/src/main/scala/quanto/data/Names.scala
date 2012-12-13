package quanto.data
import scala.collection._
import quanto.util.StringNamer


abstract class Name[N <: Name[N]] extends Ordered[N] {
  def succ: N
}

abstract class StrName[N <: StrName[N]] extends Name[N] {
  val s: String
  def compare(that: N) = s compare that.s
  def succStr : String = {
    val last = s.charAt(s.length - 1)
    if ('0' <= last && last < '9')
      s.substring(0, s.length - 1) + ((last + 1) toChar)
    else s + "0"
  }
  override def toString = s
}

case class GName(s: String) extends StrName[GName] {
  def succ = GName(succStr)
}
case class VName(s: String) extends StrName[VName] {
  def succ = VName(succStr)
}
case class EName(s: String) extends StrName[EName] {
  def succ = EName(succStr)
}
case class BBName(s: String) extends StrName[BBName] {
  def succ = BBName(succStr)
}

class DuplicateNameException[N <: Name[N]](ty: String, val name: N)
  extends Exception("Duplicate " + ty + " name: '" + name + "'")
class DuplicateVertexNameException(override val name: VName)
  extends DuplicateNameException("vertex", name)
class DuplicateEdgeNameException(override val name: EName)
  extends DuplicateNameException("edge", name)
class DuplicateBBoxNameException(override val name: BBName)
  extends DuplicateNameException("bang box", name)


object Names {
  class NameSet[N <: Name[N]](val set: Set[N]) {
    def fresh(implicit default: N) : N = if (set.isEmpty) default else (set max).succ
  }

  class NameMap[N <: Name[N], T](val map: Map[N,T]) {
    def fresh(implicit default: N) : N = if (map.isEmpty) default else (map.keys max).succ
  }

  class NamePFun[N <: Name[N], T](val pf: PFun[N,T]) {
    def fresh(implicit default: N) : N = if (pf.isEmpty) default else (pf.dom max).succ
  }

  implicit def setToNameSet[N <: Name[N]](set : Set[N]):NameSet[N] =
    new NameSet(set)
  implicit def mapToNameMap[N <: Name[N], T](map : Map[N,T]):NameMap[N,T] =
    new NameMap(map)

  // these support general-purpose string-for-name substitution
  implicit def stringToGName(s: String)  = GName(s)
  implicit def stringToVName(s: String)  = VName(s)
  implicit def stringToEName(s: String)  = EName(s)
  implicit def stringToBBName(s: String) = BBName(s)

  implicit def stringSetToGNameSet(set: Set[String]) = set map (GName(_))
  implicit def stringSetToVNameSet(set: Set[String]) = set map (VName(_))
  implicit def stringSetToENameSet(set: Set[String]) = set map (EName(_))
  implicit def stringSetToBBNameSet(set: Set[String]) = set map (BBName(_))

  // edge creation methods take a pair of vertices
  implicit def stringPairToVNamePair(t: (String,String)) = (VName(t._1), VName(t._2))


  implicit val defaultVName = VName("v0")
  implicit val defaultEName = EName("e0")
  implicit val defaultGName = GName("g0")
  implicit val defaultBBName = BBName("bb0")
}
