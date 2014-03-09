package quanto.data
import scala.collection._
import quanto.util.StringNamer


abstract class Name[This <: Name[This]] extends Ordered[This] {
  def succ: This
}


abstract class StrName[This <: StrName[This]] extends Name[This] {
  val s: String
  protected val mk: String => This

  val (prefix, suffix) = {
    var intIndex = s.length
    while (intIndex > 0 && s.charAt(intIndex - 1) >= '0' && s.charAt(intIndex - 1) <= '9') intIndex -= 1

    // preserve leading zeros
    while (intIndex < s.length - 1 && s.charAt(intIndex) == '0') intIndex += 1

    (
      s.substring(0, intIndex),
      if (intIndex == s.length) -1 else s.substring(intIndex, s.length).toInt
    )
  }

  def compare(that: This) = if (prefix < that.prefix) -1
                            else if (prefix > that.prefix) 1
                            else suffix compare that.suffix

  def succ: This = mk(prefix + (suffix + 1))

  override def toString = s
}

case class GName(s: String) extends StrName[GName] { protected val mk = GName(_) }
case class VName(s: String) extends StrName[VName] { protected val mk = VName(_) }
case class EName(s: String) extends StrName[EName] { protected val mk = EName(_) }
case class BBName(s: String) extends StrName[BBName] { protected val mk = BBName(_) }
case class DSName(s: String) extends StrName[DSName] { protected val mk = DSName(_) }

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
    def fresh(implicit default: N) : N = if (set.isEmpty) default else set.max.succ
    def freshWithSuggestion(s : N) : N = { var t = s; while (set.contains(t)) t = t.succ; t }
  }

  class NameMap[N <: Name[N], T](val map: Map[N,T]) {
    def fresh(implicit default: N) : N = if (map.isEmpty) default else map.keys.max.succ
    def freshWithSuggestion(s : N) : N = {
      val set = map.keySet
      var t = s
      while (set.contains(t)) t = t.succ
      t
    }
  }

  class NamePFun[N <: Name[N], T](val pf: PFun[N,T]) {
    def fresh(implicit default: N) : N = if (pf.isEmpty) default else pf.dom.max.succ
    def freshWithSuggestion(s : N) : N = {
      val set = pf.domSet
      var t = s
      while (set.contains(t)) t = t.succ
      t
    }
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
  implicit def stringToDSName(s: String) = DSName(s)

  implicit def stringSetToGNameSet(set: Set[String]) = set map (GName(_))
  implicit def stringSetToVNameSet(set: Set[String]) = set map (VName(_))
  implicit def stringSetToENameSet(set: Set[String]) = set map (EName(_))
  implicit def stringSetToBBNameSet(set: Set[String]) = set map (BBName(_))
  implicit def stringSetToDSNameSet(set: Set[String]) = set map (DSName(_))

  // edge creation methods take a pair of vertices
  implicit def stringPairToVNamePair(t: (String,String)) = (VName(t._1), VName(t._2))

  // these can be used to save names into JSON without conversion
  implicit def gNameToJsonString(n: GName) = quanto.util.json.JsonString(n.toString)
  implicit def vNameToJsonString(n: VName) = quanto.util.json.JsonString(n.toString)
  implicit def eNameToJsonString(n: EName) = quanto.util.json.JsonString(n.toString)
  implicit def bbNameToJsonString(n: BBName) = quanto.util.json.JsonString(n.toString)

  implicit val defaultVName = VName("v0")
  implicit val defaultEName = EName("e0")
  implicit val defaultGName = GName("g0")
  implicit val defaultBBName = BBName("bx0")
  implicit val defaultDSName = DSName("0")
}
