package quanto.data

import quanto.util.json.JsonString

import scala.collection._
import quanto.util.StringNamer




trait Name[This <: Name[This]] extends Ordered[This] {
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

case class GName(s: String) extends Name[GName] { protected val mk = GName(_) }
case class VName(s: String) extends Name[VName] { protected val mk = VName(_) }
case class EName(s: String) extends Name[EName] { protected val mk = EName(_) }
case class BBName(s: String) extends Name[BBName] { protected val mk = BBName(_) }
case class DSName(s: String) extends Name[DSName] { protected val mk = DSName(_) }

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

//  class NamePFun[N <: Name[N], T](val pf: PFun[N,T]) {
//    def fresh(implicit default: N) : N = if (pf.isEmpty) default else pf.dom.max.succ
//    def freshWithSuggestion(s : N) : N = {
//      val set = pf.domSet
//      var t = s
//      while (set.contains(t)) t = t.succ
//      t
//    }
//  }

  // TODO: overkill with implicits?

  implicit def setToNameSet[N <: Name[N]](set : Set[N]):NameSet[N] =
    new NameSet(set)
  implicit def mapToNameMap[N <: Name[N], T](map : Map[N,T]):NameMap[N,T] =
    new NameMap(map)

  // these support general-purpose string-for-name substitution
  implicit def stringToGName(s: String): GName  = GName(s)
  implicit def stringToVName(s: String): VName  = VName(s)
  implicit def stringToEName(s: String): EName  = EName(s)
  implicit def stringToBBName(s: String): BBName = BBName(s)
  implicit def stringToDSName(s: String): DSName = DSName(s)

  implicit def stringSetToGNameSet(set: Set[String]): Set[GName] = set map GName.apply
  implicit def stringSetToVNameSet(set: Set[String]): Set[VName] = set map VName.apply
  implicit def stringSetToENameSet(set: Set[String]): Set[EName] = set map EName.apply
  implicit def stringSetToBBNameSet(set: Set[String]): Set[BBName] = set map BBName.apply
  implicit def stringSetToDSNameSet(set: Set[String]): Set[DSName] = set map DSName.apply

  // edge creation methods take a pair of vertices
  implicit def stringPairToVNamePair(t: (String,String)): (VName, VName) = (VName(t._1), VName(t._2))

  // these can be used to save names into JSON without conversion
  implicit def gNameToJsonString(n: GName): JsonString = quanto.util.json.JsonString(n.toString)
  implicit def vNameToJsonString(n: VName): JsonString = quanto.util.json.JsonString(n.toString)
  implicit def eNameToJsonString(n: EName): JsonString = quanto.util.json.JsonString(n.toString)
  implicit def bbNameToJsonString(n: BBName): JsonString = quanto.util.json.JsonString(n.toString)

  implicit val defaultVName = VName("v0")
  implicit val defaultEName = EName("e0")
  implicit val defaultGName = GName("g0")
  implicit val defaultBBName = BBName("bx0")
  implicit val defaultDSName = DSName("0")
}
