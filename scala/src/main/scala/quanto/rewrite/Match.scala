package quanto.rewrite
import quanto.data._

sealed abstract class BBOp

case class Match(pattern: Graph,
                 target: Graph,
                 vmap: PFun[VName,VName] = PFun(),
                 emap: PFun[EName,EName] = PFun(),
                 bbmap: PFun[BBName,BBName] = PFun(),
                 bbops: List[BBOp] = List(),
                 subst: Map[String,AngleExpression] = Map()) {

  def addVertex(vPair: (VName, VName)): Match =
    copy(vmap = vmap + vPair)
  def addEdge(ePair: (EName, EName), vPair: (VName, VName)): Match =
    copy(vmap = vmap + vPair, emap = emap + ePair)
}
