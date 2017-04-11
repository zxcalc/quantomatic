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

  protected val factory = new Match(_,_,_,_,_,_,_)

  def copy(pattern: Graph = this.pattern,
           target: Graph = this.target,
           vmap: PFun[VName,VName] = this.vmap,
           emap: PFun[EName,EName] = this.emap,
           bbmap: PFun[BBName,BBName] = this.bbmap,
           subst: Map[String,AngleExpression] = this.subst): Match =
    factory(pattern,target,vmap,emap,bbmap,bbops,subst)
}
