package quanto.data


class QGraph(val name: GName,
             val verts: Map[VName,VData]         = Map[VName,VData](),
             val edges: Map[EName,Unit]          = Map[EName,Unit](),
             val source: PFun[EName,VName]       = PFun[EName,VName](),
             val target: PFun[EName,VName]       = PFun[EName,VName](),
             val bboxes: Map[BBName,Unit]        = Map[BBName,Unit](),
             val inBBox: PFun[VName,BBName]      = PFun[VName,BBName](),
             val bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
extends GraphLike[Unit,VData,Unit,Unit,QGraph]
{
  val data = ()
  def copy(name: GName                     = this.name,
           data: Unit                      = this.data,
           verts: Map[VName,VData]         = this.verts,
           edges: Map[EName,Unit]          = this.edges,
           source: PFun[EName,VName]       = this.source,
           target: PFun[EName,VName]       = this.target,
           bboxes: Map[BBName,Unit]        = this.bboxes,
           inBBox: PFun[VName,BBName]      = this.inBBox,
           bboxParent: PFun[BBName,BBName] = this.bboxParent): QGraph =
    new QGraph(name,verts,edges,source,target,bboxes,inBBox,bboxParent)
}

object QGraph {
  def apply(name: GName = Names.defaultGName) = new QGraph(name = name)
  // name can be ignored on methods that return a graph and a name
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (QGraph, Name[N])) : QGraph = t._1
}