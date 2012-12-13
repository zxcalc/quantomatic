package quanto.data

import org.codehaus.jackson._

class QGraphJsonException(message: String, cause: Throwable = null)
extends Exception(message, cause)

case class QGraph(
  name: GName                     = Names.defaultGName,
  data: Unit                      = (),
  verts: Map[VName,VData]         = Map[VName,VData](),
  edges: Map[EName,EData]         = Map[EName,EData](),
  source: PFun[EName,VName]       = PFun[EName,VName](),
  target: PFun[EName,VName]       = PFun[EName,VName](),
  bboxes: Map[BBName,Unit]        = Map[BBName,Unit](),
  inBBox: PFun[VName,BBName]      = PFun[VName,BBName](),
  bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
extends GraphLike[Unit,VData,EData,Unit,QGraph]
{
  protected val factory = new QGraph(_,_,_,_,_,_,_,_,_)
}

object QGraph {
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (QGraph, Name[N])) : QGraph = t._1

  def apply(json: JsonNode) = {
    try {
      if (!json.isObject) throw new QGraphJsonException("Expected: JSON object")
      val wireVerts = json.get("verts") match {case null => None; case v => Some(v)}
    } catch {
      case e: GraphException => throw new QGraphJsonException("Error when updating graph", e)
    }
  }
}