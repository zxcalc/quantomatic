package quanto.data

import quanto.util.json.JsonObject

abstract class EData(val isDirected: Boolean) extends GraphElementData

case class DirEdge(data: JsonObject = JsonObject(), annotation: JsonObject = JsonObject()) extends EData(true)
case class UndirEdge(data: JsonObject = JsonObject(), annotation: JsonObject = JsonObject()) extends EData(false)