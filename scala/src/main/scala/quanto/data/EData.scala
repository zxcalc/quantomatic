package quanto.data

import quanto.util.json.JsonObject

abstract class EData(val directed: Boolean) {
  def data: JsonObject
  def annotation: JsonObject
}

case class DirEdge(data: JsonObject = JsonObject(), annotation: JsonObject = JsonObject()) extends EData(true)
case class UndirEdge(data: JsonObject = JsonObject(), annotation: JsonObject = JsonObject()) extends EData(false)