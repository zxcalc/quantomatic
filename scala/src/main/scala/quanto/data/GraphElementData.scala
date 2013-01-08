package quanto.data

import quanto.util.json.JsonObject


abstract class GraphElementData {
  def data: JsonObject
  def annotation: JsonObject
  def json: JsonObject = JsonObject("data" -> data, "annotation" -> annotation).noEmpty
}
