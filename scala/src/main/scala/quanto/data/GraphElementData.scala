package quanto.data

import quanto.util.json.JsonObject


abstract class GraphElementData {
  def theory: Theory
  def data: JsonObject
  def annotation: JsonObject
  def toJson: JsonObject = JsonObject("data" -> data, "annotation" -> annotation).noEmpty
}
