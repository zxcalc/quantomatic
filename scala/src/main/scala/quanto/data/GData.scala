package quanto.data

import quanto.util.json.JsonObject

case class GData(data: JsonObject, annotation: JsonObject) extends GraphElementData
