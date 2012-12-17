package quanto.data

import quanto.util.json.JsonObject

case class GData(data: JsonObject = JsonObject(), annotation: JsonObject = JsonObject()) extends GraphElementData
