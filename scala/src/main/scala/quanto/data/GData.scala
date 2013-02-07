package quanto.data

import quanto.util.json.JsonObject

/**
 * A class which represents graph data
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/GData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 */
case class GData(
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends GraphElementData
