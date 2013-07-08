package quanto.data

import quanto.util.json.JsonObject

/**
 * A class which represents the data for bang boxes
 * 
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/BBData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @author Aleks Kissinger
 */
case class BBData(
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends GraphElementData
