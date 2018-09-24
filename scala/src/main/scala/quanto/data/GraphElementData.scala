package quanto.data

import quanto.util.json.JsonObject

/**
  * An abstract class providing a general interface for accessing
  * information contained in its different components
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/GraphElementData.scala Source code]]
  * @see Known Subclasses below
  * @author Aleks Kissinger
  */
abstract class GraphElementData {
  def theory: Theory

  def data: JsonObject

  def annotation: JsonObject

  def toJson: JsonObject = JsonObject("data" -> data, "annotation" -> annotation).noEmpty
}
