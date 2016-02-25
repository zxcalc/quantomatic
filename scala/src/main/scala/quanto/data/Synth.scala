package quanto.data

import quanto.util.json.{JsonAccessException, Json}

class SynthLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

class EqClass(json : Json, theory: Theory) {
  val rep = Graph.fromJson(json / "rep", theory)
  val redexes = (json / "redexes").mapValue.mapValues(Graph.fromJson(_, theory))
  val congs = (json / "congs").mapValue.mapValues(Graph.fromJson(_, theory))
  val data = (json / "data").stringValue
}

case class Synth(classes: Vector[EqClass] = Vector[EqClass]())

object Synth {
  def fromJson(json: Json, theory: Theory) = {
    val classes : Vector[EqClass] =
      try {
        (json / "classes").vectorValue.map(new EqClass(_, theory))
      } catch {
        case e: JsonAccessException =>
          throw new SynthLoadException(e.getMessage, e)
        case e: Exception =>
          e.printStackTrace()
          throw new SynthLoadException("Unexpected error reading JSON", e)
      }
    new Synth(classes)
  }
}
