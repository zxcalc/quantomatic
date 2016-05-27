package quanto.data

import quanto.util.json._

class SynthLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

case class EqClass(rep: Graph, redexes : Map[String, Graph], congs : Map[String, Graph], data : String)

object EqClass {
  def fromJson(json: Json, theory: Theory) = {
    new EqClass(
      rep = Graph.fromJson(json / "rep", theory),
      redexes = (json / "redexes").mapValue.mapValues(Graph.fromJson(_, theory)),
      congs = (json / "congs").mapValue.mapValues(Graph.fromJson(_, theory)),
      data = (json / "data").stringValue
    )
  }

  def toJson(cls: EqClass, theory: Theory) = {
    JsonObject(
      "rep" -> Graph.toJson(cls.rep, theory),
      "redexes" -> JsonObject(cls.redexes.mapValues(Graph.toJson(_, theory))),
      "congs" -> JsonObject(cls.congs.mapValues(Graph.toJson(_, theory))),
      "data" -> JsonString(cls.data)
    )
  }
}

case class Synth(classes: Vector[EqClass] = Vector[EqClass]())

object Synth {
  def fromJson(json: Json, theory: Theory) = {
    val classes : Vector[EqClass] =
      try {
        (json / "classes").vectorValue.map(EqClass.fromJson(_, theory))
      } catch {
        case e: JsonAccessException =>
          throw new SynthLoadException(e.getMessage, e)
        case e: Exception =>
          e.printStackTrace()
          throw new SynthLoadException("Unexpected error reading JSON", e)
      }
    new Synth(classes)
  }

  def toJson(synth: Synth, theory: Theory) =
    JsonObject("classes" -> JsonArray(synth.classes.map { cls => EqClass.toJson(cls, theory) }))
}
