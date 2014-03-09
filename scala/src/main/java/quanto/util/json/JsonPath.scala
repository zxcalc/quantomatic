package quanto.util.json

import util.parsing.combinator._

class JsonPathParseException(msg: String) extends JsonParseException(msg)

case class JsonPath(components: List[JsonPath.Component]) {
  import JsonPath._

  override def toString =
    components.foldLeft("$") {
      case (s,Field(f)) => s + "." + f
      case (s,Index(i)) => s + "[" + i + "]"
    }

  private def _get(json: Json, comps: List[Component]): Json = comps match {
    case (Field(f) :: cs) => json.get(f) match { case Some(j) => _get(j, cs); case None => JsonNull }
    case (Index(i) :: cs) => json.get(i) match { case Some(j) => _get(j, cs); case None => JsonNull }
    case _ => json
  }

  private def _update(json: Json, comps: List[Component], fn: Json => Json): Json = comps match {
    case (Field(f) :: cs) =>
      val map = json.asObject.mapValue
      JsonObject(map + (f -> _update(map.getOrElse(f, JsonNull), cs, fn)))
    case (Index(i) :: cs) =>
      val vect = json.asArray.vectorValue.padTo(i+1, JsonNull)
      JsonArray(vect.updated(i, _update(vect(i), cs, fn)))
    case _ => fn(json)
  }

  // returns the Json object associated with the given path. If an undefined field is encountered,
  // null is returned. An exception is thrown if a json element of the wrong type is in the way.
  def get(json: Json) = _get(json, components)

  // sets or updates Json. If the path doesn't exist, it will try to create it by adding new fields and creating or
  // padding arrays to the appropriate size. An exception is thrown if a json element of the wrong type is in the way.
  def set(json: Json, x: Json): Json = _update(json, components, (_ => x))
  def update(json: Json)(fn: Json => Json): Json = _update(json, components, fn)
}

object JsonPath {
  sealed abstract class Component
  case class Field(f: String) extends Component
  case class Index(i: Int) extends Component

  private object JsonPathParser extends RegexParsers {
    // tokens
    val PField = """\.([^.\[\]]+)""".r
    val PIndex = """\[([0-9]+)\]""".r

    // grammar
    def field = PField ^^ { case PField(f) => Field(f) }
    def index = PIndex ^^ { case PIndex(i) => Index(i.toInt) }
    def expr  = "$" ~> rep(field | index )

    def apply(s: String) = parseAll(expr, s) match {
      case Success(r, _) => r
      case failure: NoSuccess => throw new JsonPathParseException(failure.msg)
    }
  }

  def apply(path: String): JsonPath = new JsonPath(JsonPathParser(path))
}
