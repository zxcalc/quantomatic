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
    case (Field(f) :: cs) => _get(json.asObject(f), cs)
    case (Index(i) :: cs) => _get(json.asArray(i), cs)
    case _ => json
  }

  private def _update(json: Json, comps: List[Component], fn: Json => Json): Json = comps match {
    case (Field(f) :: cs) => JsonObject(json.mapValue + (f -> _update(json.asObject(f), cs, fn)))
    case (Index(i) :: cs) => JsonArray(json.vectorValue.updated(i, _update(json.asArray(i), cs, fn)))
    case _ => fn(json)
  }

  def get(json: Json) = _get(json, components)
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
