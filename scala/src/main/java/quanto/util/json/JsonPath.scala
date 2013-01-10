package quanto.util.json

class JsonPath(val components: List[JsonPath.Component]) {
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
  case class Field(f: String) extends Component {
    override def toString = f
  }
  case class Index(i: Int) extends Component {
    override def toString = "[" + i + "]"
  }

  private val PFieldAndIndex = "([^.]+)\\[([0-9]+)\\]".r
  private val PIndex = "\\[([0-9]+)\\]".r

  def apply(path: String): JsonPath = new JsonPath(
    path.split('.').foldRight(List[Component]()) {
      case (PFieldAndIndex(f,i), list) => Field(f) :: Index(i.toInt) :: list
      case (PIndex(i), list) => Index(i.toInt) :: list
      case (f, list) => Field(f) :: list
    })
}
