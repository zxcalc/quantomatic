package quanto.util.json

import org.codehaus.jackson.{JsonParser,JsonGenerator,JsonFactory,JsonEncoding,JsonToken}

class JsonException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

sealed abstract class Json {
  def writeToGenerator(gen: JsonGenerator)

  def writeTo(out: java.io.OutputStream) {
    val gen = Json.factory.createJsonGenerator(out, JsonEncoding.UTF8)
    writeToGenerator(gen)
    gen.flush()
  }

  def writeTo(out: java.io.Writer) {
    val gen = Json.factory.createJsonGenerator(out)
    writeToGenerator(gen)
    gen.flush()
  }

  def writeTo(f: java.io.File) {
    val gen = Json.factory.createJsonGenerator(f, JsonEncoding.UTF8)
    writeToGenerator(gen)
    gen.flush()
  }

  protected def jsonString = {
    val sw = new java.io.StringWriter
    writeTo(sw)
    sw.toString
  }

  // Value accessors. throw JsonException if the cast fails. These are not type-safe, so better to
  // use match-case where applicable.
  def boolValue: Boolean = this match {
    case JsonBool(b) => b
    case _ => throw new JsonException("Expected: JsonBool, got: " + this.getClass)
  }

  def intValue: Int = this match {
    case JsonInt(i) => i
    case _ => throw new JsonException("Expected: JsonInt, got: " + this.getClass)
  }

  // note that integers are treated as a sub-type of doubles. This is the expected behaviour 99% of the time.
  def doubleValue: Double = this match {
    case JsonDouble(d) => d
    case JsonInt(i) => i.toDouble
    case _ => throw new JsonException("Expected: JsonDouble or JsonInt, got: " + this.getClass)
  }

  def stringValue: String = this match {
    case JsonString(s) => s
    case _ => throw new JsonException("Expected: JsonString, got: " + this.getClass)
  }

  override def toString = jsonString
}

case class JsonObject(v: Map[String,Json] = Map[String,Json]()) extends Json
with Iterable[(String,Json)]
{
  def +(kv:(String,Json)) = v + kv
  def iterator = v.iterator
  def writeToGenerator(gen: JsonGenerator) {
    gen.writeStartObject()
    for ((k,json) <- v) {
      gen.writeFieldName(k)
      json.writeToGenerator(gen)
    }
    gen.writeEndObject()
  }
  def apply(k: String) = v.get(k) match {
    case Some(x) => x
    case None    => throw new JsonException("Key not found: " + k)
  }

  override def toString() = jsonString
}

object JsonObject { def apply(kv: (String,Json)*): JsonObject = JsonObject(Map(kv: _*)) }

case class JsonArray(v: Vector[Json] = Vector[Json]()) extends Json
with Iterable[Json]
{
  def :+(x:Json) = v :+ x
  def iterator = v.iterator
  def writeToGenerator(gen: JsonGenerator) {
    gen.writeStartArray()
    for (json <- v) json.writeToGenerator(gen)
    gen.writeEndArray()
  }

  override def toString() = jsonString
}

object JsonArray { def apply(v: Json*): JsonArray = JsonArray(Vector(v: _*)) }

case class JsonNull() extends Json {
  val v = null
  def writeToGenerator(gen: JsonGenerator) { gen.writeNull() }
}

case class JsonString(v: String) extends Json {
  def writeToGenerator(gen: JsonGenerator) { gen.writeString(v) }
}

case class JsonInt(v: Int) extends Json {
  def writeToGenerator(gen: JsonGenerator) { gen.writeNumber(v) }
}

case class JsonDouble(v: Double) extends Json {
  def writeToGenerator(gen: JsonGenerator) { gen.writeNumber(v) }
}

case class JsonBool(v: Boolean) extends Json {
  def writeToGenerator(gen: JsonGenerator) { gen.writeBoolean(v) }
}

object Json {
  lazy protected val factory = new JsonFactory()

  def parse(s: String): Json               = parse(factory.createJsonParser(s))
  def parse(f: java.io.File): Json         = parse(factory.createJsonParser(f))
  def parse(in: java.io.InputStream): Json = parse(factory.createJsonParser(in))
  def parse(in: java.io.Reader): Json      = parse(factory.createJsonParser(in))

  def parse(p: JsonParser): Json = {
    val stack = collection.mutable.Stack[(Json,Option[String])]()
    var node: Json = null

    p.nextToken match {
      case JsonToken.START_ARRAY => stack.push((JsonArray(), None))
      case JsonToken.START_OBJECT => stack.push((JsonObject(), None))
      case _ => throw new JsonException("First token must open array or object")
    }

    var nextJson: Option[Json] = None
    while (!stack.isEmpty) {
      nextJson = p.nextToken() match {
        case JsonToken.START_ARRAY => stack.push((JsonArray(),None)); None
        case JsonToken.START_OBJECT => stack.push((JsonObject(),None)); None
        case JsonToken.FIELD_NAME => stack.push((stack.pop()._1, Some(p.getText))); None
        case JsonToken.NOT_AVAILABLE => throw new JsonException("Next token not available")
        case JsonToken.VALUE_EMBEDDED_OBJECT => throw new JsonException("Embedded objects not supported")
        case JsonToken.END_ARRAY => Some(stack.pop()._1)
        case JsonToken.END_OBJECT => Some(stack.pop()._1)
        case JsonToken.VALUE_FALSE => Some(JsonBool(false))
        case JsonToken.VALUE_TRUE => Some(JsonBool(true))
        case JsonToken.VALUE_NULL => Some(JsonNull())
        case JsonToken.VALUE_NUMBER_FLOAT => Some(JsonDouble(p.getValueAsDouble))
        case JsonToken.VALUE_NUMBER_INT => Some(JsonInt(p.getValueAsInt))
        case JsonToken.VALUE_STRING => Some(JsonString(p.getText))
      }

      nextJson.map(item => {
        if (!stack.isEmpty) stack.push(stack.pop() match {
          case (JsonObject(obj), Some(field)) => (JsonObject(obj + (field -> item)), None)
          case (JsonArray(arr), _) => (JsonArray(arr :+ item), None)
          case x => x
        })
      })
    }

    nextJson.get
  }

  // implicit conversions to simplify working with JSON trees
  implicit def stringToJson(x: String): JsonString = JsonString(x)
  implicit def boolToJson(x: Boolean): JsonBool = JsonBool(x)
  implicit def intToJson(x: Int): JsonInt = JsonInt(x)
  implicit def doubleToJson(x: Double): JsonDouble = JsonDouble(x)

  // tuple implicit conversions, useful for JsonObject(k -> v, ...) construction
  implicit def stringStringToStringJson(t: (String,String)) = (t._1, JsonString(t._2))
  implicit def stringBoolToStringJson(t: (String,Boolean)) = (t._1, JsonBool(t._2))
  implicit def stringIntToStringJson(t: (String,Int)) = (t._1, JsonInt(t._2))
  implicit def stringDoubleToStringJson(t: (String,Double)) = (t._1, JsonDouble(t._2))
}

// these are not active by default, as they are not type-safe
object JsonValues {
  implicit def jsonToBool(j: Json): Boolean = j.boolValue
  implicit def jsonToInt(j: Json): Int = j.intValue
  implicit def jsonToDouble(j: Json): Double = j.doubleValue
  implicit def jsonToString(j: Json): String = j.stringValue
}


