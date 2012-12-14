package quanto.util.json

import org.codehaus.jackson.{JsonParser,JsonGenerator,JsonFactory,JsonEncoding,JsonToken}

abstract class JsonException(message: String)
  extends Exception(message)

// thrown if the user tries to access some part of the tree incorrectly (i.e. with a bad key, index, or type)
class JsonAccessException(message: String)
  extends JsonException(message)

// thrown if a problem is encountered while parsing the JSON
class JsonParseException(message: String)
  extends JsonException(message)

sealed abstract class Json {
  def writeTo(out: Json.Output)

  def writeTo(out: java.io.OutputStream) {
    val jsonOut = new Json.Output(out)
    writeTo(jsonOut)
    jsonOut.flush()
  }

  def writeTo(out: java.io.Writer) {
    val jsonOut = new Json.Output(out)
    writeTo(jsonOut)
    jsonOut.flush()
  }

  def writeTo(f: java.io.File) {
    val jsonOut = new Json.Output(f)
    writeTo(jsonOut)
    jsonOut.flush()
  }

  protected def jsonString = {
    val sw = new java.io.StringWriter
    writeTo(sw)
    sw.toString
  }

  // Convenience coercions for objects and arrays. These are not type-safe, so better to
  // use match-case if they might fail.
  def asObject = this match {
    case x: JsonObject => x
    case _ => throw new JsonAccessException("Expected: JsonObject, got: " + this.getClass)
  }

  def asArray = this match {
    case x: JsonArray => x
    case _ => throw new JsonAccessException("Expected: JsonArray, got: " + this.getClass)
  }

  // Value accessors. throw JsonException if the cast fails.
  def boolValue: Boolean = this match {
    case JsonBool(b) => b
    case _ => throw new JsonAccessException("Expected: JsonBool, got: " + this.getClass)
  }

  def intValue: Int = this match {
    case JsonInt(i) => i
    case _ => throw new JsonAccessException("Expected: JsonInt, got: " + this.getClass)
  }

  // note that integers are treated as a sub-type of doubles. This is the expected behaviour 99% of the time.
  def doubleValue: Double = this match {
    case JsonDouble(d) => d
    case JsonInt(i) => i.toDouble
    case _ => throw new JsonAccessException("Expected: JsonDouble or JsonInt, got: " + this.getClass)
  }

  def stringValue: String = this match {
    case JsonString(s) => s
    case _ => throw new JsonAccessException("Expected: JsonString, got: " + this.getClass)
  }

  override def toString = jsonString
}

case class JsonObject(v: Map[String,Json] = Map[String,Json]()) extends Json
with Iterable[(String,Json)]
{
  def +(kv:(String,Json)) = v + kv
  def iterator = v.iterator
  def writeTo(out: Json.Output) {
    out.g.writeStartObject()
    for ((k,json) <- v) {
      out.g.writeFieldName(k)
      json.writeTo(out)
    }
    out.g.writeEndObject()
  }
  def apply(key: String) = v.get(key) match {
    case Some(x) => x
    case None    => throw new JsonAccessException("Key not found: " + key)
  }

  override def toString() = jsonString
}

object JsonObject { def apply(kv: (String,Json)*): JsonObject = JsonObject(Map(kv: _*)) }

case class JsonArray(v: Vector[Json] = Vector[Json]()) extends Json
with Iterable[Json]
{
  def :+(x:Json) = v :+ x
  def iterator = v.iterator
  def writeTo(out: Json.Output) {
    out.g.writeStartArray()
    for (json <- v) json.writeTo(out)
    out.g.writeEndArray()
  }

  def apply(index: Int) =
    try { v(index) }
    catch { case _: IndexOutOfBoundsException =>
              throw new JsonAccessException("Index: " + index + " out of bounds") }

  override def toString() = jsonString
}

object JsonArray { def apply(v: Json*): JsonArray = JsonArray(Vector(v: _*)) }

case class JsonNull() extends Json {
  val v = null
  def writeTo(out: Json.Output) { out.g.writeNull() }
}

case class JsonString(v: String) extends Json {
  def writeTo(out: Json.Output) { out.g.writeString(v) }
}

case class JsonInt(v: Int) extends Json {
  def writeTo(out: Json.Output) { out.g.writeNumber(v) }
}

case class JsonDouble(v: Double) extends Json {
  def writeTo(out: Json.Output) { out.g.writeNumber(v) }
}

case class JsonBool(v: Boolean) extends Json {
  def writeTo(out: Json.Output) { out.g.writeBoolean(v) }
}

object Json {
  lazy protected val factory = new JsonFactory()

  class Input(val p: JsonParser) {
    p.enable(JsonParser.Feature.AUTO_CLOSE_SOURCE)

    def this(s: String) = this(factory.createJsonParser(s))
    def this(f: java.io.File) =         this(factory.createJsonParser(f))
    def this(in: java.io.InputStream) = this(factory.createJsonParser(in))
    def this(in: java.io.Reader) =      this(factory.createJsonParser(in))

    def close() { p.close() }
  }

  class Output(val g: JsonGenerator) {
    g.enable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)

    def this(out: java.io.OutputStream) = this(factory.createJsonGenerator(out, JsonEncoding.UTF8))
    def this(out: java.io.Writer) =       this(factory.createJsonGenerator(out))
    def this(f: java.io.File) =           this(factory.createJsonGenerator(f, JsonEncoding.UTF8))

    def flush() { g.flush() }
    def close() { g.close() }
  }

  def parse(s: String): Json = parse(new Input(s))
  def parse(f: java.io.File): Json = parse(new Input(f))

  def parse(jsonInput: Input): Json = {
    val p = jsonInput.p
    val stack = collection.mutable.Stack[(Json,Option[String])]()

    p.nextToken match {
      case JsonToken.START_ARRAY => stack.push((JsonArray(), None))
      case JsonToken.START_OBJECT => stack.push((JsonObject(), None))
      case tok => throw new JsonParseException("First token must open array or object, got: " + tok)
    }

    var nextJson: Option[Json] = None
    while (!stack.isEmpty) {
      nextJson = p.nextToken() match {
        case JsonToken.START_ARRAY => stack.push((JsonArray(),None)); None
        case JsonToken.START_OBJECT => stack.push((JsonObject(),None)); None
        case JsonToken.FIELD_NAME => stack.push((stack.pop()._1, Some(p.getText))); None
        case JsonToken.NOT_AVAILABLE => throw new JsonParseException("Next token not available")
        case JsonToken.VALUE_EMBEDDED_OBJECT => throw new JsonParseException("Embedded objects not supported")
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


