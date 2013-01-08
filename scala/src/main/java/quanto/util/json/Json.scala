package quanto.util.json

import org.codehaus.jackson.{JsonParser,JsonGenerator,JsonFactory,JsonEncoding,JsonToken}

abstract class JsonException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

// thrown if the user tries to access some part of the tree incorrectly (i.e. with a bad key, index, or type)
class JsonAccessException(message: String, val json: Json)
  extends JsonException(message)

// thrown if a problem is encountered while parsing the JSON
class JsonParseException(message: String, cause: org.codehaus.jackson.JsonParseException = null)
  extends JsonException(
    message + (if (cause != null) ": " + cause.getMessage else ""),
    cause)

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

  // permissive coercions for arrays and objects

  // treat string lists like {"a":{}, "b":{}, ...} and null like {}
  def asObject: JsonObject = this match {
    case obj: JsonObject => obj
    case JsonArray(x)    => x.foldLeft(JsonObject()) { (o,k) => o + (k.stringValue -> JsonObject()) }
    case JsonNull()      => JsonObject()
    case other           => throw new JsonAccessException("Expected: JsonObject, JsonArray, or JsonNull", other)
  }

  // treat objects like string lists of their keys and null like []
  def asArray: JsonArray = this match {
    case arr: JsonArray  => arr
    case JsonObject(x)   => JsonArray(Vector() ++ x.keys.map(JsonString(_)))
    case JsonNull()      => JsonArray()
    case other           => throw new JsonAccessException("Expected: JsonObject, JsonArray, or JsonNull", other)
  }

  // Convenience accessors for collections. These are overridden to not throw exceptions, where appropriate.
  def apply(index: Int): Json = get(index) match {
    case Some(x) => x
    case None    => throw new JsonAccessException("Index: " + index + " out of bounds", this)
  }

  def apply(key: String) = get(key) match {
    case Some(x) => x
    case None    => throw new JsonAccessException("Key not found: " + key, this)
  }

  def get(index: Int): Option[Json] =
    throw new JsonAccessException("Expected: JsonArray, got: " + this.getClass, this)

  def get(key: String): Option[Json] =
    throw new JsonAccessException("Expected: JsonObject, got: " + this.getClass, this)

  def getOrElse[B1 >: Json](index: Int, default: => B1): B1 =
    get(index) match { case Some(v) => v; case None => default }

  def getOrElse[B1 >: Json](key: String, default: => B1): B1 =
    get(key) match { case Some(v) => v; case None => default }


  // optional child notation
  def ?(key: String)    = getOrElse(key, JsonNull())

  // shorthand coercions for optional arrays and objects
  def ?@(key: String)   = (this ? key).asArray
  def ?#(key: String)   = (this ? key).asObject


  def mapValue: Map[String,Json] =
    throw new JsonAccessException("Expected: JsonObject, got: " + this.getClass, this)

  def vectorValue: Vector[Json] =
    throw new JsonAccessException("Expected: JsonArray, got: " + this.getClass, this)

  def boolValue: Boolean =
    throw new JsonAccessException("Expected: JsonBool, got: " + this.getClass, this)

  def intValue: Int =
    throw new JsonAccessException("Expected: JsonInt, got: " + this.getClass, this)

  def doubleValue: Double =
    throw new JsonAccessException("Expected: JsonDouble or JsonInt, got: " + this.getClass, this)

  def stringValue: String =
    throw new JsonAccessException("Expected: JsonString, got: " + this.getClass, this)

  override def toString = jsonString
}

case class JsonObject(v: Map[String,Json] = Map[String,Json]()) extends Json
with Iterable[(String,Json)]
{
  def +(kv:(String,Json)*) = JsonObject(kv.foldLeft(v){ _ + _ })
  def iterator = v.iterator
  def keysIterator = v.keysIterator
  def valuesIterator = v.valuesIterator
  def writeTo(out: Json.Output) {
    out.g.writeStartObject()
    for ((k,json) <- v) {
      out.g.writeFieldName(k)
      json.writeTo(out)
    }
    out.g.writeEndObject()
  }

  override def get(key: String) = v.get(key)
  override def getOrElse[B1 >: Json](key: String, default: => B1) = v.getOrElse[B1](key,default)
  override def mapValue = v
  override def toString() = jsonString
}

object JsonObject { def apply(kv: (String,Json)*): JsonObject = JsonObject(Map(kv: _*)) }

case class JsonArray(v: Vector[Json] = Vector[Json]()) extends Json
with Iterable[Json]
{
  def :+(x:Json*) = JsonArray(x.foldLeft(v){ _ :+ _ })
  def iterator = v.iterator
  def writeTo(out: Json.Output) {
    out.g.writeStartArray()
    for (json <- v) json.writeTo(out)
    out.g.writeEndArray()
  }

  override def vectorValue = v
  override def get(index: Int) =
    try   { Some(v(index)) }
    catch { case _: IndexOutOfBoundsException => None }

  override def toString() = jsonString
}

object JsonArray { def apply(v: Json*): JsonArray = JsonArray(Vector(v: _*)) }

case class JsonNull() extends Json {
  val v = null
  def writeTo(out: Json.Output) { out.g.writeNull() }
  override def get(key: String): Option[Json] = None
}

case class JsonString(v: String) extends Json {
  override def stringValue = v
  def writeTo(out: Json.Output) { out.g.writeString(v) }
}

case class JsonInt(v: Int) extends Json {
  override def intValue = v
  // note that integers are treated as a sub-type of doubles. This is the expected behaviour 99% of the time.
  override def doubleValue = v.toDouble
  def writeTo(out: Json.Output) { out.g.writeNumber(v) }
}

case class JsonDouble(v: Double) extends Json {
  override def doubleValue = v
  def writeTo(out: Json.Output) { out.g.writeNumber(v) }
}

case class JsonBool(v: Boolean) extends Json {
  override def boolValue = v
  def writeTo(out: Json.Output) { out.g.writeBoolean(v) }
}

object Json {
  lazy protected val factory = new JsonFactory()

  class Input(val p: JsonParser) {
    p.enable(JsonParser.Feature.AUTO_CLOSE_SOURCE)

    def this(s: String) =
      this(try { factory.createJsonParser(s) } catch {
        case e: org.codehaus.jackson.JsonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(f: java.io.File) =
      this(try { factory.createJsonParser(f) } catch {
        case e: org.codehaus.jackson.JsonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(in: java.io.InputStream) =
      this(try { factory.createJsonParser(in) } catch {
        case e: org.codehaus.jackson.JsonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(in: java.io.Reader) =
      this(try { factory.createJsonParser(in) } catch {
        case e: org.codehaus.jackson.JsonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

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
      try {
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
      } catch {
        case e: org.codehaus.jackson.JsonParseException =>
          throw new JsonParseException("Error while parsing", e)
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
  implicit def jsonToMap(j: Json): Map[String,Json] = j.mapValue
  implicit def jsonToVector(j: Json): Vector[Json] = j.vectorValue
}


