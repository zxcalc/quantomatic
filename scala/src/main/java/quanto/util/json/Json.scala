package quanto.util.json

import com.fasterxml.jackson
import jackson.core.{JsonParser,JsonGenerator,JsonFactory,JsonEncoding,JsonToken}
import jackson.core.{JsonParseException => JacksonParseException}

abstract class JsonException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

// thrown if the user tries to access some part of the tree incorrectly (i.e. with a bad key, index, or type)
class JsonAccessException(message: String, val json: Json)
  extends JsonException(message)

// thrown if a problem is encountered while parsing the JSON
class JsonParseException(message: String, cause: Throwable = null)
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
    val jsonOut = new Json.Output(sw)
    jsonOut.prettyPrint = true
    writeTo(jsonOut)
    jsonOut.close()
    sw.toString
  }

  // permissive coercions for arrays and objects

  // treat string lists like {"a":{}, "b":{}, ...} and null like {}
  def asObject: JsonObject = this match {
    case obj: JsonObject => obj
    case JsonArray(x)    => x.foldLeft(JsonObject()) { (o,k) => o + (k.stringValue -> JsonObject()) }
    case JsonNull        => JsonObject()
    case other           => throw new JsonAccessException("Expected: JsonObject, JsonArray, or JsonNull", other)
  }

  // treat objects like string lists of their keys and null like []
  def asArray: JsonArray = this match {
    case arr: JsonArray  => arr
    case JsonObject(x)   => JsonArray(x.keys.toVector.sorted.map(JsonString(_)))
    case JsonNull        => JsonArray()
    case other           => throw new JsonAccessException("Expected: JsonObject, JsonArray, or JsonNull", other)
  }

  // Convenience accessors for collections. These are overridden to not throw exceptions, where appropriate.
  def get(index: Int): Option[Json] =
    throw new JsonAccessException("Expected: JsonArray, got: " + this.getClass, this)

  def get(key: String): Option[Json] =
    throw new JsonAccessException("Expected: JsonObject, got: " + this.getClass, this)


  def getOrElse(key: String, default: => Json): Json =
    get(key) match { case Some(v) => v; case None => default }

  def getOrElse(key: Int, default: => Json): Json =
    get(key) match { case Some(v) => v; case None => default }


  // 'slash' notation for required children
  def /(key: String)    = get(key) match {
    case Some(x) => x
    case None    => throw new JsonAccessException("Key not found: " + key, this)
  }

  def /(index: Int)    = get(index) match {
    case Some(x) => x
    case None    => throw new JsonAccessException("Index: " + index + " out of bounds", this)
  }

  // optional child notation
  def ?(key: String)    = getOrElse(key, JsonNull)
  def ?(index: Int)     = getOrElse(index, JsonNull)

  // shorthand coercions for optional arrays and objects. The will return an empty collection of the appropriate
  // type if the given field is missing.
//  def ?@(key: String)   = (this ? key).asArray
//  def ?#(key: String)   = (this ? key).asObject

  // JsonPath methods
  def getPath(path: JsonPath): Json = path.get(this)
  def setPath(path: JsonPath, to: Json): Json = path.set(this, to)
  def updatePath(path: JsonPath)(f: Json => Json): Json = path.update(this)(f)

  def getPath(s: String): Json = getPath(JsonPath(s))
  def setPath(s: String, to: Json): Json = setPath(JsonPath(s), to)
  def updatePath(s: String)(f: Json => Json): Json = updatePath(JsonPath(s))(f)

  // convention: returns false for non-null value types
  def isEmpty: Boolean

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

  def floatValue = doubleValue.toFloat

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

  def noEmpty = JsonObject(v.filter(!_._2.isEmpty))

  override def get(key: String) = v.get(key)
  override def getOrElse(key: String, default: => Json) = v.getOrElse[Json](key,default)
  override def mapValue = v
  override def toString() = jsonString

  def asObjectOrKeyArray = if (forall(_._2.isEmpty)) this.asArray else this
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

object JsonArray {
  def apply(v: Json*): JsonArray = JsonArray(Vector(v: _*))
  def apply[T <% Json](c: TraversableOnce[T]): JsonArray = c.foldLeft(JsonArray()){ (a, v) => a :+ v }
}

case object JsonNull extends Json {
  val v = null
  def writeTo(out: Json.Output) { out.g.writeNull() }
  override def get(key: String): Option[Json] = None
  override def get(index: Int): Option[Json] = None
  override def asObject = JsonObject()
  override def asArray = JsonArray()
  override def vectorValue = Vector()
  override def mapValue = Map()
  override def stringValue = ""
  override def intValue = 0
  override def boolValue = false
  def isEmpty = true
}

case class JsonString(v: String) extends Json {
  override def stringValue = v
  def writeTo(out: Json.Output) { out.g.writeString(v) }
  def isEmpty = false
}

case class JsonInt(v: Int) extends Json {
  override def intValue = v
  // note that integers are treated as a sub-type of doubles. This is the expected behaviour 99% of the time.
  override def doubleValue = v.toDouble
  def writeTo(out: Json.Output) { out.g.writeNumber(v) }
  def isEmpty = false
}

case class JsonDouble(v: Double) extends Json {
  override def doubleValue = v
  def writeTo(out: Json.Output) { out.g.writeNumber(v.toString) }
  def isEmpty = false
}

case class JsonBool(v: Boolean) extends Json {
  override def boolValue = v
  def writeTo(out: Json.Output) { out.g.writeBoolean(v) }
  def isEmpty = false
}



trait JsonEnumConversions { self: Enumeration =>
  implicit def fromJson(s: Json) =
    try { withName(s.stringValue) }
    catch { case _: NoSuchElementException =>
      val exp = new StringBuilder
      values.map("\"" + _ + "\"").addString(exp, ", ")
      throw new JsonParseException("Expected " + exp + ", got: " + s)
    }

  implicit def toJson(t: Value) = JsonString(t.toString)
}

object Json {
  lazy protected val factory = new JsonFactory()

  class Input(val p: JsonParser) {
    p.enable(JsonParser.Feature.AUTO_CLOSE_SOURCE)

    def this(s: String) =
      this(try { factory.createJsonParser(s) } catch {
        case e: JacksonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(f: java.io.File) =
      this(try { factory.createJsonParser(f) } catch {
        case e: JacksonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(in: java.io.InputStream) =
      this(try { factory.createJsonParser(in) } catch {
        case e: JacksonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def this(in: java.io.Reader) =
      this(try { factory.createJsonParser(in) } catch {
        case e: JacksonParseException =>
          throw new JsonParseException("Error initialising parser", e) })

    def close() { p.close() }
  }

  class Output(val g: JsonGenerator) {
    g.enable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)

    def this(out: java.io.OutputStream) = this(factory.createJsonGenerator(out, JsonEncoding.UTF8))
    def this(out: java.io.Writer) =       this(factory.createJsonGenerator(out))
    def this(f: java.io.File) =           this(factory.createJsonGenerator(f, JsonEncoding.UTF8))

    private var _pp = false
    def prettyPrint_=(b: Boolean) {
      _pp = b
      if (_pp) g.useDefaultPrettyPrinter() else g.setPrettyPrinter(null)
    }

    def prettyPrint = _pp

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
    while (stack.nonEmpty) {
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
          case JsonToken.VALUE_NULL => Some(JsonNull)
          case JsonToken.VALUE_NUMBER_FLOAT => Some(JsonDouble(p.getValueAsDouble))
          case JsonToken.VALUE_NUMBER_INT => Some(JsonInt(p.getValueAsInt))
          case JsonToken.VALUE_STRING => Some(JsonString(p.getText))
        }
      } catch {
        case e: JacksonParseException =>
          throw new JsonParseException("Error while parsing", e)
      }


      nextJson.map(item => {
        if (stack.nonEmpty) stack.push(stack.pop() match {
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
  implicit def floatToJson(x: Float): JsonDouble = JsonDouble(x.toDouble)

  // tuple implicit conversions, useful for JsonObject(k -> v, ...) construction
  implicit def stringXToStringJson[T <% Json](t: (String,T)): (String,Json) = (t._1, t._2:Json)
  implicit def traversableOnceToJson[T <% Json](c: TraversableOnce[T]): JsonArray = JsonArray(c)
  implicit def optionToJson[T <% Json](jo: Option[T]): Json = jo match { case Some (j) => j; case None => JsonNull }
}

// these are not active by default, as they are not type-safe
object JsonValues {
  implicit def jsonToBool(j: Json): Boolean = j.boolValue
  implicit def jsonToInt(j: Json): Int = j.intValue
  implicit def jsonToDouble(j: Json): Double = j.doubleValue
  implicit def jsonToFloat(j: Json): Double = j.floatValue
  implicit def jsonToString(j: Json): String = j.stringValue
  implicit def jsonToMap(j: Json): Map[String,Json] = j.mapValue
  implicit def jsonToVector(j: Json): Vector[Json] = j.vectorValue
}


