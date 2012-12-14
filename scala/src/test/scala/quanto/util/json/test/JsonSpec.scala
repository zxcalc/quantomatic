import org.scalatest.FlatSpec
import quanto.util.json._

// implicit conversions from JsonXXX to XXX
import JsonValues._

class JsonSpec extends FlatSpec {
  behavior of "Json parser"

  val testObj =
    """
      |{
      |  "foo": 12,
      |  "bar": [23, 14, "etc"],
      |  "nested": {"obj": "ect", "and": ["l", "i", {"s": "t"}]}
      |}
    """.stripMargin

  val testArr =
    """
      |[
      |  "foo", 12,
      |  "bar", [23, 14, "etc"],
      |  "nested", {"obj": "ect", "and": ["l", "i", {"s": "t"}]}
      |]
    """.stripMargin

  it should "parse an array from a string" in {
    Json.parse(testArr)
  }

  val stream = new Json.Input(new java.io.StringReader(testObj + testArr + "\n" + testObj + "\n"))

  it should "parse the first item from a stream" in {
    val obj = Json.parse(stream)
    assert(obj.isInstanceOf[JsonObject])
  }

  it should "parse the second item from a stream" in {
    val arr = Json.parse(stream)
    assert(arr.isInstanceOf[JsonArray])
  }

  it should "parse the third item from a stream" in {
    val obj = Json.parse(stream)
    assert(obj.isInstanceOf[JsonObject])
  }


  behavior of "Json tree"

  var result : Json = _
  it can "be parsed from a string" in {
    result = Json.parse(testObj)
  }

  it should "be the expected object" in {
    result match {
      case obj: JsonObject =>
        assert(obj("foo").intValue === 12)

        // match-case for nested objects and arrays (recommended if coercion might fail)
        obj("bar") match {
          case arr: JsonArray =>
            assert(arr(0).intValue === 23)
            assert(arr(1).intValue === 14)
            assert(arr(2).stringValue === "etc")
          case _ => fail("expected: JsonArray")
        }

        // coercion for nested objects/arrays
        val obj1 = obj("nested").asObject
        assert(obj1("obj").stringValue === "ect")

        val arr = obj1("and").asArray
        assert(arr(0).stringValue === "l")
        assert(arr(1).stringValue === "i")

        val obj2 = arr(2).asObject
        assert(obj2("s").stringValue === "t")
      case _ => fail("expected: JsonObject")
    }
  }

  it should "throw JsonAccessException for bad coercions" in {
    intercept[JsonAccessException] { result.boolValue }
    intercept[JsonAccessException] { result.asArray }

    val obj = result.asObject

    intercept[JsonAccessException] {
      obj("foo").stringValue
    }
  }


}