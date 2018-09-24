package quanto.data.test

import org.scalatest._
import quanto.data.CompositeExpression._
import quanto.data.Theory.ValueType
import quanto.data._
import quanto.util.Rational

class CompositeExpressionSpec extends FlatSpec {

  behavior of "Type Parsing"

  val vs : List[ValueType] = ValueType.values.toList

  it should "parse singletons" in {
    assert(parseTypes("Angle") == Vector(ValueType.AngleExpr))
    assert(parseTypes("string") == Vector(ValueType.String))
    assert(parseTypes("String") == Vector(ValueType.String))
    assert(parseTypes("angle_expr") == Vector(ValueType.AngleExpr))
    assert(parseTypes("long") == Vector(ValueType.Long))
    assert(parseTypes("Empty") == Vector(ValueType.Empty))
    assert(parseTypes("empty") == Vector(ValueType.Empty))
  }

  it should "parse pairs" in {
    var pairs = vs.flatMap(v => vs.map(w => (v,w)))
    pairs.foreach(p => {
      var v1 = p._1
      var v2 = p._2
      var combined = s"$v1, $v2"
      assert(parseTypes(combined) === Vector(v1, v2))
    }
    )
  }

  it should "parse all in a row" in {
    var all = vs.mkString("(", ", ", ")")
    assert(parseTypes(all) === vs.toVector)
  }

}