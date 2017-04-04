import quanto.data._
import quanto.rewrite._
val e1 = AngleExpression.parse("x")
val f1 = AngleExpression.parse("2 a + b")
val p = Vector("x", "y", "z")
val t = Vector("a", "b", "c")
var m = AngleExpressionMatcher(p, t)
println(m.mat)
//m = m.mtch(e1, f1).get
//m.toMap


