package quanto.rewrite

import quanto.data._
import quanto.util._

class AngleExpressionMatcher(pVars : Vector[String], tVars : Vector[String], mat : RationalMatrix) {
  def mtch(pExpr : AngleExpression, tExpr : AngleExpression) : Option[AngleExpressionMatcher] = {
    val r1 = pVars.map { v => pExpr.coeffs.getOrElse(v, Rational(0)) * -1 }
    val r2 = pVars.map { v => tExpr.coeffs.getOrElse(v, Rational(0)) }
    val row = r1 ++ r2 :+ (tExpr.const - pExpr.const)
    mat.gaussUpdate(row).map { m => new AngleExpressionMatcher(pVars, tVars, m) }
  }

  def toMap : Map[String, AngleExpression] = {
    var mp = Map[String,AngleExpression]()
    mat.rows.foreach { row =>
      val p = RationalMatrix.findPivot(row)
      var e = AngleExpression(Rational(0))
    }
    mp
  }
}
