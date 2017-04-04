package quanto.rewrite

import quanto.data._
import quanto.util._


class AngleExpressionMatcher(pVars : Vector[String], tVars : Vector[String], mat : RationalMatrix) {
  def addMatch(pExpr : AngleExpression, tExpr : AngleExpression) : Option[AngleExpressionMatcher] = {
    val r1 = pVars.map { v => pExpr.coeffs.getOrElse(v, Rational(0)) }
    val r2 = tVars.map { v => tExpr.coeffs.getOrElse(v, Rational(0)) }
    val row = r1 ++ r2 :+ (tExpr.const - pExpr.const)
    mat.gaussUpdate(row).map { m => new AngleExpressionMatcher(pVars, tVars, m) }
  }


  def toMap : Map[String, AngleExpression] =
    if (mat.numCols == 0) Map()
    else mat.rows.foldLeft(Map[String,AngleExpression]()) { (mp, row) =>
      val p = RationalMatrix.findPivot(row)
      var coeffs = Map[String,Rational]()
      for (i <- p+1 until mat.line)
        if (row(i) != Rational(0)) coeffs = coeffs + (pVars(i) -> row(i) * -1)
      for (i <- mat.line to row.length - 2)
        if (row(i) != Rational(0)) coeffs = coeffs + (tVars(i-mat.line) -> row(i))

      mp + (pVars(p) -> AngleExpression(row.last, coeffs))
    }
}

object AngleExpressionMatcher {
  def apply(pVars : Vector[String], tVars : Vector[String]) =
    new AngleExpressionMatcher(pVars, tVars, new RationalMatrix(Vector(), pVars.length))
}
