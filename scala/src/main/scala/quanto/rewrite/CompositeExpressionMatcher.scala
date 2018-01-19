package quanto.rewrite


import quanto.data.Theory.ValueType
import quanto.data.{CompositeExpression, PhaseExpression}
import quanto.util.{Rational, RationalMatrix}

class PhaseExpressionMatcher(pVars: Vector[String], tVars: Vector[String], mat: RationalMatrix) {
  val pvSet : Set[String] = pVars.toSet
  val tvSet : Set[String] = tVars.toSet

  def addMatch(patternExpression: PhaseExpression, targetExpression: PhaseExpression): Option[PhaseExpressionMatcher] = {
    val patternVars1 = pVars ++ (patternExpression.vars.toSet -- pvSet).toVector
    val targetVars1 = tVars ++ (targetExpression.vars.toSet -- tvSet).toVector
    val zero = Rational(0)
    val r1 = patternVars1.map { v => patternExpression.coefficients.getOrElse(v, zero) }
    val r2 = targetVars1.map { v => targetExpression.coefficients.getOrElse(v, zero) }
    val row = r1 ++ r2 :+ (targetExpression.constant - patternExpression.constant)

    mat.padTo(patternVars1.length, targetVars1.length).gaussUpdate(row).map { mat1 =>
      new PhaseExpressionMatcher(patternVars1, targetVars1, mat1)
    }
  }


  def toMap: Map[String, PhaseExpression] =
    if (mat.numCols == 0) Map()
    else mat.rows.foldLeft(Map[String, PhaseExpression]()) { (mp, row) =>
      val p = RationalMatrix.findPivot(row)
      var coefficients = Map[String, Rational]()
      for (i <- p + 1 until mat.line)
        if (row(i) != Rational(0)) coefficients = coefficients + (pVars(i) -> row(i) * -1)
      for (i <- mat.line to row.length - 2)
        if (row(i) != Rational(0)) coefficients = coefficients + (tVars(i - mat.line) -> row(i))

      mp + (pVars(p) -> PhaseExpression(row.last, coefficients, ValueType.Rational))
    }

  def toMap(valueType: ValueType) : Map[String, PhaseExpression] = toMap.mapValues(_.as(valueType))
}

object PhaseExpressionMatcher {
  def apply(pVars: Vector[String], tVars: Vector[String], modulus: Option[Int]) =
    new PhaseExpressionMatcher(pVars, tVars, new RationalMatrix(Vector(), pVars.length, modulus))

  def empty : PhaseExpressionMatcher = PhaseExpressionMatcher(Vector(), Vector(), None)
}


class CompositeExpressionMatcher(matchers: Map[ValueType, Option[PhaseExpressionMatcher]]) {

  // Add a single matching to a specific valueType
  def addPhaseMatch(valueType: ValueType, pExpr: PhaseExpression, tExpr: PhaseExpression): Option[CompositeExpressionMatcher] = {
    val updatedSingletonMatcher = matchers.
      getOrElse(valueType, Some(PhaseExpressionMatcher(Vector(), Vector(), pExpr.modulus))).
      get.addMatch(pExpr, tExpr)
    if (updatedSingletonMatcher.nonEmpty) {
      Some(new CompositeExpressionMatcher(matchers + (valueType -> updatedSingletonMatcher)))
    } else {
      None
    }
  }

  // Add matchings by component
  def addMatch(pExpr: CompositeExpression, tExpr: CompositeExpression): Option[CompositeExpressionMatcher] = {
    val valuePairs = pExpr.values.zip(tExpr.values)
    val typeValueTriples = pExpr.valueTypes.zip(valuePairs)
    // Loop through each valueType and apply relevant matches
    // If any of the matches fail then the whole thing needs to fail
    typeValueTriples.foldLeft(Some(this): Option[CompositeExpressionMatcher])((om, t) => {
      if (om.nonEmpty) {
        om.get.addPhaseMatch(t._1, t._2._1.asInstanceOf[PhaseExpression], t._2._2.asInstanceOf[PhaseExpression])
      } else {
        None
      }
    })
  }


  def toMap: Map[ValueType, Map[String, PhaseExpression]] =
    matchers.keySet.map(
      valueType => valueType -> matchers(valueType).getOrElse(PhaseExpressionMatcher.empty).toMap.mapValues(_.as(valueType))
    ).toMap
}

object CompositeExpressionMatcher {
  def apply() : CompositeExpressionMatcher = new CompositeExpressionMatcher(Map())
}
