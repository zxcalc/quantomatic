package quanto.util

/**
  *
  * A matrix of the form (M1|M2) representing a system of equations:
  *   M1 v = M2 c
  * where "v" is a vector of pattern variables and "c" a vector of target variables (treated as constants) where the
  * final constant "pi" is treated modulo 2.
  *
  * The variables in "t" are treated as constants when judging whether a solution exists.
  *
  * @param mat A 2D Vector of rational numbers
  * @param line The position of the line between LHS and RHS matrices, i.e. the number of free variables
  * @param constModulo Modulus to apply to the constant or -1 for no modulus.
  */

class RationalMatrix(val mat: Vector[Vector[Rational]], val line : Int, val constModulo : Int = 2) {
  import RationalMatrix._
  def numRows : Int = mat.length
  def numCols : Int = if (mat.isEmpty) 0 else mat(0).length

  override def equals(that: Any) = that match {
    case m1 : RationalMatrix =>
      mat == m1.mat && line == line && constModulo == constModulo
    case _ => false
  }

  def apply(i : Int): Vector[Rational] = mat(i)
  def rows = mat

  //def insertVar = new RationalMatrix(mat.map { row => ins(row, line) }, line+1, constModulo)
  //def insertConst = new RationalMatrix(mat.map { row => ins(row, row.length-1) }, line, constModulo)

  def padTo(vCols: Int, cCols: Int) = {
    val m = vCols - line
    val n = cCols - (numCols - line - 1)
    if (m >= 0 && n >= 0)
      new RationalMatrix(mat.map { row =>
        row.slice(0, line) ++ Vector.fill(m)(Rational(0)) ++
        row.slice(line, row.length - 1) ++ Vector.fill(n)(Rational(0)) :+
        row(row.length - 1)
      }, line + vCols, constModulo)
    else this
  }

//  private def ins(row : Vector[Rational], i : Int) : Vector[Rational] =
//    row.take(i) ++ (Rational(0) +: row.takeRight(row.length - i))

  def isReduced: Boolean = {
    var p = -1
    for (r <- mat) {
      val p1 = findPivot(r)
      if (p1 <= p || r(p1) != Rational(1)) return false
      p = p1
    }

    true
  }

  // translate matrix to echelon form. returns None if there is an inconsistent row, i.e. a row of the form
  // (0..0|v!=0..0)
  def gauss : Option[RationalMatrix] = {
    val empty : Option[RationalMatrix] = Some(new RationalMatrix(Vector(), line, constModulo))
    mat.foldLeft(empty) {
      case (Some(m), r) => m.gaussUpdate(r)
      case _ => None
    }
  }

  // add a new row, keeping the matrix in echelon form. Returns None if new row introduces an inconsistency.
  def gaussUpdate(row : Vector[Rational]) : Option[RationalMatrix] = {
    var r = row

    // gaussian reduce the new row
    mat.foreach { r1 =>
      val p1 = findPivot(r1)
      if (p1 != -1) {
        r = reduceWith(r, r1, p1)
      }
    }

    val p = findPivot(r)
    if (p == -1) {
      // new row is redundant, so return this
      Some(this)
    } else if (p >= line) {
      // new row makes system inconsistent, so return None
      None
    } else {
      // otherwise use the new row to further reduce existing rows, and insert into the correct position
      r = normaliseRow(r, p)
      val (mat1, inserted) = mat.foldLeft((Vector[Vector[Rational]](),false)) {
        case ((rows, ins),row1) =>
          if (findPivot(row1) > p) {
            if (!ins) (rows :+ r :+ row1, true)
            else (rows :+ row1, ins)
          } else {
            (rows :+ reduceWith(row1, r, p), ins)
          }
      }

      Some(new RationalMatrix(if (!inserted) mat1 :+ r else mat1, line, constModulo))
    }
  }

  // multiply row by a scalar to make pivot = 1
  private def normaliseRow(row : Vector[Rational], p : Int) = {
    val r = row(p).inv
    Vector.tabulate(row.length) { i =>
      if (constModulo != -1 && i == row.length) (row(i) * r) mod constModulo
      else row(i) * r
    }
  }

  // subtract a multiple of row2 (with pivot p2) from row1 to make row1(p2) == 0. Assumes row2 is
  // normalised.
  private def reduceWith(row1 : Vector[Rational], row2 : Vector[Rational], p2 : Int) = {
    val n = row1(p2)
    if (!n.isZero) {
      Vector.tabulate(row1.length) { i =>
        val n1 = row1(i) - (n * row2(i))
        if (constModulo != -1 && i == row1.length) n1 mod constModulo
        else n1
      }
    } else {
      row1
    }
  }

  override def toString = mat.foldLeft("\n") { (s,row) =>
    var t = "["
    for (i <- row.indices) {
      t += " " + row(i).toString
      if (i == line-1) t += " |"
      else t += " "
    }
    s + t + "]\n"
  }
}

object RationalMatrix {
  def findPivot(row : Vector[Rational]) : Int =
    row.indexWhere(!_.isZero)
}

