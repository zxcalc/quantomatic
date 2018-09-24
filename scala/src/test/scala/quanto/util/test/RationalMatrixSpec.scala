package quanto.util.test

import org.scalatest._
import quanto.util._
import Rational.intToRational


class RationalMatrixSpec extends FlatSpec {
  behavior of "A rational matrix"

  it should "be constructable" in {
    val m = new RationalMatrix(Vector(Vector(1,2,3), Vector(4,5,6), Vector(7,8,9)), 3, Some(2))
  }

  it should "perform gaussian elimination" in {
    val m1 = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 3, Some(2))
    val m2 = new RationalMatrix(Vector(Vector(1,2), Vector(2,2)), 2, Some(2))
    val m3 = new RationalMatrix(Vector(Vector(Rational(1,5),2,3,4), Vector(2,2,2,2)), 3, Some(2))

    assert(!m1.isReduced)
    assert(m1.gauss.get.isReduced)
    assert(!m2.isReduced)
    assert(m2.gauss.get.isReduced)
    assert(!m3.isReduced)
    assert(m3.gauss.get.isReduced)
  }

  it should "cope with leading 0s" in {
    val m1 = new RationalMatrix(Vector(Vector(0,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 3)
    val m1r = m1.gauss.get
    val m2 = new RationalMatrix(Vector(Vector(0,0,0,0), Vector(2,2,2,2), Vector(2,1,3,1)), 3)
    val m2r = m2.gauss.get
    assert(!m1.isReduced)
    assert(m1.gauss.get.isReduced)
    assert(!m2.isReduced)
    assert(m2.gauss.get.isReduced)
//    println(m1 + "->" + m1r)
//    print(m2 + "->" + m2r)
  }

  it should "solve a consistent system" in {
    val m = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 3)
    val solution = new RationalMatrix(Vector(
      Vector(1,0,0,Rational(-4,3)),
      Vector(0,1,0,Rational(5,3)),
      Vector(0,0,1,Rational(2,3))),
      3)
    assert(m.gauss.get === solution)
  }

  it should "give None for an inconsistent system" in {
    val m = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(1,2,3,5)), 3)
    assert(m.gauss === None)
  }

  it should "do gaussian elimination eagerly" in {
    val m1 = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 3)
    var m2 = new RationalMatrix(Vector(), 3)
    m2 = m2.gaussUpdate(Vector(1,2,3,4)).get
    m2 = m2.gaussUpdate(Vector(2,2,2,2)).get
    m2 = m2.gaussUpdate(Vector(2,1,3,1)).get
    assert(m1.gauss.get === m2)
  }

  it should "insert new variables" in {
    val m1 = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 2).padTo(3,1)
    val m2 = new RationalMatrix(Vector(Vector(1,2,0,3,4), Vector(2,2,0,2,2), Vector(2,1,0,3,1)), 3)
    println(m1)
    println(m2)

    assert(m1 === m2)
  }

  it should "insert new constants" in {
    val m1 = new RationalMatrix(Vector(Vector(1,2,3,4), Vector(2,2,2,2), Vector(2,1,3,1)), 2).padTo(2,2)
    val m2 = new RationalMatrix(Vector(Vector(1,2,3,0,4), Vector(2,2,2,0,2), Vector(2,1,3,0,1)), 2)
    println(m1)
    println(m2)

    assert(m1 === m2)
  }
}
