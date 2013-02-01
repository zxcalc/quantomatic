package quanto.util

import math.{abs,min,max}

/**
 * A quadtree for 2D spacial queries
 */

sealed abstract class QuadTree[A] {
  def x1: Double
  def y1: Double
  def x2: Double
  def y2: Double

  def value: Option[A]
  def insert(v: A, p: (Double,Double)): QuadTree[A]

  // visit each node/leaf with 'f', recursing until f returns true or a leaf is encountered
  def visit(f : A => Boolean)
}

object QuadTree {
  def apply[A](items: Iterable[(A,(Double,Double))]): QuadTree[A] = {
    items.headOption match {
      case Some((_,(hx,hy))) =>
        // compute bounding box
        val (x1,y1,x2,y2) = items.tail.foldLeft(hx,hy,hx,hy) {
          case ((minX,minY,maxX,maxY),(_,(x,y))) => (min(minX,x),min(minY,y),max(maxX,x),max(maxY,y))
        }

        items.foldLeft[QuadTree[A]](QuadNode[A](x1,y1,x2,y2,None)) { case (tr,(v,p)) => tr.insert(v,p) }
      case None => QuadNode[A](0,0,0,0,None)
    }
  }
}

case class QuadNode[A](
  x1: Double, y1: Double,
  x2: Double, y2: Double,

  value: Option[A],

  nw: QuadTree[A],
  ne: QuadTree[A],
  sw: QuadTree[A],
  se: QuadTree[A]) extends QuadTree[A]
{
  lazy val children = Seq(nw,ne,sw,se)
  val midX = (x2 - x1) / 2
  val midY = (y2 - y1) / 2

  def insert(v: A, p: (Double,Double)) = p match {
    case (x,y) if x <  midX && y <  midY => copy(nw = nw.insert(v,p))
    case (x,y) if x >= midX && y <  midY => copy(ne = ne.insert(v,p))
    case (x,y) if x <  midX && y >= midY => copy(sw = sw.insert(v,p))
    case (x,y) if x >= midX && y >= midY => copy(se = se.insert(v,p))
  }

  def visit(f: A => Boolean) {
    value match {
      case None =>
        nw.visit(f)
        ne.visit(f)
        sw.visit(f)
        se.visit(f)
      case Some(v) =>
        if (!f(v)) {
          nw.visit(f)
          ne.visit(f)
          sw.visit(f)
          se.visit(f)
        }
    }
  }
}

object QuadNode {

  // a new empty quad node with given bounds
  def apply[A](x1: Double, y1: Double, x2: Double, y2: Double, value: Option[A]): QuadNode[A] = {
    val midX = (x2 - x1) / 2
    val midY = (y2 - y1) / 2
    val nw = QuadLeaf[A](x1,y1,midX,midY)
    val ne = QuadLeaf[A](midX,y1,x2,midY)
    val sw = QuadLeaf[A](x1,midY,midX,y2)
    val se = QuadLeaf[A](midX,midY,x2,y2)
    QuadNode[A](x1,y1,x2,y2,value,nw,ne,sw,se)
  }
}

case class QuadLeaf[A](
  x1: Double, y1: Double,
  x2: Double, y2: Double,

  p: (Double,Double) = (0,0),
  value: Option[A] = None) extends QuadTree[A]
{
  def insert(v1: A, p1: (Double,Double)) =
    value match {
      case None => copy(p = p1, value = Some(v1))
      case Some(v) =>
        if (abs(p._1 - p1._1) < 0.01 && abs(p._2 - p1._2) < 0.01) {
          // if they are very close, place the current value on the node, and add the new value as a leaf
          QuadNode[A](x1,y1,x2,y2,Some(v)).insert(v1,p1)
        } else {
          // otherwise place them on their own leaves
          QuadNode[A](x1,y1,x2,y2,None).insert(v,p).insert(v1,p1)
        }
    }

  def visit(f: A => Boolean) { value.map(f(_)) }
}

