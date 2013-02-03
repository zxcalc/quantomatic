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
  def p: (Double,Double)
  def insert(p: (Double,Double), v: A): QuadTree[A]

  // visit each node/leaf with 'f', recursing until f returns true or a leaf is encountered
  def visit(f : QuadTree[A] => Boolean)
}

object QuadTree {
  def apply[A](items: Iterable[((Double,Double),A)]): QuadTree[A] = {
    items.headOption match {
      case Some(((hx,hy),_)) =>
        // compute bounding box
        val (x1,y1,x2,y2) = items.tail.foldLeft(hx,hy,hx,hy) {
          case ((minX,minY,maxX,maxY),((x,y),_)) => (min(minX,x),min(minY,y),max(maxX,x),max(maxY,y))
        }

        // make bounds square
        val sz = max(x2 - x1, y2 - y1)

        val emptyTree: QuadTree[A] = QuadNode[A](x1,y1,x1+sz,y1+sz)

        items.foldLeft(emptyTree) { case (tr,(p,v)) => tr.insert(p,v) }
      case None => QuadNode[A](0,0,0,0)
    }
  }
}

case class QuadNode[A](
  x1: Double, y1: Double,
  x2: Double, y2: Double,

  value: Option[A],
  p: (Double,Double) = (0,0),
  nw: QuadTree[A],
  ne: QuadTree[A],
  sw: QuadTree[A],
  se: QuadTree[A]) extends QuadTree[A]
{
  lazy val children = Seq(nw,ne,sw,se)
  val midX = (x1 + x2) / 2
  val midY = (y1 + y2) / 2

  def insert(p: (Double,Double), v: A) = p match {
    case (x,y) if x <  midX && y <  midY => copy(nw = nw.insert(p,v))
    case (x,y) if x >= midX && y <  midY => copy(ne = ne.insert(p,v))
    case (x,y) if x <  midX && y >= midY => copy(sw = sw.insert(p,v))
    case (x,y) if x >= midX && y >= midY => copy(se = se.insert(p,v))
  }

  def visit(f : QuadTree[A] => Boolean) {
      if (!f(this)) {
        nw.visit(f)
        ne.visit(f)
        sw.visit(f)
        se.visit(f)
      }
  }

  //def mapValue[B](f: Option[A] => Option[B]) = QuadNode[B](x1,y1,x2,y2,f(value),p,nw,ne,sw,se)
}

object QuadNode {

  // a new empty quad node with given bounds
  def apply[A](x1: Double, y1: Double, x2: Double, y2: Double): QuadNode[A] = {
    QuadNode[A](x1,y1,x2,y2,None,(0.0,0.0))
  }

  // a new node with a value, point, and the given bounds
  def apply[A](x1: Double, y1: Double, x2: Double, y2: Double, value: Option[A], p: (Double,Double)): QuadNode[A] = {
    val midX = (x1 + x2) / 2
    val midY = (y1 + y2) / 2
    val nw = QuadLeaf[A](x1,y1,midX,midY)
    val ne = QuadLeaf[A](midX,y1,x2,midY)
    val sw = QuadLeaf[A](x1,midY,midX,y2)
    val se = QuadLeaf[A](midX,midY,x2,y2)
    QuadNode[A](x1,y1,x2,y2,value,p,nw,ne,sw,se)
  }
}

case class QuadLeaf[A](
  x1: Double, y1: Double,
  x2: Double, y2: Double,
  value: Option[A] = None,
  p: (Double,Double) = (0,0)) extends QuadTree[A]
{
  def insert(p1: (Double,Double), v1: A) =
    value match {
      case None => copy(p = p1, value = Some(v1))
      case Some(v) =>
        if (abs(p._1 - p1._1) < 0.01 && abs(p._2 - p1._2) < 0.01) {
          // if they are very close, place the current value on the node, and add the new value as a leaf
          QuadNode[A](x1,y1,x2,y2,Some(v),p).insert(p1,v1)
        } else {
          // otherwise place them on their own leaves
          QuadNode[A](x1,y1,x2,y2).insert(p,v).insert(p1,v1)
        }
    }

  def visit(f : QuadTree[A] => Boolean) { f(this) }
  def mapValue[B](f: A => B): QuadLeaf[B] = QuadLeaf[B](x1,y1,x2,y2,value.map(f(_)),p)
}

