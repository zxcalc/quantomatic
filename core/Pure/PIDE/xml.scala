/*  Title:      Pure/PIDE/xml.scala
    Module:     PIDE
    Author:     Makarius

Untyped XML trees and basic data representation.
*/

package isabelle


import java.util.WeakHashMap
import java.lang.ref.WeakReference
import javax.xml.parsers.DocumentBuilderFactory


object XML
{
  /** XML trees **/

  /* datatype representation */

  type Attributes = Properties.T

  sealed abstract class Tree { override def toString: String = string_of_tree(this) }
  case class Elem(markup: Markup, body: List[Tree]) extends Tree
  {
    def name: String = markup.name
  }
  case class Text(content: String) extends Tree

  def elem(name: String, body: List[Tree]) = Elem(Markup(name, Nil), body)
  def elem(name: String) = Elem(Markup(name, Nil), Nil)

  type Body = List[Tree]


  /* wrapped elements */

  val XML_ELEM = "xml_elem"
  val XML_NAME = "xml_name"
  val XML_BODY = "xml_body"

  object Wrapped_Elem
  {
    def apply(markup: Markup, body1: Body, body2: Body): XML.Elem =
      Elem(Markup(XML_ELEM, (XML_NAME, markup.name) :: markup.properties),
        Elem(Markup(XML_BODY, Nil), body1) :: body2)

    def unapply(tree: Tree): Option[(Markup, Body, Body)] =
      tree match {
        case
          Elem(Markup(XML_ELEM, (XML_NAME, name) :: props),
            Elem(Markup(XML_BODY, Nil), body1) :: body2) =>
          Some(Markup(name, props), body1, body2)
        case _ => None
      }
  }


  /* traverse text */

  def traverse_text[A](body: Body)(a: A)(op: (A, String) => A): A =
  {
    def traverse(x: A, t: Tree): A =
      t match {
        case Wrapped_Elem(_, _, ts) => (x /: ts)(traverse)
        case Elem(_, ts) => (x /: ts)(traverse)
        case Text(s) => op(x, s)
      }
    (a /: body)(traverse)
  }

  def text_length(body: Body): Int = traverse_text(body)(0) { case (n, s) => n + s.length }


  /* text content */

  def content(body: Body): String =
  {
    val text = new StringBuilder(text_length(body))
    traverse_text(body)(()) { case (_, s) => text.append(s) }
    text.toString
  }

  def content(tree: Tree): String = content(List(tree))



  /** string representation **/

  def string_of_body(body: Body): String =
  {
    val s = new StringBuilder

    def text(txt: String) {
      if (txt == null) s ++= txt
      else {
        for (c <- txt.iterator) c match {
          case '<' => s ++= "&lt;"
          case '>' => s ++= "&gt;"
          case '&' => s ++= "&amp;"
          case '"' => s ++= "&quot;"
          case '\'' => s ++= "&apos;"
          case _ => s += c
        }
      }
    }
    def attrib(p: (String, String)) { s ++= " "; s ++= p._1; s ++= "=\""; text(p._2); s ++= "\"" }
    def elem(markup: Markup) { s ++= markup.name; markup.properties.foreach(attrib) }
    def tree(t: Tree): Unit =
      t match {
        case Elem(markup, Nil) =>
          s ++= "<"; elem(markup); s ++= "/>"
        case Elem(markup, ts) =>
          s ++= "<"; elem(markup); s ++= ">"
          ts.foreach(tree)
          s ++= "</"; s ++= markup.name; s ++= ">"
        case Text(txt) => text(txt)
      }
    body.foreach(tree)
    s.toString
  }

  def string_of_tree(tree: XML.Tree): String = string_of_body(List(tree))



  /** cache for partial sharing (weak table) **/

  class Cache(initial_size: Int = 131071, max_string: Int = 100)
  {
    private var table = new WeakHashMap[Any, WeakReference[Any]](initial_size)

    private def lookup[A](x: A): Option[A] =
    {
      val ref = table.get(x)
      if (ref == null) None
      else {
        val y = ref.asInstanceOf[WeakReference[A]].get
        if (y == null) None
        else Some(y)
      }
    }
    private def store[A](x: A): A =
    {
      table.put(x, new WeakReference[Any](x))
      x
    }

    private def trim_bytes(s: String): String = new String(s.toCharArray)

    private def cache_string(x: String): String =
      if (x == "true") "true"
      else if (x == "false") "false"
      else if (x == "0.0") "0.0"
      else if (Library.is_small_int(x)) Library.signed_string_of_int(Integer.parseInt(x))
      else
        lookup(x) match {
          case Some(y) => y
          case None =>
            val z = trim_bytes(x)
            if (z.length > max_string) z else store(z)
        }
    private def cache_props(x: Properties.T): Properties.T =
      if (x.isEmpty) x
      else
        lookup(x) match {
          case Some(y) => y
          case None => store(x.map(p => (trim_bytes(p._1).intern, cache_string(p._2))))
        }
    private def cache_markup(x: Markup): Markup =
      lookup(x) match {
        case Some(y) => y
        case None =>
          x match {
            case Markup(name, props) =>
              store(Markup(cache_string(name), cache_props(props)))
          }
      }
    private def cache_tree(x: XML.Tree): XML.Tree =
      lookup(x) match {
        case Some(y) => y
        case None =>
          x match {
            case XML.Elem(markup, body) =>
              store(XML.Elem(cache_markup(markup), cache_body(body)))
            case XML.Text(text) => store(XML.Text(cache_string(text)))
          }
      }
    private def cache_body(x: XML.Body): XML.Body =
      if (x.isEmpty) x
      else
        lookup(x) match {
          case Some(y) => y
          case None => x.map(cache_tree(_))
        }

    // main methods
    def string(x: String): String = synchronized { cache_string(x) }
    def props(x: Properties.T): Properties.T = synchronized { cache_props(x) }
    def markup(x: Markup): Markup = synchronized { cache_markup(x) }
    def tree(x: XML.Tree): XML.Tree = synchronized { cache_tree(x) }
    def body(x: XML.Body): XML.Body = synchronized { cache_body(x) }
    def elem(x: XML.Elem): XML.Elem = synchronized { cache_tree(x).asInstanceOf[XML.Elem] }
  }



  /** XML as data representation language **/

  abstract class Error(s: String) extends Exception(s)
  class XML_Atom(s: String) extends Error(s)
  class XML_Body(body: XML.Body) extends Error("")

  object Encode
  {
    type T[A] = A => XML.Body


    /* atomic values */

    def long_atom(i: Long): String = Library.signed_string_of_long(i)

    def int_atom(i: Int): String = Library.signed_string_of_int(i)

    def bool_atom(b: Boolean): String = if (b) "1" else "0"

    def unit_atom(u: Unit) = ""


    /* structural nodes */

    private def node(ts: XML.Body): XML.Tree = XML.Elem(Markup(":", Nil), ts)

    private def vector(xs: List[String]): XML.Attributes =
      xs.zipWithIndex.map({ case (x, i) => (int_atom(i), x) })

    private def tagged(tag: Int, data: (List[String], XML.Body)): XML.Tree =
      XML.Elem(Markup(int_atom(tag), vector(data._1)), data._2)


    /* representation of standard types */

    val properties: T[Properties.T] =
      (props => List(XML.Elem(Markup(":", props), Nil)))

    val string: T[String] = (s => if (s.isEmpty) Nil else List(XML.Text(s)))

    val long: T[Long] = (x => string(long_atom(x)))

    val int: T[Int] = (x => string(int_atom(x)))

    val bool: T[Boolean] = (x => string(bool_atom(x)))

    val unit: T[Unit] = (x => string(unit_atom(x)))

    def pair[A, B](f: T[A], g: T[B]): T[(A, B)] =
      (x => List(node(f(x._1)), node(g(x._2))))

    def triple[A, B, C](f: T[A], g: T[B], h: T[C]): T[(A, B, C)] =
      (x => List(node(f(x._1)), node(g(x._2)), node(h(x._3))))

    def list[A](f: T[A]): T[List[A]] =
      (xs => xs.map((x: A) => node(f(x))))

    def option[A](f: T[A]): T[Option[A]] =
    {
      case None => Nil
      case Some(x) => List(node(f(x)))
    }

    def variant[A](fs: List[PartialFunction[A, (List[String], XML.Body)]]): T[A] =
    {
      case x =>
        val (f, tag) = fs.iterator.zipWithIndex.find(p => p._1.isDefinedAt(x)).get
        List(tagged(tag, f(x)))
    }
  }

  object Decode
  {
    type T[A] = XML.Body => A
    type V[A] = (List[String], XML.Body) => A


    /* atomic values */

    def long_atom(s: String): Long =
      try { java.lang.Long.parseLong(s) }
      catch { case e: NumberFormatException => throw new XML_Atom(s) }

    def int_atom(s: String): Int =
      try { Integer.parseInt(s) }
      catch { case e: NumberFormatException => throw new XML_Atom(s) }

    def bool_atom(s: String): Boolean =
      if (s == "1") true
      else if (s == "0") false
      else throw new XML_Atom(s)

    def unit_atom(s: String): Unit =
      if (s == "") () else throw new XML_Atom(s)


    /* structural nodes */

    private def node(t: XML.Tree): XML.Body =
      t match {
        case XML.Elem(Markup(":", Nil), ts) => ts
        case _ => throw new XML_Body(List(t))
      }

    private def vector(atts: XML.Attributes): List[String] =
      atts.iterator.zipWithIndex.map(
        { case ((a, x), i) => if (int_atom(a) == i) x else throw new XML_Atom(a) }).toList

    private def tagged(t: XML.Tree): (Int, (List[String], XML.Body)) =
      t match {
        case XML.Elem(Markup(name, atts), ts) => (int_atom(name), (vector(atts), ts))
        case _ => throw new XML_Body(List(t))
      }


    /* representation of standard types */

    val properties: T[Properties.T] =
    {
      case List(XML.Elem(Markup(":", props), Nil)) => props
      case ts => throw new XML_Body(ts)
    }

    val string: T[String] =
    {
      case Nil => ""
      case List(XML.Text(s)) => s
      case ts => throw new XML_Body(ts)
    }

    val long: T[Long] = (x => long_atom(string(x)))

    val int: T[Int] = (x => int_atom(string(x)))

    val bool: T[Boolean] = (x => bool_atom(string(x)))

    val unit: T[Unit] = (x => unit_atom(string(x)))

    def pair[A, B](f: T[A], g: T[B]): T[(A, B)] =
    {
      case List(t1, t2) => (f(node(t1)), g(node(t2)))
      case ts => throw new XML_Body(ts)
    }

    def triple[A, B, C](f: T[A], g: T[B], h: T[C]): T[(A, B, C)] =
    {
      case List(t1, t2, t3) => (f(node(t1)), g(node(t2)), h(node(t3)))
      case ts => throw new XML_Body(ts)
    }

    def list[A](f: T[A]): T[List[A]] =
      (ts => ts.map(t => f(node(t))))

    def option[A](f: T[A]): T[Option[A]] =
    {
      case Nil => None
      case List(t) => Some(f(node(t)))
      case ts => throw new XML_Body(ts)
    }

    def variant[A](fs: List[V[A]]): T[A] =
    {
      case List(t) =>
        val (tag, (xs, ts)) = tagged(t)
        val f =
          try { fs(tag) }
          catch { case _: IndexOutOfBoundsException => throw new XML_Body(List(t)) }
        f(xs, ts)
      case ts => throw new XML_Body(ts)
    }
  }
}
