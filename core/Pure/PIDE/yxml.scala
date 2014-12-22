/*  Title:      Pure/PIDE/yxml.scala
    Module:     PIDE
    Author:     Makarius

Efficient text representation of XML trees.  Suitable for direct
inlining into plain text.
*/

package isabelle


import scala.collection.mutable


object YXML
{
  /* chunk markers */

  val X = '\u0005'
  val Y = '\u0006'

  val is_X = (c: Char) => c == X
  val is_Y = (c: Char) => c == Y

  val X_string = X.toString
  val Y_string = Y.toString

  def detect(s: String): Boolean = s.exists(c => c == X || c == Y)


  /* string representation */  // FIXME byte array version with pseudo-utf-8 (!?)

  def string_of_body(body: XML.Body): String =
  {
    val s = new StringBuilder
    def attrib(p: (String, String)) { s += Y; s ++= p._1; s += '='; s ++= p._2 }
    def tree(t: XML.Tree): Unit =
      t match {
        case XML.Elem(Markup(name, atts), ts) =>
          s += X; s += Y; s ++= name; atts.foreach(attrib); s += X
          ts.foreach(tree)
          s += X; s += Y; s += X
        case XML.Text(text) => s ++= text
      }
    body.foreach(tree)
    s.toString
  }

  def string_of_tree(tree: XML.Tree): String = string_of_body(List(tree))


  /* parsing */

  private def err(msg: String) = error("Malformed YXML: " + msg)
  private def err_attribute() = err("bad attribute")
  private def err_element() = err("bad element")
  private def err_unbalanced(name: String) =
    if (name == "") err("unbalanced element")
    else err("unbalanced element " + quote(name))

  private def parse_attrib(source: CharSequence) = {
    val s = source.toString
    val i = s.indexOf('=')
    if (i <= 0) err_attribute()
    (s.substring(0, i), s.substring(i + 1))
  }


  def parse_body(source: CharSequence): XML.Body =
  {
    /* stack operations */

    def buffer(): mutable.ListBuffer[XML.Tree] = new mutable.ListBuffer[XML.Tree]
    var stack: List[(Markup, mutable.ListBuffer[XML.Tree])] = List((Markup.Empty, buffer()))

    def add(x: XML.Tree)
    {
      (stack: @unchecked) match { case ((_, body) :: _) => body += x }
    }

    def push(name: String, atts: XML.Attributes)
    {
      if (name == "") err_element()
      else stack = (Markup(name, atts), buffer()) :: stack
    }

    def pop()
    {
      (stack: @unchecked) match {
        case ((Markup.Empty, _) :: _) => err_unbalanced("")
        case ((markup, body) :: pending) =>
          stack = pending
          add(XML.Elem(markup, body.toList))
      }
    }


    /* parse chunks */

    for (chunk <- Library.separated_chunks(is_X, source) if chunk.length != 0) {
      if (chunk.length == 1 && chunk.charAt(0) == Y) pop()
      else {
        Library.separated_chunks(is_Y, chunk).toList match {
          case ch :: name :: atts if ch.length == 0 =>
            push(name.toString, atts.map(parse_attrib))
          case txts => for (txt <- txts) add(XML.Text(txt.toString))
        }
      }
    }
    (stack: @unchecked) match {
      case List((Markup.Empty, body)) => body.toList
      case (Markup(name, _), _) :: _ => err_unbalanced(name)
    }
  }

  def parse(source: CharSequence): XML.Tree =
    parse_body(source) match {
      case List(result) => result
      case Nil => XML.Text("")
      case _ => err("multiple results")
    }


  /* failsafe parsing */

  private def markup_broken(source: CharSequence) =
    XML.Elem(Markup.Broken, List(XML.Text(source.toString)))

  def parse_body_failsafe(source: CharSequence): XML.Body =
  {
    try { parse_body(source) }
    catch { case ERROR(_) => List(markup_broken(source)) }
  }

  def parse_failsafe(source: CharSequence): XML.Tree =
  {
    try { parse(source) }
    catch { case ERROR(_) => markup_broken(source) }
  }
}
