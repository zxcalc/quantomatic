/*  Title:      Pure/Thy/html.scala
    Author:     Makarius

HTML presentation elements.
*/

package isabelle


object HTML
{
  /* encode text with control symbols */

  val control =
    Map(
      Symbol.sub -> "sub",
      Symbol.sub_decoded -> "sub",
      Symbol.sup -> "sup",
      Symbol.sup_decoded -> "sup",
      Symbol.bold -> "b",
      Symbol.bold_decoded -> "b")

  def output(text: String, s: StringBuilder)
  {
    def output_char(c: Char) =
      c match {
        case '<' => s ++= "&lt;"
        case '>' => s ++= "&gt;"
        case '&' => s ++= "&amp;"
        case '"' => s ++= "&quot;"
        case '\'' => s ++= "&#39;"
        case '\n' => s ++= "<br/>"
        case _ => s += c
      }
    def output_chars(str: String) = str.iterator.foreach(output_char(_))

    var ctrl = ""
    for (sym <- Symbol.iterator(text)) {
      if (control.isDefinedAt(sym)) ctrl = sym
      else {
        control.get(ctrl) match {
          case Some(elem) if Symbol.is_controllable(sym) && sym != "\"" =>
            s ++= ("<" + elem + ">")
            output_chars(sym)
            s ++= ("</" + elem + ">")
          case _ =>
            output_chars(ctrl)
            output_chars(sym)
        }
        ctrl = ""
      }
    }
    output_chars(ctrl)
  }

  def output(text: String): String = Library.make_string(output(text, _))


  /* output XML as HTML */

  def output(body: XML.Body, s: StringBuilder)
  {
    def attrib(p: (String, String)): Unit =
      { s ++= " "; s ++= p._1; s ++= "=\""; output(p._2, s); s ++= "\"" }
    def elem(markup: Markup): Unit =
      { s ++= markup.name; markup.properties.foreach(attrib) }
    def tree(t: XML.Tree): Unit =
      t match {
        case XML.Elem(markup, Nil) =>
          s ++= "<"; elem(markup); s ++= "/>"
        case XML.Elem(markup, ts) =>
          s ++= "\n<"; elem(markup); s ++= ">"
          ts.foreach(tree)
          s ++= "</"; s ++= markup.name; s ++= ">\n"
        case XML.Text(txt) => output(txt, s)
      }
    body.foreach(tree)
  }

  def output(body: XML.Body): String = Library.make_string(output(body, _))
  def output(tree: XML.Tree): String = output(List(tree))


  /* structured markup operators */

  def text(txt: String): XML.Body = if (txt.isEmpty) Nil else List(XML.Text(txt))

  class Operator(name: String)
  { def apply(body: XML.Body): XML.Elem = XML.elem(name, body) }

  class Heading(name: String) extends Operator(name)
  { def apply(txt: String): XML.Elem = super.apply(text(txt)) }

  val div = new Operator("div")
  val span = new Operator("span")
  val par = new Operator("p")
  val emph = new Operator("em")
  val bold = new Operator("b")

  val title = new Heading("title")
  val chapter = new Heading("h1")
  val section = new Heading("h2")
  val subsection = new Heading("h3")
  val subsubsection = new Heading("h4")
  val paragraph = new Heading("h5")
  val subparagraph = new Heading("h6")

  def itemize(items: List[XML.Body]): XML.Elem =
    XML.elem("ul", items.map(XML.elem("li", _)))

  def enumerate(items: List[XML.Body]): XML.Elem =
    XML.elem("ol", items.map(XML.elem("li", _)))

  def description(items: List[(XML.Body, XML.Body)]): XML.Elem =
    XML.elem("dl", items.flatMap({ case (x, y) => List(XML.elem("dt", x), XML.elem("dd", y)) }))

  def link(href: String, body: XML.Body = Nil): XML.Elem =
    XML.Elem(Markup("a", List("href" -> href)), if (body.isEmpty) text(href) else body)


  /* document */

  val header: String =
    """<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">"""

  val head_meta: XML.Elem =
    XML.Elem(Markup("meta",
      List("http-equiv" -> "Content-Type", "content" -> "text/html; charset=utf-8")), Nil)

  def output_document(head: XML.Body, body: XML.Body): String =
    cat_lines(
      List(header,
        output(XML.elem("head", head_meta :: head)),
        output(XML.elem("body", body))))


  /* Isabelle document */

  def begin_document(title: String): String =
    header + "\n" +
    "<head>\n" + output(head_meta) + "\n" +
    "<title>" + output(title + " (" + Distribution.version + ")") + "</title>\n" +
    "<link media=\"all\" rel=\"stylesheet\" type=\"text/css\" href=\"isabelle.css\"/>\n" +
    "</head>\n" +
    "\n" +
    "<body>\n" +
    "<div class=\"head\">" +
    "<h1>" + output(title) + "</h1>\n"

  val end_document = "\n</div>\n</body>\n</html>\n"


  /* common markup elements */

  private def session_entry(entry: (String, String)): String =
  {
    val (name, description) = entry
    val descr =
      if (description == "") Nil
      else List(XML.elem("br"), XML.elem("pre", List(XML.Text(description))))
    XML.string_of_tree(
      XML.elem("li",
        List(XML.Elem(Markup("a", List(("href", name + "/index.html"))),
          List(XML.Text(name)))) ::: descr)) + "\n"
  }

  def chapter_index(chapter: String, sessions: List[(String, String)]): String =
  {
    begin_document("Isabelle/" + chapter + " sessions") +
      (if (sessions.isEmpty) ""
       else "<div class=\"sessions\"><ul>\n" + sessions.map(session_entry(_)).mkString + "</ul>") +
    end_document
  }
}
