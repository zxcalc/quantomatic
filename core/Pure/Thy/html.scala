/*  Title:      Pure/Thy/html.scala
    Author:     Makarius

HTML presentation elements.
*/

package isabelle


object HTML
{
  /* encode text with control symbols */

  val control_decoded =
    Map(Symbol.sub_decoded -> "sub",
      Symbol.sup_decoded -> "sup",
      Symbol.bold_decoded -> "b")

  def encode(text: String): String =
  {
    val result = new StringBuilder

    def encode_char(c: Char) =
      c match {
        case '<' => result ++= "&lt;"
        case '>' => result ++= "&gt;"
        case '&' => result ++= "&amp;"
        case '"' => result ++= "&quot;"
        case '\'' => result ++= "&#39;"
        case '\n' => result ++= "<br/>"
        case _ => result += c
      }
    def encode_chars(s: String) = s.iterator.foreach(encode_char(_))

    var control = ""
    for (sym <- Symbol.iterator(text)) {
      if (control_decoded.isDefinedAt(sym)) control = sym
      else {
        control_decoded.get(control) match {
          case Some(elem) if Symbol.is_controllable(sym) && sym != "\"" =>
            result ++= ("<" + elem + ">")
            encode_chars(sym)
            result ++= ("</" + elem + ">")
          case _ =>
            encode_chars(control)
            encode_chars(sym)
        }
        control = ""
      }
    }
    encode_chars(control)

    result.toString
  }


  /* document */

  val end_document = "\n</div>\n</body>\n</html>\n"

  def begin_document(title: String): String =
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" +
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" " +
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" +
    "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" +
    "<head>\n" +
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n" +
    "<title>" + encode(title) + "</title>\n" +
    "<link media=\"all\" rel=\"stylesheet\" type=\"text/css\" href=\"isabelle.css\"/>\n" +
    "</head>\n" +
    "\n" +
    "<body>\n" +
    "<div class=\"head\">" +
    "<h1>" + encode(title) + "</h1>\n"


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
