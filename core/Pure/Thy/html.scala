/*  Title:      Pure/Thy/html.scala
    Module:     PIDE
    Author:     Makarius

HTML presentation elements.
*/

package isabelle


object HTML
{
  /* encode text */

  def encode(text: String): String =
  {
    val s = new StringBuilder
    for (c <- text.iterator) c match {
      case '<' => s ++= "&lt;"
      case '>' => s ++= "&gt;"
      case '&' => s ++= "&amp;"
      case '"' => s ++= "&quot;"
      case '\'' => s ++= "&#39;"
      case '\n' => s ++= "<br/>"
      case _ => s += c
    }
    s.toString
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
