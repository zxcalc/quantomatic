/*  Title:      Pure/General/output.scala
    Author:     Makarius

Isabelle output channels.
*/

package isabelle


object Output
{
  def clean_yxml(msg: String): String =
    try { XML.content(Protocol_Message.clean_reports(YXML.parse_body(msg))) }
    catch { case ERROR(_) => msg }

  def writeln_text(msg: String): String = clean_yxml(msg)
  def warning_text(msg: String): String = cat_lines(split_lines(clean_yxml(msg)).map("### " + _))
  def error_text(msg: String): String = cat_lines(split_lines(clean_yxml(msg)).map("*** " + _))

  def writeln(msg: String, stdout: Boolean = false)
  {
    if (msg != "") {
      if (stdout) Console.println(writeln_text(msg))
      else Console.err.println(writeln_text(msg))
    }
  }

  def warning(msg: String, stdout: Boolean = false)
  {
    if (msg != "") {
      if (stdout) Console.println(warning_text(msg))
      else Console.err.println(warning_text(msg))
    }
  }

  def error_message(msg: String, stdout: Boolean = false)
  {
    if (msg != "") {
      if (stdout) Console.println(error_text(msg))
      else Console.err.println(error_text(msg))
    }
  }
}
