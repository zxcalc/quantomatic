/*  Title:      Pure/General/output.scala
    Module:     PIDE
    Author:     Makarius

Isabelle channels for diagnostic output.
*/

package isabelle


object Output
{
  def clean_yxml(msg: String): String =
    try { XML.content(YXML.parse_body(msg)) }
    catch { case ERROR(_) => msg }

  def writeln_text(msg: String): String = clean_yxml(msg)
  def warning_text(msg: String): String = cat_lines(split_lines(clean_yxml(msg)).map("### " + _))
  def error_text(msg: String): String = cat_lines(split_lines(clean_yxml(msg)).map("*** " + _))

  def writeln(msg: String) { Console.err.println(writeln_text(msg)) }
  def warning(msg: String) { Console.err.println(warning_text(msg)) }
  def error_message(msg: String) { Console.err.println(error_text(msg)) }
}
