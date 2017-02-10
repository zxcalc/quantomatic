/*  Title:      Pure/General/output.scala
    Module:     PIDE
    Author:     Makarius

Isabelle channels for diagnostic output.
*/

package isabelle


object Output
{
  def warning_text(msg: String): String = cat_lines(split_lines(msg).map("### " + _))
  def error_text(msg: String): String = cat_lines(split_lines(msg).map("*** " + _))

  def writeln(msg: String) { Console.err.println(msg) }
  def warning(msg: String) { Console.err.println(warning_text(msg)) }
  def error_message(msg: String) { Console.err.println(error_text(msg)) }
}

