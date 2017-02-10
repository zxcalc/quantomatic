/*  Title:      Pure/ML/ml_syntax.scala
    Author:     Makarius

Concrete ML syntax for basic values.
*/

package isabelle


object ML_Syntax
{
  /* int */

  private def signed_int(s: String): String =
    if (s(0) == '-') "~" + s.substring(1) else s

  def print_int(i: Int): String = signed_int(Value.Int(i))
  def print_long(i: Long): String = signed_int(Value.Long(i))


  /* string */

  private def print_chr(c: Byte): String =
    c match {
      case 34 => "\\\""
      case 92 => "\\\\"
      case 9 => "\\t"
      case 10 => "\\n"
      case 12 => "\\f"
      case 13 => "\\r"
      case _ =>
        if (c < 0) "\\" + Library.signed_string_of_int(256 + c)
        else if (c < 32) new String(Array[Char](92, 94, (c + 64).toChar))
        else if (c < 127) Symbol.ascii(c.toChar)
        else "\\" + Library.signed_string_of_int(c)
    }

  def print_char(s: Symbol.Symbol): String =
    if (s.startsWith("\\<")) s
    else UTF8.bytes(s).iterator.map(print_chr(_)).mkString

  def print_string(str: String): String =
    quote(Symbol.iterator(str).map(print_char(_)).mkString)

  def print_string0(str: String): String =
    quote(UTF8.bytes(str).iterator.map(print_chr(_)).mkString)


  /* list */

  def print_list[A](f: A => String)(list: List[A]): String =
    "[" + commas(list.map(f)) + "]"
}
