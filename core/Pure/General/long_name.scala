/*  Title:      Pure/General/long_name.scala
    Author:     Makarius

Long names.
*/

package isabelle


object Long_Name
{
  val separator = "."
  val separator_char = '.'

  def is_qualified(name: String): Boolean = name.contains(separator_char)

  def implode(names: List[String]): String = names.mkString(separator)
  def explode(name: String): List[String] = Library.space_explode(separator_char, name)

  def qualify(qual: String, name: String): String =
    if (qual == "" || name == "") name
    else qual + separator + name

  def base_name(name: String): String =
    if (name == "") ""
    else explode(name).last
}

