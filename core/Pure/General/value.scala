/*  Title:      Pure/General/value.scala
    Author:     Makarius

Plain values, represented as string.
*/

package isabelle


object Value
{
  object Boolean
  {
    def apply(x: scala.Boolean): java.lang.String = x.toString
    def unapply(s: java.lang.String): Option[scala.Boolean] =
      s match {
        case "true" => Some(true)
        case "false" => Some(false)
        case _ => None
      }
    def parse(s: java.lang.String): scala.Boolean =
      unapply(s) getOrElse error("Bad boolean: " + quote(s))
  }

  object Int
  {
    def apply(x: scala.Int): java.lang.String = Library.signed_string_of_int(x)
    def unapply(s: java.lang.String): Option[scala.Int] =
      try { Some(Integer.parseInt(s)) }
      catch { case _: NumberFormatException => None }
    def parse(s: java.lang.String): scala.Int =
      unapply(s) getOrElse error("Bad integer: " + quote(s))
  }

  object Long
  {
    def apply(x: scala.Long): java.lang.String = Library.signed_string_of_long(x)
    def unapply(s: java.lang.String): Option[scala.Long] =
      try { Some(java.lang.Long.parseLong(s)) }
      catch { case _: NumberFormatException => None }
    def parse(s: java.lang.String): scala.Long =
      unapply(s) getOrElse error("Bad integer: " + quote(s))
  }

  object Double
  {
    def apply(x: scala.Double): java.lang.String = x.toString
    def unapply(s: java.lang.String): Option[scala.Double] =
      try { Some(java.lang.Double.parseDouble(s)) }
      catch { case _: NumberFormatException => None }
    def parse(s: java.lang.String): scala.Double =
      unapply(s) getOrElse error("Bad real: " + quote(s))
  }
}
