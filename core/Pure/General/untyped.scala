/*  Title:      Pure/General/untyped.scala
    Module:     PIDE
    Author:     Makarius

Untyped, unscoped, unchecked access to JVM objects.
*/

package isabelle


object Untyped
{
  def get(obj: AnyRef, x: String): AnyRef =
  {
    obj.getClass.getDeclaredFields.find(_.getName == x) match {
      case Some(field) =>
        field.setAccessible(true)
        field.get(obj)
      case None => error("No field " + quote(x) + " for " + obj)
    }
  }
}

