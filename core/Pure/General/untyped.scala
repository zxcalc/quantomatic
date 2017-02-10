/*  Title:      Pure/General/untyped.scala
    Author:     Makarius

Untyped, unscoped, unchecked access to JVM objects.
*/

package isabelle


import java.lang.reflect.{Method, Field}


object Untyped
{
  def method(c: Class[_], name: String, arg_types: Class[_]*): Method =
  {
    val m = c.getDeclaredMethod(name, arg_types: _*)
    m.setAccessible(true)
    m
  }

  def classes(obj: AnyRef): Iterator[Class[_ <: AnyRef]] = new Iterator[Class[_ <: AnyRef]] {
    private var next_elem: Class[_ <: AnyRef] = obj.getClass
    def hasNext(): Boolean = next_elem != null
    def next(): Class[_ <: AnyRef] = {
      val c = next_elem
      next_elem = c.getSuperclass.asInstanceOf[Class[_ <: AnyRef]]
      c
    }
  }

  def field(obj: AnyRef, x: String): Field =
  {
    val iterator =
      for {
        c <- classes(obj)
        field <- c.getDeclaredFields.iterator
        if field.getName == x
      } yield {
        field.setAccessible(true)
        field
      }
    if (iterator.hasNext) iterator.next
    else error("No field " + quote(x) + " for " + obj)
  }

  def get[A](obj: AnyRef, x: String): A =
    if (obj == null) null.asInstanceOf[A]
    else field(obj, x).get(obj).asInstanceOf[A]

  def set[A](obj: AnyRef, x: String, y: A): Unit =
    field(obj, x).set(obj, y)
}
