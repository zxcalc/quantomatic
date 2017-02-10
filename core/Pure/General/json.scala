/*  Title:      Pure/General/json.scala
    Author:     Makarius

Support for JSON parsing.
*/

package isabelle


object JSON
{
  /* parse */

  def parse(text: String): Any =
    scala.util.parsing.json.JSON.parseFull(text) getOrElse error("Malformed JSON")


  /* field access */

  def any(obj: Any, name: String): Option[Any] =
    obj match {
      case m: Map[String, Any] => m.get(name)
      case _ => None
    }

  def array(obj: Any, name: String): List[Any] =
    any(obj, name) match {
      case Some(a: List[Any]) => a
      case _ => Nil
    }

  def string(obj: Any, name: String): Option[String] =
    any(obj, name) match {
      case Some(x: String) => Some(x)
      case _ => None
    }

  def double(obj: Any, name: String): Option[Double] =
    any(obj, name) match {
      case Some(x: Double) => Some(x)
      case _ => None
    }

  def long(obj: Any, name: String): Option[Long] =
    double(obj, name).map(_.toLong)

  def int(obj: Any, name: String): Option[Int] =
    double(obj, name).map(_.toInt)

  def bool(obj: Any, name: String): Option[Boolean] =
    any(obj, name) match {
      case Some(x: Boolean) => Some(x)
      case _ => None
    }
}
