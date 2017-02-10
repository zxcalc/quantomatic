/*  Title:      Pure/General/properties.scala
    Author:     Makarius

Property lists.
*/

package isabelle


object Properties
{
  type Entry = (java.lang.String, java.lang.String)
  type T = List[Entry]

  def defined(props: T, name: java.lang.String): java.lang.Boolean =
    props.exists({ case (x, _) => x == name })

  def get(props: T, name: java.lang.String): Option[java.lang.String] =
    props.collectFirst({ case (x, y) if x == name => y })

  def put(props: T, entry: Entry): T =
  {
    val (x, y) = entry
    def update(ps: T): T =
      ps match {
        case (p @ (x1, _)) :: rest =>
          if (x1 == x) (x1, y) :: rest else p :: update(rest)
        case Nil => Nil
      }
    if (defined(props, x)) update(props) else entry :: props
  }


  /* named entries */

  class String(val name: java.lang.String)
  {
    def apply(value: java.lang.String): T = List((name, value))
    def unapply(props: T): Option[java.lang.String] =
      props.find(_._1 == name).map(_._2)
  }

  class Boolean(val name: java.lang.String)
  {
    def apply(value: scala.Boolean): T = List((name, Value.Boolean(value)))
    def unapply(props: T): Option[scala.Boolean] =
      props.find(_._1 == name) match {
        case None => None
        case Some((_, value)) => Value.Boolean.unapply(value)
      }
  }

  class Int(val name: java.lang.String)
  {
    def apply(value: scala.Int): T = List((name, Value.Int(value)))
    def unapply(props: T): Option[scala.Int] =
      props.find(_._1 == name) match {
        case None => None
        case Some((_, value)) => Value.Int.unapply(value)
      }
  }

  class Long(val name: java.lang.String)
  {
    def apply(value: scala.Long): T = List((name, Value.Long(value)))
    def unapply(props: T): Option[scala.Long] =
      props.find(_._1 == name) match {
        case None => None
        case Some((_, value)) => Value.Long.unapply(value)
      }
  }

  class Double(val name: java.lang.String)
  {
    def apply(value: scala.Double): T = List((name, Value.Double(value)))
    def unapply(props: T): Option[scala.Double] =
      props.find(_._1 == name) match {
        case None => None
        case Some((_, value)) => Value.Double.unapply(value)
      }
  }
}

