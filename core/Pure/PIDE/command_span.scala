/*  Title:      Pure/PIDE/command_span.scala
    Author:     Makarius

Syntactic representation of command spans.
*/

package isabelle


import scala.collection.mutable


object Command_Span
{
  sealed abstract class Kind {
    override def toString: String =
      this match {
        case Command_Span(name, _) => if (name != "") name else "<command>"
        case Ignored_Span => "<ignored>"
        case Malformed_Span => "<malformed>"
      }
  }
  case class Command_Span(name: String, pos: Position.T) extends Kind
  case object Ignored_Span extends Kind
  case object Malformed_Span extends Kind

  sealed case class Span(kind: Kind, content: List[Token])
  {
    def name: String =
      kind match { case Command_Span(name, _) => name case _ => "" }

    def position: Position.T =
      kind match { case Command_Span(_, pos) => pos case _ => Position.none }

    def length: Int = (0 /: content)(_ + _.source.length)

    def compact_source: (String, Span) =
    {
      val source = Token.implode(content)
      val content1 = new mutable.ListBuffer[Token]
      var i = 0
      for (Token(kind, s) <- content) {
        val n = s.length
        val s1 = source.substring(i, i + n)
        content1 += Token(kind, s1)
        i += n
      }
      (source, Span(kind, content1.toList))
    }
  }

  val empty: Span = Span(Ignored_Span, Nil)

  def unparsed(source: String): Span =
    Span(Malformed_Span, List(Token(Token.Kind.UNPARSED, source)))
}
