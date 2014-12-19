/*  Title:      Pure/System/command_line.scala
    Author:     Makarius

Support for Isabelle/Scala command line tools.
*/

package isabelle


object Command_Line
{
  object Chunks
  {
    private def chunks(list: List[String]): List[List[String]] =
      list.indexWhere(_ == "\n") match {
        case -1 => List(list)
        case i =>
          val (chunk, rest) = list.splitAt(i)
          chunk :: chunks(rest.tail)
      }
    def unapplySeq(list: List[String]): Option[List[List[String]]] = Some(chunks(list))
  }

  var debug = false

  def tool(body: => Int): Nothing =
  {
    val rc =
      try { body }
      catch {
        case exn: Throwable =>
          if (debug) exn.printStackTrace
          Output.error_message(Exn.message(exn))
          Exn.return_code(exn, 2)
      }
    sys.exit(rc)
  }

  def tool0(body: => Unit): Nothing = tool { body; 0 }
}

