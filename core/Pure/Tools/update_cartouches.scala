/*  Title:      Pure/Tools/update_cartouches.scala
    Author:     Makarius

Update theory syntax to use cartouches.
*/

package isabelle


object Update_Cartouches
{
  /* update cartouches */

  private def cartouche(s: String): String =
    Symbol.open + s + Symbol.close

  private val Verbatim_Body = """(?s)[ \t]*(.*?)[ \t]*""".r

  def update_cartouches(path: Path)
  {
    val text0 = File.read(path)
    val text1 =
      (for (tok <- Token.explode(Keyword.Keywords.empty, text0).iterator)
        yield {
          if (tok.kind == Token.Kind.ALT_STRING) cartouche(tok.content)
          else if (tok.kind == Token.Kind.VERBATIM) {
            tok.content match {
              case Verbatim_Body(s) => cartouche(s)
              case s => tok.source
            }
          }
          else tok.source
        }
      ).mkString

    if (text0 != text1) {
      Output.writeln("changing " + path)
      File.write_backup2(path, text1)
    }
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      args.foreach(arg => update_cartouches(Path.explode(arg)))
    }
  }
}
