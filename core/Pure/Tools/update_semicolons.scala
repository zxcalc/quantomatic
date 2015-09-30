/*  Title:      Pure/Tools/update_semicolons.scala
    Author:     Makarius

Remove obsolete semicolons from theory sources.
*/

package isabelle


object Update_Semicolons
{
  def update_semicolons(path: Path)
  {
    val text0 = File.read(path)
    val text1 =
      (for (tok <- Token.explode(Keyword.Keywords.empty, text0).iterator if tok.source != ";")
        yield tok.source).mkString

    if (text0 != text1) {
      Output.writeln("changing " + path)
      File.write_backup2(path, text1)
    }
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      args.foreach(arg => update_semicolons(Path.explode(arg)))
    }
  }
}
