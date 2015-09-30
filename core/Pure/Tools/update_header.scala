/*  Title:      Pure/Tools/update_header.scala
    Author:     Makarius

Replace theory header command.
*/

package isabelle


object Update_Header
{
  def update_header(section: String, path: Path)
  {
    val text0 = File.read(path)
    val text1 =
      (for (tok <- Token.explode(Keyword.Keywords.empty, text0).iterator)
        yield { if (tok.source == "header") section else tok.source }).mkString

    if (text0 != text1) {
      Output.writeln("changing " + path)
      File.write_backup2(path, text1)
    }
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      args.toList match {
        case section :: files =>
          if (!Set("chapter", "section", "subsection", "subsubsection").contains(section))
            error("Bad heading command: " + quote(section))
          files.foreach(file => update_header(section, Path.explode(file)))
        case _ => error("Bad arguments:\n" + cat_lines(args))
      }
    }
  }
}
