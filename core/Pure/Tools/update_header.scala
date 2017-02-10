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


  /* Isabelle tool wrapper */

  private val headings =
    Set("chapter", "section", "subsection", "subsubsection", "paragraph", "subparagraph")

  val isabelle_tool =
    Isabelle_Tool("update_header", "replace obsolete theory header command", args =>
    {
      var section = "section"

      val getopts = Getopts("""
Usage: isabelle update_header [FILES|DIRS...]

  Options are:
    -s COMMAND   alternative heading command (default 'section')

  Recursively find .thy files and replace obsolete theory header commands
  by 'chapter', 'section' (default), 'subsection', 'subsubsection',
  'paragraph', 'subparagraph'.

  Old versions of files are preserved by appending "~~".
""",
        "s:" -> (arg => section = arg))

      val specs = getopts(args)
      if (specs.isEmpty) getopts.usage()

      if (!headings.contains(section))
        error("Bad heading command: " + quote(section))

      for {
        spec <- specs
        file <- File.find_files(Path.explode(spec).file, file => file.getName.endsWith(".thy"))
      } update_header(section, Path.explode(File.standard_path(file)))
    })
}
