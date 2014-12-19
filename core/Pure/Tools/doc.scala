/*  Title:      Pure/Tools/doc.scala
    Author:     Makarius

Access to Isabelle documentation.
*/

package isabelle


import scala.util.matching.Regex


object Doc
{
  /* dirs */

  def dirs(): List[Path] =
    Path.split(Isabelle_System.getenv("ISABELLE_DOCS")).map(dir =>
      if (dir.is_dir) dir
      else error("Bad documentation directory: " + dir))


  /* contents */

  private def contents_lines(): List[(Path, String)] =
    for {
      dir <- dirs()
      catalog = dir + Path.basic("Contents")
      if catalog.is_file
      line <- split_lines(Library.trim_line(File.read(catalog)))
    } yield (dir, line)

  sealed abstract class Entry
  case class Section(text: String, important: Boolean) extends Entry
  case class Doc(name: String, title: String, path: Path) extends Entry
  case class Text_File(name: String, path: Path) extends Entry

  def text_file(name: Path): Option[Text_File] =
  {
    val path = Path.variable("ISABELLE_HOME") + name
    if (path.is_file) Some(Text_File(name.implode, path))
    else None
  }

  private val Section_Entry = new Regex("""^(\S.*)\s*$""")
  private val Doc_Entry = new Regex("""^\s+(\S+)\s+(.+)\s*$""")

  private def release_notes(): List[Entry] =
    Section("Release notes", true) ::
      Path.split(Isabelle_System.getenv_strict("ISABELLE_DOCS_RELEASE_NOTES")).flatMap(text_file(_))

  private def examples(): List[Entry] =
    Section("Examples", true) ::
      Path.split(Isabelle_System.getenv_strict("ISABELLE_DOCS_EXAMPLES")).map(file =>
        text_file(file) match {
          case Some(entry) => entry
          case None => error("Bad entry in ISABELLE_DOCS_EXAMPLES: " + file)
        })

  def contents(): List[Entry] =
    (for {
      (dir, line) <- contents_lines()
      entry <-
        line match {
          case Section_Entry(text) =>
            Library.try_unsuffix("!", text) match {
              case None => Some(Section(text, false))
              case Some(txt) => Some(Section(txt, true))
            }
          case Doc_Entry(name, title) => Some(Doc(name, title, dir + Path.basic(name)))
          case _ => None
        }
    } yield entry) ::: release_notes() ::: examples()


  /* view */

  def view(path: Path)
  {
    if (path.is_file) Console.println(Library.trim_line(File.read(path)))
    else {
      val pdf = path.ext("pdf")
      if (pdf.is_file) Isabelle_System.pdf_viewer(pdf)
      else error("Bad Isabelle documentation file: " + pdf)
    }
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      val entries = contents()
      if (args.isEmpty) Console.println(cat_lines(contents_lines().map(_._2)))
      else {
        args.foreach(arg =>
          entries.collectFirst { case Doc(name, _, path) if arg == name => path } match {
            case Some(path) => view(path)
            case None => error("No Isabelle documentation entry: " + quote(arg))
          }
        )
      }
    }
  }
}

