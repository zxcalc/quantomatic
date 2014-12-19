/*  Title:      Pure/Tools/check_source.scala
    Author:     Makarius

Some sanity checks for Isabelle sources.
*/

package isabelle


object Check_Source
{
  def check_file(path: Path)
  {
    val file_name = path.implode
    val file_pos = path.position
    def line_pos(i: Int) = Position.Line_File(i + 1, file_name)

    val content = File.read(path)

    for { (line, i) <- split_lines(content).iterator.zipWithIndex }
    {
      try {
        Symbol.decode_strict(line)

        for { c <- Word.codepoint_iterator(line); if c > 128 && !Character.isAlphabetic(c) }
        {
          Output.warning("Suspicious Unicode character " + quote(Word.codepoint(c)) +
            Position.here(line_pos(i)))
        }
      }
      catch { case ERROR(msg) => Output.error_message(msg + Position.here(line_pos(i))) }

      if (line.contains('\t'))
        Output.warning("TAB character" + Position.here(line_pos(i)))
    }

    if (content.contains('\r'))
      Output.warning("CR character" + Position.here(file_pos))
  }

  def check_hg(root: Path)
  {
    Output.writeln("Checking " + root + " ...")
    Isabelle_System.hg("--repository " + Isabelle_System.shell_path(root) + " root").check_error
    for {
      file <- Isabelle_System.hg("manifest", root).check_error.out_lines
      if file.endsWith(".thy") || file.endsWith(".ML")
    } check_file(root + Path.explode(file))
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      for (root <- args) check_hg(Path.explode(root))
    }
  }
}

