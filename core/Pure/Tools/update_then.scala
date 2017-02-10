/*  Title:      Pure/Tools/update_then.scala
    Author:     Makarius

Expand old Isar command conflations 'hence' and 'thus'.
*/

package isabelle


object Update_Then
{
  def update_then(path: Path)
  {
    val text0 = File.read(path)
    val text1 =
      (for (tok <- Token.explode(Keyword.Keywords.empty, text0).iterator)
        yield {
          tok.source match {
            case "hence" => "then have"
            case "thus" => "then show"
            case s => s
        } }).mkString

    if (text0 != text1) {
      Output.writeln("changing " + path)
      File.write_backup2(path, text1)
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("update_then", "expand old Isar command conflations 'hence' and 'thus'", args =>
    {
      val getopts = Getopts("""
Usage: isabelle update_then [FILES|DIRS...]

  Recursively find .thy files and expand old Isar command conflations:

    hence  ~>  then have
    thus   ~>  then show

  Old versions of files are preserved by appending "~~".
""")

      val specs = getopts(args)
      if (specs.isEmpty) getopts.usage()

      for {
        spec <- specs
        file <- File.find_files(Path.explode(spec).file, file => file.getName.endsWith(".thy"))
      } update_then(Path.explode(File.standard_path(file)))
    })
}
