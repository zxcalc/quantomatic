/*  Title:      Pure/Thy/present.scala
    Author:     Makarius

Theory presentation: HTML.
*/

package isabelle


import scala.collection.immutable.SortedMap


object Present
{
  /* maintain chapter index -- NOT thread-safe */

  private val index_path = Path.basic("index.html")
  private val sessions_path = Path.basic(".sessions")

  private def read_sessions(dir: Path): List[(String, String)] =
  {
    val path = dir + sessions_path
    if (path.is_file) {
      import XML.Decode._
      list(pair(string, string))(YXML.parse_body(File.read(path)))
    }
    else Nil
  }

  private def write_sessions(dir: Path, sessions: List[(String, String)])
  {
    import XML.Encode._
    File.write(dir + sessions_path, YXML.string_of_body(list(pair(string, string))(sessions)))
  }

  def update_chapter_index(info_path: Path, chapter: String, new_sessions: List[(String, String)])
  {
    val dir = info_path + Path.basic(chapter)
    Isabelle_System.mkdirs(dir)

    val sessions0 =
      try { read_sessions(dir) }
      catch { case _: XML.Error => Nil }

    val sessions = (SortedMap.empty[String, String] ++ sessions0 ++ new_sessions).toList

    write_sessions(dir, sessions)
    File.write(dir + index_path, HTML.chapter_index(chapter, sessions))
  }
}

