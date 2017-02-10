/*  Title:      Pure/Thy/present.scala
    Author:     Makarius

Theory presentation: HTML.
*/

package isabelle


import java.io.{File => JFile}

import scala.collection.immutable.SortedMap


object Present
{
  /* session graph */

  def session_graph(
    parent_session: String,
    parent_loaded: String => Boolean,
    deps: List[Thy_Info.Dep]): Graph_Display.Graph =
  {
    val parent_session_node =
      Graph_Display.Node("[" + parent_session + "]", "session." + parent_session)

    def node(name: Document.Node.Name): Graph_Display.Node =
      if (parent_loaded(name.theory)) parent_session_node
      else Graph_Display.Node(name.theory, "theory." + name.theory)

    (Graph_Display.empty_graph /: deps) {
      case (g, dep) =>
        if (parent_loaded(dep.name.theory)) g
        else {
          val a = node(dep.name)
          val bs = dep.header.imports.map({ case (name, _) => node(name) })
          ((g /: (a :: bs))(_.default_node(_, Nil)) /: bs)(_.add_edge(_, a))
        }
    }
  }


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

  def update_chapter_index(browser_info: Path, chapter: String, new_sessions: List[(String, String)])
  {
    val dir = browser_info + Path.basic(chapter)
    Isabelle_System.mkdirs(dir)

    val sessions0 =
      try { read_sessions(dir) }
      catch { case _: XML.Error => Nil }

    val sessions = (SortedMap.empty[String, String] ++ sessions0 ++ new_sessions).toList

    write_sessions(dir, sessions)
    File.write(dir + index_path, HTML.chapter_index(chapter, sessions))
  }

  def make_global_index(browser_info: Path)
  {
    if (!(browser_info + Path.explode("index.html")).is_file) {
      Isabelle_System.mkdirs(browser_info)
      File.copy(Path.explode("~~/lib/logo/isabelle.gif"),
        browser_info + Path.explode("isabelle.gif"))
      File.write(browser_info + Path.explode("index.html"),
        File.read(Path.explode("~~/lib/html/library_index_header.template")) +
        File.read(Path.explode("~~/lib/html/library_index_content.template")) +
        File.read(Path.explode("~~/lib/html/library_index_footer.template")))
    }
  }


  /* finish session */

  def finish(
    progress: Progress,
    browser_info: Path,
    graph_file: JFile,
    info: Sessions.Info,
    name: String)
  {
    val session_prefix = browser_info + Path.basic(info.chapter) + Path.basic(name)

    if (info.options.bool("browser_info")) {
      Isabelle_System.mkdirs(session_prefix)

      val session_graph = session_prefix + Path.basic("session_graph.pdf")
      File.copy(graph_file, session_graph.file)
      Isabelle_System.bash("chmod a+r " + File.bash_path(session_graph))

      File.copy(Path.explode("~~/etc/isabelle.css"), session_prefix)

      for (font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS")))
        File.copy(font, session_prefix)
      for (font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS_HTML")))
        File.copy(font, session_prefix)
    }
  }
}
