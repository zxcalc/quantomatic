/*  Title:      Pure/Admin/build_doc.scala
    Author:     Makarius

Build Isabelle documentation.
*/

package isabelle


import java.io.{File => JFile}


object Build_Doc
{
  /* build_doc */

  def build_doc(
    options: Options,
    progress: Progress = Ignore_Progress,
    all_docs: Boolean = false,
    max_jobs: Int = 1,
    system_mode: Boolean = false,
    docs: List[String] = Nil): Int =
  {
    val selection =
      for {
        (name, info) <- Sessions.load(options).topological_order
        if info.groups.contains("doc")
        doc = info.options.string("document_variants")
        if all_docs || docs.contains(doc)
      } yield (doc, name)

    val selected_docs = selection.map(_._1)
    val sessions = selection.map(_._2)

    docs.filter(doc => !selected_docs.contains(doc)) match {
      case Nil =>
      case bad => error("No documentation session for " + commas_quote(bad))
    }

    progress.echo("Build started for documentation " + commas_quote(selected_docs))

    val res1 =
      Build.build(options, progress, requirements = true, build_heap = true,
        max_jobs = max_jobs, system_mode = system_mode, sessions = sessions)
    if (res1.ok) {
      Isabelle_System.with_tmp_dir("document_output")(output =>
        {
          val res2 =
            Build.build(
              options.bool.update("browser_info", false).
                string.update("document", "pdf").
                string.update("document_output", output.implode),
              progress, clean_build = true, max_jobs = max_jobs, system_mode = system_mode,
              sessions = sessions)
          if (res2.ok) {
            val doc_dir = Path.explode("$ISABELLE_HOME/doc")
            for (doc <- selected_docs) {
              val name = Path.explode(doc + ".pdf")
              File.copy(output + name, doc_dir + name)
            }
          }
          res2.rc
        })
    }
    else res1.rc
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("build_doc", "build Isabelle documentation", args =>
    {
      var all_docs = false
      var max_jobs = 1
      var system_mode = false

      val getopts =
        Getopts("""
Usage: isabelle build_doc [OPTIONS] [DOCS ...]

  Options are:
    -a           select all documentation sessions
    -j INT       maximum number of parallel jobs (default 1)
    -s           system build mode

  Build Isabelle documentation from documentation sessions with
  suitable document_variants entry.
""",
          "a" -> (_ => all_docs = true),
          "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
          "s" -> (_ => system_mode = true))

      val docs = getopts(args)

      if (!all_docs && docs.isEmpty) getopts.usage()

      val options = Options.init()
      val progress = new Console_Progress()
      val rc =
        progress.interrupt_handler {
          build_doc(options, progress, all_docs, max_jobs, system_mode, docs)
        }
      sys.exit(rc)
    }, admin = true)
}
