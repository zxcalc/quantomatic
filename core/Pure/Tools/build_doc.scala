/*  Title:      Pure/Tools/build_doc.scala
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
    progress: Build.Progress = Build.Ignore_Progress,
    all_docs: Boolean = false,
    max_jobs: Int = 1,
    system_mode: Boolean = false,
    docs: List[String] = Nil): Int =
  {
    val selection =
      for {
        (name, info) <- Build.find_sessions(options).topological_order
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

    val rc1 =
      Build.build(options, progress, requirements = true, build_heap = true,
        max_jobs = max_jobs, system_mode = system_mode, sessions = sessions)
    if (rc1 == 0) {
      Isabelle_System.with_tmp_dir("document_output")(output =>
        {
          val rc2 =
            Build.build(
              options.bool.update("browser_info", false).
                string.update("document", "pdf").
                string.update("document_output", Isabelle_System.posix_path(output)),
              progress, clean_build = true, max_jobs = max_jobs, system_mode = system_mode,
              sessions = sessions)
          if (rc2 == 0) {
            val doc_dir = Path.explode("$ISABELLE_HOME/doc").file
            for (doc <- selected_docs) {
              val name = doc + ".pdf"
              File.copy(new JFile(output, name), new JFile(doc_dir, name))
            }
          }
          rc2
        })
    }
    else rc1
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool {
      args.toList match {
        case
          Properties.Value.Boolean(all_docs) ::
          Properties.Value.Int(max_jobs) ::
          Properties.Value.Boolean(system_mode) ::
          Command_Line.Chunks(docs) =>
            val options = Options.init()
            val progress = new Build.Console_Progress()
            progress.interrupt_handler {
              build_doc(options, progress, all_docs, max_jobs, system_mode, docs)
            }
        case _ => error("Bad arguments:\n" + cat_lines(args))
      }
    }
  }
}

