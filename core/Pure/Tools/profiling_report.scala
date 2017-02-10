/*  Title:      Pure/Tools/profiling_report.scala
    Author:     Makarius

Report Poly/ML profiling information from log files.
*/

package isabelle


import java.util.Locale


object Profiling_Report
{
  def profiling_report(log_file: Build_Log.Log_File): List[(Long, String)] =
  {
    val Line = """^(?:### )?([ 0-9]{10}) (\S+|GARBAGE COLLECTION.*)$""".r
    val Count = """ *(\d+)""".r
    val clean = """-?\(\d+\).*$""".r

    var results = Map.empty[String, Long]
    for (Line(Count(Value.Long(count)), raw_fun) <- log_file.lines) {
      val fun = clean.replaceAllIn(raw_fun, "")
      results += (fun -> (results.getOrElse(fun, 0L) + count))
    }
    for ((fun, count) <- results.toList.sortBy(_._2)) yield (count, fun)
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("profiling_report", "report Poly/ML profiling information from log files", args =>
    {
      Command_Line.tool0 {
        val getopts =
          Getopts("""
Usage: isabelle profiling_report [LOGS ...]

  Report Poly/ML profiling output from log files (potentially compressed).
""")
        val log_names = getopts(args)
        for (name <- log_names) {
          val log_file = Build_Log.Log_File(Path.explode(name))
          val results =
            for ((count, fun) <- profiling_report(log_file))
              yield
                String.format(Locale.ROOT, "%14d %s",
                  count.asInstanceOf[AnyRef], fun.asInstanceOf[AnyRef])
          if (results.nonEmpty)
            Output.writeln(cat_lines((log_file.name + ":") :: results))
        }
      }
    })
}
