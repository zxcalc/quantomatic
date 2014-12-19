/*  Title:      Pure/Tools/build_console.scala
    Author:     Makarius

Check and build Isabelle session for console tool.
*/

package isabelle


object Build_Console
{
  /* build_console */

  def build_console(
    options: Options,
    progress: Build.Progress = Build.Ignore_Progress,
    dirs: List[Path] = Nil,
    no_build: Boolean = false,
    system_mode: Boolean = false,
    session: String): Int =
  {
    if (no_build ||
        Build.build(options = options, build_heap = true, no_build = true,
          dirs = dirs, sessions = List(session)) == 0) 0
    else {
      progress.echo("Build started for Isabelle/" + session + " ...")
      Build.build(options = options, progress = progress, build_heap = true,
        dirs = dirs, system_mode = system_mode, sessions = List(session))
    }
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool {
      args.toList match {
        case
          session ::
          Properties.Value.Boolean(no_build) ::
          Properties.Value.Boolean(system_mode) ::
          options_file ::
          Command_Line.Chunks(dirs, system_options) =>
            val options = (Options.init() /: system_options)(_ + _)
            File.write(Path.explode(options_file), YXML.string_of_body(options.encode))

            val progress = new Build.Console_Progress()
            progress.interrupt_handler {
              build_console(options, progress,
                dirs.map(Path.explode(_)), no_build, system_mode, session)
            }
        case _ => error("Bad arguments:\n" + cat_lines(args))
      }
    }
  }
}

