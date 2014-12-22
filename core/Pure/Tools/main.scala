/*  Title:      Pure/Tools/main.scala
    Author:     Makarius

Main Isabelle application entry point.
*/

package isabelle


import java.lang.{Class, ClassLoader}
import java.io.{File => JFile, BufferedReader, InputStreamReader}
import java.nio.file.Files

import scala.annotation.tailrec


object Main
{
  /** main entry point **/

  def main(args: Array[String])
  {
    val system_dialog = new System_Dialog

    def exit_error(exn: Throwable): Nothing =
    {
      GUI.dialog(null, "Isabelle", GUI.scrollable_text(Exn.message(exn)))
      system_dialog.return_code(Exn.return_code(exn, 2))
      system_dialog.join_exit
    }

    def build
    {
      try {
        GUI.init_laf()
        Isabelle_System.init()

        val mode = Isabelle_System.getenv("JEDIT_BUILD_MODE")
        if (mode == "none")
          system_dialog.return_code(0)
        else {
          val options = Options.init()
          val system_mode = mode == "" || mode == "system"
          val dirs = Path.split(Isabelle_System.getenv("JEDIT_SESSION_DIRS"))
          val session = Isabelle_System.default_logic(
            Isabelle_System.getenv("JEDIT_LOGIC"),
            options.string("jedit_logic"))

          if (Build.build(options = options, build_heap = true, no_build = true,
              dirs = dirs, sessions = List(session)) == 0)
            system_dialog.return_code(0)
          else {
            system_dialog.title("Isabelle build (" + Isabelle_System.getenv("ML_IDENTIFIER") + ")")
            system_dialog.echo("Build started for Isabelle/" + session + " ...")

            val (out, rc) =
              try {
                ("",
                  Build.build(options = options, progress = system_dialog, build_heap = true,
                    dirs = dirs, system_mode = system_mode, sessions = List(session)))
              }
              catch {
                case exn: Throwable =>
                  (Output.error_text(Exn.message(exn)) + "\n", Exn.return_code(exn, 2))
              }

            system_dialog.echo(out + (if (rc == 0) "OK\n" else "Return code: " + rc + "\n"))
            system_dialog.return_code(rc)
          }
        }
      }
      catch { case exn: Throwable => exit_error(exn) }
    }

    def start
    {
      val do_start =
      {
        try {
          /* settings directory */

          val settings_dir = Path.explode("$JEDIT_SETTINGS")
          Isabelle_System.mkdirs(settings_dir + Path.explode("DockableWindowManager"))

          if (!(settings_dir + Path.explode("perspective.xml")).is_file) {
            File.write(settings_dir + Path.explode("DockableWindowManager/perspective-view0.xml"),
              """<DOCKING LEFT="" TOP="" RIGHT="isabelle-documentation" BOTTOM="" LEFT_POS="0" TOP_POS="0" RIGHT_POS="250" BOTTOM_POS="250" />""")
            File.write(settings_dir + Path.explode("perspective.xml"),
              """<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE PERSPECTIVE SYSTEM "perspective.dtd">
<PERSPECTIVE>
<VIEW PLAIN="FALSE">
<GEOMETRY X="0" Y="35" WIDTH="1072" HEIGHT="787" EXT_STATE="0" />
</VIEW>
</PERSPECTIVE>""")
          }


          /* args */

          val jedit_options =
            Isabelle_System.getenv_strict("JEDIT_OPTIONS").split(" +")

          val jedit_settings =
            Array("-settings=" + Isabelle_System.platform_path(Path.explode("$JEDIT_SETTINGS")))

          val more_args =
            if (args.isEmpty)
              Array(Isabelle_System.platform_path(Path.explode("$USER_HOME/Scratch.thy")))
            else args


          /* startup */

          update_environment()

          System.setProperty("jedit.home",
            Isabelle_System.platform_path(Path.explode("$JEDIT_HOME/dist")))

          System.setProperty("scala.home",
            Isabelle_System.platform_path(Path.explode("$SCALA_HOME")))

          val jedit =
            Class.forName("org.gjt.sp.jedit.jEdit", true, ClassLoader.getSystemClassLoader)
          val jedit_main = jedit.getDeclaredMethod("main", classOf[Array[String]])

          () => jedit_main.invoke(null, jedit_options ++ jedit_settings ++ more_args)
        }
        catch { case exn: Throwable => exit_error(exn) }
      }
      do_start()
    }

    if (Platform.is_windows) {
      try {
        GUI.init_laf()

        val isabelle_home0 = System.getenv("ISABELLE_HOME")
        val isabelle_home = System.getProperty("isabelle.home")

        if (isabelle_home0 == null || isabelle_home0 == "") {
          if (isabelle_home == null || isabelle_home == "")
            error("Unknown Isabelle home directory")
          if (!(new JFile(isabelle_home)).isDirectory)
            error("Bad Isabelle home directory: " + quote(isabelle_home))

          val cygwin_root = isabelle_home + "\\contrib\\cygwin"
          if ((new JFile(cygwin_root)).isDirectory)
            System.setProperty("cygwin.root", cygwin_root)

          val uninitialized_file = new JFile(cygwin_root, "isabelle\\uninitialized")
          val uninitialized = uninitialized_file.isFile && uninitialized_file.delete

          if (uninitialized) cygwin_init(system_dialog, isabelle_home, cygwin_root)
        }
      }
      catch { case exn: Throwable => exit_error(exn) }

      if (system_dialog.stopped) {
        system_dialog.return_code(Exn.Interrupt.return_code)
        system_dialog.join_exit
      }
    }

    build
    val rc = system_dialog.join
    if (rc == 0) start else sys.exit(rc)
  }



  /** Cygwin init (e.g. after extraction via 7zip) **/

  private def cygwin_init(system_dialog: System_Dialog, isabelle_home: String, cygwin_root: String)
  {
    system_dialog.title("Isabelle system initialization")
    system_dialog.echo("Initializing Cygwin ...")

    def execute(args: String*): Int =
    {
      val cwd = new JFile(isabelle_home)
      val env = Map("CYGWIN" -> "nodosfilewarning")
      system_dialog.execute(cwd, env, args: _*)
    }

    system_dialog.echo("symlinks ...")
    val symlinks =
    {
      val path = (new JFile(cygwin_root + "\\isabelle\\symlinks")).toPath
      Files.readAllLines(path, UTF8.charset).toArray.toList.asInstanceOf[List[String]]
    }
    @tailrec def recover_symlinks(list: List[String]): Unit =
    {
      list match {
        case Nil | List("") =>
        case link :: content :: rest =>
          val path = (new JFile(isabelle_home, link)).toPath

          val writer = Files.newBufferedWriter(path, UTF8.charset)
          try { writer.write("!<symlink>" + content + "\u0000") }
          finally { writer.close }

          Files.setAttribute(path, "dos:system", true)

          recover_symlinks(rest)
        case _ => error("Unbalanced symlinks list")
      }
    }
    recover_symlinks(symlinks)

    system_dialog.echo("rebaseall ...")
    execute(cygwin_root + "\\bin\\dash.exe", "/isabelle/rebaseall")

    system_dialog.echo("postinstall ...")
    execute(cygwin_root + "\\bin\\bash.exe", "/isabelle/postinstall")

    system_dialog.echo("init ...")
    Isabelle_System.init()
  }



  /** adhoc update of JVM environment variables **/

  def update_environment()
  {
    val update =
    {
      val isabelle_home = Isabelle_System.getenv("ISABELLE_HOME")
      val isabelle_home_user = Isabelle_System.getenv("ISABELLE_HOME_USER")
      val upd =
        if (Platform.is_windows)
          List(
            "ISABELLE_HOME" -> Isabelle_System.jvm_path(isabelle_home),
            "ISABELLE_HOME_USER" -> Isabelle_System.jvm_path(isabelle_home_user),
            "INI_DIR" -> "")
        else
          List(
            "ISABELLE_HOME" -> isabelle_home,
            "ISABELLE_HOME_USER" -> isabelle_home_user)

      (env0: Any) => {
        val env = env0.asInstanceOf[java.util.Map[String, String]]
        upd.foreach {
          case (x, "") => env.remove(x)
          case (x, y) => env.put(x, y)
        }
      }
    }

    classOf[java.util.Collections].getDeclaredClasses
      .find(c => c.getName == "java.util.Collections$UnmodifiableMap") match
    {
      case Some(c) =>
        val m = c.getDeclaredField("m")
        m.setAccessible(true)
        update(m.get(System.getenv()))

        if (Platform.is_windows) {
          val ProcessEnvironment = Class.forName("java.lang.ProcessEnvironment")
          val field = ProcessEnvironment.getDeclaredField("theCaseInsensitiveEnvironment")
          field.setAccessible(true)
          update(field.get(null))
        }

      case None =>
        error("Failed to update JVM environment -- platform incompatibility")
    }
  }
}

