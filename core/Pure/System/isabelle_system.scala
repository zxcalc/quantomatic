/*  Title:      Pure/System/isabelle_system.scala
    Author:     Makarius

Fundamental Isabelle system environment: quasi-static module with
optional init operation.
*/

package isabelle


import java.io.{File => JFile, IOException, BufferedReader, InputStreamReader}
import java.nio.file.{Path => JPath, Files, SimpleFileVisitor, FileVisitResult}
import java.nio.file.attribute.BasicFileAttributes

import scala.collection.mutable


object Isabelle_System
{
  /** bootstrap information **/

  def jdk_home(): String =
  {
    val java_home = System.getProperty("java.home", "")
    val home = new JFile(java_home)
    val parent = home.getParent
    if (home.getName == "jre" && parent != null &&
        (new JFile(new JFile(parent, "bin"), "javac")).exists) parent
    else java_home
  }

  def bootstrap_directory(
    preference: String, envar: String, property: String, description: String): String =
  {
    def check(s: String): Option[String] =
      if (s != null && s != "") Some(s) else None

    val value =
      check(preference) orElse  // explicit argument
      check(System.getenv(envar)) orElse  // e.g. inherited from running isabelle tool
      check(System.getProperty(property)) getOrElse  // e.g. via JVM application boot process
      error("Unknown " + description + " directory")

    if ((new JFile(value)).isDirectory) value
    else error("Bad " + description + " directory " + quote(value))
  }



  /** implicit settings environment **/

  @volatile private var _settings: Option[Map[String, String]] = None

  def settings(): Map[String, String] =
  {
    if (_settings.isEmpty) init()  // unsynchronized check
    _settings.get
  }

  def init(isabelle_root: String = "", cygwin_root: String = ""): Unit = synchronized {
    if (_settings.isEmpty) {
      import scala.collection.JavaConversions._

      val isabelle_root1 =
        bootstrap_directory(isabelle_root, "ISABELLE_ROOT", "isabelle.root", "Isabelle root")

      val cygwin_root1 =
        if (Platform.is_windows)
          bootstrap_directory(cygwin_root, "CYGWIN_ROOT", "cygwin.root", "Cygwin root")
        else ""

      if (Platform.is_windows) Cygwin.init(isabelle_root1, cygwin_root1)

      def set_cygwin_root()
      {
        if (Platform.is_windows)
          _settings = Some(_settings.getOrElse(Map.empty) + ("CYGWIN_ROOT" -> cygwin_root1))
      }

      set_cygwin_root()

      def default(env: Map[String, String], entry: (String, String)): Map[String, String] =
        if (env.isDefinedAt(entry._1) || entry._2 == "") env
        else env + entry

      val env =
      {
        val temp_windows =
        {
          val temp = if (Platform.is_windows) System.getenv("TEMP") else null
          if (temp != null && temp.contains('\\')) temp else ""
        }
        val user_home = System.getProperty("user.home", "")
        val isabelle_app = System.getProperty("isabelle.app", "")

        default(
          default(
            default(sys.env + ("ISABELLE_JDK_HOME" -> File.standard_path(jdk_home())),
              "TEMP_WINDOWS" -> temp_windows),
            "HOME" -> user_home),
          "ISABELLE_APP" -> "true")
      }

      val settings =
      {
        val dump = JFile.createTempFile("settings", null)
        dump.deleteOnExit
        try {
          val cmd1 =
            if (Platform.is_windows)
              List(cygwin_root1 + "\\bin\\bash", "-l",
                File.standard_path(isabelle_root1 + "\\bin\\isabelle"))
            else
              List(isabelle_root1 + "/bin/isabelle")
          val cmd = cmd1 ::: List("getenv", "-d", dump.toString)

          val (output, rc) = process_output(process(cmd, env = env, redirect = true))
          if (rc != 0) error(output)

          val entries =
            (for (entry <- File.read(dump).split("\u0000") if entry != "") yield {
              val i = entry.indexOf('=')
              if (i <= 0) entry -> ""
              else entry.substring(0, i) -> entry.substring(i + 1)
            }).toMap
          entries + ("PATH" -> entries("PATH_JVM")) - "PATH_JVM"
        }
        finally { dump.delete }
      }
      _settings = Some(settings)
      set_cygwin_root()
    }
  }


  /* getenv */

  def getenv(name: String, env: Map[String, String] = settings()): String =
    env.getOrElse(name, "")

  def getenv_strict(name: String, env: Map[String, String] = settings()): String =
  {
    val value = getenv(name, env)
    if (value != "") value
    else error("Undefined Isabelle environment variable: " + quote(name))
  }

  def cygwin_root(): String = getenv_strict("CYGWIN_ROOT")

  def library_path(env: Map[String, String], elem: String): Map[String, String] =
    if (Platform.is_windows) env
    else {
      val x = if (Platform.is_macos) "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH"
      env.getOrElse(x, "") match {
        case "" => env + (x -> elem)
        case y => env + (x -> (y + ":" + elem))
      }
    }



  /** file-system operations **/

  /* source files of Isabelle/ML bootstrap */

  def source_file(path: Path): Option[Path] =
  {
    def check(p: Path): Option[Path] = if (p.is_file) Some(p) else None

    if (path.is_absolute || path.is_current) check(path)
    else {
      check(Path.explode("~~/src/Pure") + path) orElse
        (if (getenv("ML_SOURCES") == "") None
         else check(Path.explode("$ML_SOURCES") + path))
    }
  }


  /* directories */

  def mkdirs(path: Path): Unit =
    if (!path.is_dir) {
      bash("perl -e \"use File::Path make_path; make_path('" + File.standard_path(path) + "');\"")
      if (!path.is_dir) error("Failed to create directory: " + quote(File.platform_path(path)))
    }

  def copy_dir(dir1: Path, dir2: Path): Unit =
    bash("cp -a " + File.bash_path(dir1) + " " + File.bash_path(dir2)).check


  /* tmp files */

  private def isabelle_tmp_prefix(): JFile =
  {
    val path = Path.explode("$ISABELLE_TMP_PREFIX")
    path.file.mkdirs  // low-level mkdirs
    File.platform_file(path)
  }

  def tmp_file(name: String, ext: String = ""): JFile =
  {
    val suffix = if (ext == "") "" else "." + ext
    val file = Files.createTempFile(isabelle_tmp_prefix().toPath, name, suffix).toFile
    file.deleteOnExit
    file
  }

  def with_tmp_file[A](name: String, ext: String = "")(body: Path => A): A =
  {
    val file = tmp_file(name, ext)
    try { body(File.path(file)) } finally { file.delete }
  }


  /* tmp dirs */

  def rm_tree(root: Path): Unit = rm_tree(root.file)

  def rm_tree(root: JFile)
  {
    root.delete
    if (root.isDirectory) {
      Files.walkFileTree(root.toPath,
        new SimpleFileVisitor[JPath] {
          override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult =
          {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: JPath, e: IOException): FileVisitResult =
          {
            if (e == null) {
              Files.delete(dir)
              FileVisitResult.CONTINUE
            }
            else throw e
          }
        }
      )
    }
  }

  def tmp_dir(name: String): JFile =
  {
    val dir = Files.createTempDirectory(isabelle_tmp_prefix().toPath, name).toFile
    dir.deleteOnExit
    dir
  }

  def with_tmp_dir[A](name: String)(body: Path => A): A =
  {
    val dir = tmp_dir(name)
    try { body(File.path(dir)) } finally { rm_tree(dir) }
  }



  /** external processes **/

  /* raw process */

  def process(command_line: List[String],
    cwd: JFile = null,
    env: Map[String, String] = settings(),
    redirect: Boolean = false): Process =
  {
    val proc = new ProcessBuilder
    proc.command(command_line:_*)  // fragile on Windows
    if (cwd != null) proc.directory(cwd)
    if (env != null) {
      proc.environment.clear
      for ((x, y) <- env) proc.environment.put(x, y)
    }
    proc.redirectErrorStream(redirect)
    proc.start
  }

  def process_output(proc: Process): (String, Int) =
  {
    proc.getOutputStream.close

    val output = File.read_stream(proc.getInputStream)
    val rc =
      try { proc.waitFor }
      finally {
        proc.getInputStream.close
        proc.getErrorStream.close
        proc.destroy
        Thread.interrupted
      }
    (output, rc)
  }

  def kill(signal: String, group_pid: String): (String, Int) =
  {
    val bash =
      if (Platform.is_windows) List(cygwin_root() + "\\bin\\bash.exe")
      else List("/usr/bin/env", "bash")
    process_output(process(bash ::: List("-c", "kill -" + signal + " -" + group_pid)))
  }


  /* GNU bash */

  def bash(script: String,
    cwd: JFile = null,
    env: Map[String, String] = settings(),
    redirect: Boolean = false,
    progress_stdout: String => Unit = (_: String) => (),
    progress_stderr: String => Unit = (_: String) => (),
    progress_limit: Option[Long] = None,
    strict: Boolean = true,
    cleanup: () => Unit = () => ()): Process_Result =
  {
    Bash.process(script, cwd = cwd, env = env, redirect = redirect, cleanup = cleanup).
      result(progress_stdout, progress_stderr, progress_limit, strict)
  }

  def hostname(): String = bash("hostname -s").check.out

  def open(arg: String): Unit =
    bash("exec \"$ISABELLE_OPEN\" " + Bash.string(arg) + " >/dev/null 2>/dev/null &")

  def pdf_viewer(arg: Path): Unit =
    bash("exec \"$PDF_VIEWER\" " + File.bash_path(arg) + " >/dev/null 2>/dev/null &")



  /** Isabelle resources **/

  /* repository clone with Admin */

  def admin(): Boolean = Path.explode("~~/Admin").is_dir


  /* components */

  def components(): List[Path] =
    Path.split(getenv_strict("ISABELLE_COMPONENTS"))


  /* default logic */

  def default_logic(args: String*): String =
  {
    args.find(_ != "") match {
      case Some(logic) => logic
      case None => Isabelle_System.getenv_strict("ISABELLE_LOGIC")
    }
  }
}
