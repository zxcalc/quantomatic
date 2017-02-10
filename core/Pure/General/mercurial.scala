/*  Title:      Pure/General/mercurial.scala
    Author:     Makarius

Support for Mercurial repositories, with local or remote repository clone
and working directory (via ssh connection).
*/

package isabelle


import java.io.{File => JFile}


object Mercurial
{
  /* command-line syntax */

  def optional(s: String, prefix: String = ""): String =
    if (s == "") "" else " " + prefix + " " + Bash.string(s)

  def opt_flag(flag: String, b: Boolean): String = if (b) " " + flag else ""
  def opt_rev(s: String): String = optional(s, "--rev")
  def opt_template(s: String): String = optional(s, "--template")


  /* repository access */

  def is_repository(root: Path, ssh: Option[SSH.Session] = None): Boolean =
    new Repository(root, ssh).command("root").ok

  def repository(root: Path, ssh: Option[SSH.Session] = None): Repository =
  {
    val hg = new Repository(root, ssh)
    hg.command("root").check
    hg
  }

  def clone_repository(
    source: String, root: Path, options: String = "", ssh: Option[SSH.Session] = None): Repository =
  {
    val hg = new Repository(root, ssh)
    ssh match {
      case None => Isabelle_System.mkdirs(hg.root.dir)
      case Some(ssh) => ssh.mkdirs(hg.root.dir)
    }
    hg.command("clone", Bash.string(source) + " " + File.bash_path(hg.root), options).check
    hg
  }

  def setup_repository(source: String, root: Path, ssh: Option[SSH.Session] = None): Repository =
  {
    val present =
      ssh match {
        case None => root.is_dir
        case Some(ssh) => ssh.is_dir(root)
      }
    if (present) { val hg = repository(root, ssh = ssh); hg.pull(remote = source); hg }
    else clone_repository(source, root, options = "--noupdate", ssh = ssh)
  }

  class Repository private[Mercurial](root_path: Path, ssh: Option[SSH.Session])
  {
    hg =>

    val root =
      ssh match {
        case None => root_path.expand
        case Some(ssh) => root_path.expand_env(ssh.settings)
      }

    def root_url: String =
      ssh match {
        case None => root.implode
        case Some(ssh) => ssh.hg_url + root.implode
      }

    override def toString: String =
      ssh match {
        case None => root.implode
        case Some(ssh) => ssh.toString + ":" + root.implode
      }

    def command(name: String, args: String = "", options: String = ""): Process_Result =
    {
      val cmdline =
        "\"${HG:-hg}\"" +
          (if (name == "clone") "" else " --repository " + File.bash_path(root)) +
          " --noninteractive " + name + " " + options + " " + args
      ssh match {
        case None => Isabelle_System.bash(cmdline)
        case Some(ssh) => ssh.execute(cmdline)
      }
    }

    def archive(target: String, rev: String = "", options: String = ""): Unit =
      hg.command("archive", opt_rev(rev) + " " + Bash.string(target), options).check

    def heads(template: String = "{node|short}\n", options: String = ""): List[String] =
      hg.command("heads", opt_template(template), options).check.out_lines

    def identify(rev: String = "tip", options: String = ""): String =
      hg.command("id", opt_rev(rev), options).check.out_lines.headOption getOrElse ""

    def id(rev: String = "tip"): String = identify(rev, options = "-i")

    def manifest(rev: String = "", options: String = ""): List[String] =
      hg.command("manifest", opt_rev(rev), options).check.out_lines

    def log(rev: String = "", template: String = "", options: String = ""): String =
      hg.command("log", opt_rev(rev) + opt_template(template), options).check.out

    def push(remote: String = "", rev: String = "", force: Boolean = false, options: String = "")
    {
      hg.command("push", opt_rev(rev) + opt_flag("--force", force) + optional(remote), options).
        check_rc(rc => rc == 0 | rc == 1)
    }

    def pull(remote: String = "", rev: String = "", options: String = ""): Unit =
      hg.command("pull", opt_rev(rev) + optional(remote), options).check

    def update(
      rev: String = "", clean: Boolean = false, check: Boolean = false, options: String = "")
    {
      hg.command("update",
        opt_rev(rev) + opt_flag("--clean", clean) + opt_flag("--check", check), options).check
    }
  }
}
