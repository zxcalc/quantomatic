/*  Title:      Pure/System/progress.scala
    Author:     Makarius

Progress context for system processes.
*/

package isabelle


import java.io.{File => JFile}


class Progress
{
  def echo(msg: String) {}
  def echo_if(cond: Boolean, msg: String) { if (cond) echo(msg) }
  def theory(session: String, theory: String) {}
  def stopped: Boolean = false
  override def toString: String = if (stopped) "Progress(stopped)" else "Progress"

  def bash(script: String,
    cwd: JFile = null,
    env: Map[String, String] = Isabelle_System.settings(),
    redirect: Boolean = false,
    echo: Boolean = false): Process_Result =
  {
    Isabelle_System.bash(script, cwd = cwd, env = env, redirect = redirect,
      progress_stdout = echo_if(echo, _),
      progress_stderr = echo_if(echo, _))
  }
}

object Ignore_Progress extends Progress

class Console_Progress(verbose: Boolean = false, stderr: Boolean = false) extends Progress
{
  override def echo(msg: String)
  {
    if (stderr) Console.err.println(msg) else Console.println(msg)
  }

  override def theory(session: String, theory: String): Unit =
    if (verbose) echo(session + ": theory " + theory)

  @volatile private var is_stopped = false
  def interrupt_handler[A](e: => A): A = POSIX_Interrupt.handler { is_stopped = true } { e }
  override def stopped: Boolean =
  {
    if (Thread.interrupted) is_stopped = true
    is_stopped
  }
}
