/*  Title:      Pure/System/bash.scala
    Author:     Makarius

GNU bash processes, with propagation of interrupts.
*/

package isabelle


import java.io.{File => JFile, BufferedReader, InputStreamReader,
  BufferedWriter, OutputStreamWriter}


object Bash
{
  /* concrete syntax */

  private def bash_chr(c: Byte): String =
  {
    val ch = c.toChar
    ch match {
      case '\t' => "$'\\t'"
      case '\n' => "$'\\n'"
      case '\f' => "$'\\f'"
      case '\r' => "$'\\r'"
      case _ =>
        if (Symbol.is_ascii_letter(ch) || Symbol.is_ascii_digit(ch) || "-./:_".contains(ch))
          Symbol.ascii(ch)
        else if (c < 0) "$'\\x" + Integer.toHexString(256 + c) + "'"
        else if (c < 16) "$'\\x0" + Integer.toHexString(c) + "'"
        else if (c < 32 || c >= 127) "$'\\x" + Integer.toHexString(c) + "'"
        else  "\\" + ch
    }
  }

  def string(s: String): String =
    if (s == "") "\"\""
    else UTF8.bytes(s).iterator.map(bash_chr(_)).mkString

  def strings(ss: List[String]): String =
    ss.iterator.map(Bash.string(_)).mkString(" ")


  /* process and result */

  private class Limited_Progress(proc: Process, progress_limit: Option[Long])
  {
    private var count = 0L
    def apply(progress: String => Unit)(line: String): Unit = synchronized {
      progress(line)
      count = count + line.length + 1
      progress_limit match {
        case Some(limit) if count > limit => proc.terminate
        case _ =>
      }
    }
  }

  def process(script: String,
      cwd: JFile = null,
      env: Map[String, String] = Isabelle_System.settings(),
      redirect: Boolean = false,
      cleanup: () => Unit = () => ()): Process =
    new Process(script, cwd, env, redirect, cleanup)

  class Process private[Bash](
      script: String,
      cwd: JFile,
      env: Map[String, String],
      redirect: Boolean,
      cleanup: () => Unit)
    extends Prover.System_Process
  {
    private val timing_file = Isabelle_System.tmp_file("bash_timing")
    private val timing = Synchronized[Option[Timing]](None)

    private val script_file = Isabelle_System.tmp_file("bash_script")
    File.write(script_file, script)

    private val proc =
      Isabelle_System.process(
        List(File.platform_path(Path.variable("ISABELLE_BASH_PROCESS")), "-",
          File.standard_path(timing_file), "bash", File.standard_path(script_file)),
        cwd = cwd, env = env, redirect = redirect)


    // channels

    val stdin: BufferedWriter =
      new BufferedWriter(new OutputStreamWriter(proc.getOutputStream, UTF8.charset))

    val stdout: BufferedReader =
      new BufferedReader(new InputStreamReader(proc.getInputStream, UTF8.charset))

    val stderr: BufferedReader =
      new BufferedReader(new InputStreamReader(proc.getErrorStream, UTF8.charset))


    // signals

    private val pid = stdout.readLine

    def interrupt()
    { Exn.Interrupt.postpone { Isabelle_System.kill("INT", pid) } }

    private def kill(signal: String): Boolean =
      Exn.Interrupt.postpone {
        Isabelle_System.kill(signal, pid)
        Isabelle_System.kill("0", pid)._2 == 0 } getOrElse true

    private def multi_kill(signal: String): Boolean =
    {
      var running = true
      var count = 10
      while (running && count > 0) {
        if (kill(signal)) {
          Exn.Interrupt.postpone {
            Thread.sleep(100)
            count -= 1
          }
        }
        else running = false
      }
      running
    }

    def terminate()
    {
      multi_kill("INT") && multi_kill("TERM") && kill("KILL")
      proc.destroy
      do_cleanup()
    }


    // JVM shutdown hook

    private val shutdown_hook = new Thread { override def run = terminate() }

    try { Runtime.getRuntime.addShutdownHook(shutdown_hook) }
    catch { case _: IllegalStateException => }


    // cleanup

    private def do_cleanup()
    {
      try { Runtime.getRuntime.removeShutdownHook(shutdown_hook) }
      catch { case _: IllegalStateException => }

      script_file.delete

      timing.change {
        case None =>
          if (timing_file.isFile) {
            val t =
              Word.explode(File.read(timing_file)) match {
                case List(Value.Long(elapsed), Value.Long(cpu)) =>
                  Timing(Time.ms(elapsed), Time.ms(cpu), Time.zero)
                case _ => Timing.zero
              }
            timing_file.delete
            Some(t)
          }
          else Some(Timing.zero)
        case some => some
      }

      cleanup()
    }


    // join

    def join: Int =
    {
      val rc = proc.waitFor
      do_cleanup()
      rc
    }


    // result

    def result(
      progress_stdout: String => Unit = (_: String) => (),
      progress_stderr: String => Unit = (_: String) => (),
      progress_limit: Option[Long] = None,
      strict: Boolean = true): Process_Result =
    {
      stdin.close

      val limited = new Limited_Progress(this, progress_limit)
      val out_lines =
        Future.thread("bash_stdout") { File.read_lines(stdout, limited(progress_stdout)) }
      val err_lines =
        Future.thread("bash_stderr") { File.read_lines(stderr, limited(progress_stderr)) }

      val rc =
        try { join }
        catch { case Exn.Interrupt() => terminate(); Exn.Interrupt.return_code }
      if (strict && rc == Exn.Interrupt.return_code) throw Exn.Interrupt()

      Process_Result(rc, out_lines.join, err_lines.join, false, timing.value getOrElse Timing.zero)
    }
  }
}
